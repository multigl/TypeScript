namespace ts.refactor {
    const refactorName = "Move to new file";
    registerRefactor(refactorName, {
        getAvailableActions(context): ApplicableRefactorInfo[] {
            if (getStatements(context) === undefined) {
                return undefined;
            }
            //We can provide this refactoring for any set of declarations that are on the top-level.
            const description = getLocaleSpecificMessage(Diagnostics.Move_to_new_file);
            return [{ name: refactorName, description, actions: [{ name: refactorName, description }] }];
        },
        getEditsForAction(context, actionName): RefactorEditInfo {
            Debug.assert(actionName === refactorName);
            const statements = Debug.assertDefined(getStatements(context));
            //todo: more smarter
            const newFileName = combinePaths(getDirectoryPath(context.file.fileName), "newFile.ts");
            const edits = textChanges.ChangeTracker.with(context, t => doit(context.file, newFileName, context.program, statements, t));
            return { edits, renameFilename: undefined, renameLocation: undefined };
        }
    });

    // Span must fully contain some set of top-level declarations.
    //todo: tests for this
    function getStatements(context: RefactorContext): ReadonlyArray<Statement> | undefined {
        const { file } = context;
        const range = createTextRangeFromSpan(getRefactorContextSpan(context));
        const { statements } = file;
        if (statements.length === 0) return undefined;

        const startNodeIndex = findIndex(statements, s => s.end > range.pos);
        const afterEndNodeIndex = findLastIndex(statements, s => s.end > range.end);
        if (afterEndNodeIndex === 0) return undefined;
        // Can't only partially include the start node
        if (range.pos > statements[startNodeIndex].getStart(file)) return undefined;
        // Can't be partially into the next node
        if (afterEndNodeIndex !== -1 && statements[afterEndNodeIndex].getStart(file) < range.pos) return undefined;

        return statements.slice(startNodeIndex, afterEndNodeIndex === -1 ? statements.length : afterEndNodeIndex);
    }

    function doit(oldFile: SourceFile, newFileName: string, program: Program, toMove: ReadonlyArray<Statement>, tracker: textChanges.ChangeTracker): void {
        const checker = program.getTypeChecker();
        //Create a new SourceFile to represent the new file.
        //Add the declarations to it. (Also update them to `export` if that is needed.)
        //And add whatever import statements are needed.
        const usage = getUses(oldFile, toMove, checker);

        tracker.deleteNodeRange(oldFile, first(toMove), last(toMove));

        //Also, oldFile needs imports from newFile
        const x = createOldFileImportsFromNewFile(usage.oldFileImportsFromNewFile, newFileName); //name
        if (x) {
            //TODO: insert sorted
            tracker.insertNodeBefore(oldFile, oldFile.statements[0], x, /*blankLineBetween*/ true);
        }

        tracker.createNewFile(oldFile, newFileName, [...getNewFileImports(oldFile, usage.newFileImports, tracker), ...addExports(toMove, usage.oldFileImportsFromNewFile)]);

        //TODO: also delete unused oldfile imports

        //Also update all uses of things from oldFile to point to newFile
        for (const _file of program.getSourceFiles()) {
            //TODO: Get imports from oldFile and move tem
        }
    }

    function createOldFileImportsFromNewFile(newFileNeedExport: ReadonlySymbolSet, newFileName: string): ImportDeclaration | undefined {
        //todo: default imports
        const imports: ImportSpecifier[] = [];
        newFileNeedExport.forEach(symbol => {
            imports.push(createImportSpecifier(undefined, createIdentifier(symbol.name)));
        });
        return imports.length ? createBasicImport(undefined, imports, ensurePathIsRelative(getBaseFileName(newFileName))) : undefined;
    }

    //name
    function createBasicImport(defaultImport: Identifier, namedImports: ReadonlyArray<ImportSpecifier>, moduleSpecifier: string) {
        return createImportDeclaration(undefined, undefined, createImportClause(defaultImport, createNamedImports(namedImports)), createLiteral(moduleSpecifier));
    }

    function addExports(toMove: ReadonlyArray<Statement>, needExport: ReadonlySymbolSet): ReadonlyArray<Statement> {
        return toMove.map(statement => {
            const clone = ts.getSynthesizedDeepClone(statement);
            return !hasModifier(statement, ModifierFlags.Export) && forEachTopLevelDeclaration(statement, d => needExport.has(Debug.assertDefined(d.symbol)))
                ? addExport(clone as TopLevelDeclarationStatement)
                : clone;
        });
    }

    function getNewFileImports(oldFile: SourceFile, newFileImports: ReadonlySymbolSet, tracker: textChanges.ChangeTracker): ReadonlyArray<ImportDeclaration> {
        const copiedOldImports = mapDefined(oldFile.statements, oldStatement => {
            if (!isImportDeclaration(oldStatement)) return undefined;
            const oldImportClause = oldStatement.importClause;
            const newDefaultImport = newFileImports.has(oldImportClause.name.symbol) ? oldImportClause.name : undefined;
            const newNamedBindings = filterNamedBindings(oldImportClause.namedBindings, newFileImports);
            return newDefaultImport || newNamedBindings
                ? updateImportDeclaration(oldStatement, oldStatement.decorators, oldStatement.modifiers, updateImportClause(oldImportClause, newDefaultImport, newNamedBindings), oldStatement.moduleSpecifier)
                : undefined;
        });

        //Also, any newFileImports that point to *declarations* in oldFile need imports.
        let oldFileDefault: Identifier | undefined;
        let oldFileNamedImports: ImportSpecifier[] | undefined;
        newFileImports.forEach(symbol => {
            for (const decl of symbol.declarations) {
                if (!isTopLevelDeclaration(decl) || !isIdentifier(decl.name)) continue;
                const top = getTopLevelDeclarationStatement(decl);
                if (!hasModifier(top, ModifierFlags.Export)) {
                    tracker.insertExportModifier(oldFile, top); //todo: only do this once in case `const x = 0, y = 1` both used
                }

                //TODO: also add 'export' to old file if not already present
                if (hasModifier(decl, ModifierFlags.Default)) {
                    oldFileDefault = decl.name;
                }
                else {
                    oldFileNamedImports = append(oldFileNamedImports, createImportSpecifier(undefined, decl.name));
                }
            }
        });

        const oldFileImport = oldFileDefault || oldFileNamedImports ? [createBasicImport(oldFileDefault, oldFileNamedImports, `./${getBaseFileName(oldFile.fileName)}`)] : emptyArray;
        return [...copiedOldImports, ...oldFileImport];
    }
    function filterNamedBindings(namedBindings: NamedImportBindings, newFileImports: ReadonlySymbolSet): NamedImportBindings {
        if (namedBindings.kind === SyntaxKind.NamespaceImport) {
            return newFileImports.has(namedBindings.name.symbol) ? namedBindings : undefined;
        }
        else {
            const newElements = namedBindings.elements.filter(e => newFileImports.has(e.name.symbol));
            return newElements.length === 0 ? undefined : createNamedImports(newElements);
        }
    }

    interface UsageInfo {
        // Symbols that must be imported in the new file.
        // Note: Some of these symbols will be in the old file -- those will need to be exported from the old file if not already.
        readonly newFileImports: ReadonlySymbolSet;
        // Declarations in the new file that if not already exported, need to be, because the old file must import them.
        readonly oldFileImportsFromNewFile: ReadonlySymbolSet;
    }
    //get usage info:
    //* Some things in the old file must now be exported.
    //* Some things that weren't exported before will have to be now.
    //* The moved statements will need imports, of course.
    //TODO: * Some imports in the old file will now be unused.
    function getUses(oldFile: SourceFile, toMove: ReadonlyArray<Statement>, checker: TypeChecker): UsageInfo {
        const newFileImports = new SymbolSet();
        const symbolsToMove = new SymbolSet();

        for (const statement of toMove) {
            forEachTopLevelDeclaration(statement, decl => {
                symbolsToMove.add(Debug.assertDefined(decl.symbol));
            });

            forEachReference(statement, checker, symbol => {
                if (symbol.declarations.some(isTopLevelDeclaration)) newFileImports.add(symbol);
            });
        }

        const oldFileImportsFromNewFile = new SymbolSet();
        for (const statement of oldFile.statements) {
            if (contains(toMove, statement)) continue;

            forEachReference(statement, checker, symbol => {
                if (symbolsToMove.has(symbol)) oldFileImportsFromNewFile.add(symbol);
            });
        }

        return { newFileImports, oldFileImportsFromNewFile };
    }



    ////////////
    // UTILITIES
    ////////////

    function forEachReference(node: Node, checker: TypeChecker, onReference: (s: Symbol) => void) {
        node.forEachChild(function cb(node) {
            if (isIdentifier(node) && !isDeclarationName(node)) {
                const sym = checker.getSymbolAtLocation(node);
                if (sym) onReference(skipAlias(sym, checker));
            } else {
                node.forEachChild(cb);
            }
        });
    }

    interface ReadonlySymbolSet {
        has(symbol: Symbol): boolean;
        forEach(cb: (symbol: Symbol) => void): void;
    }
    class SymbolSet implements ReadonlySymbolSet {
        private map = createMap<Symbol>();
        add(s: Symbol): void { this.map.set(String(getSymbolId(s)), s); }
        has(s: Symbol): boolean { return this.map.has(String(getSymbolId(s))); }
        forEach(cb: (symbol: Symbol) => void): void { this.map.forEach(cb); }
    }

    type TopLevelDeclarationStatement = FunctionDeclaration | ClassDeclaration | VariableStatement;
    interface TopLevelVariableDeclaration extends VariableDeclaration { parent: VariableDeclarationList & { parent: VariableStatement } };
    type TopLevelDeclaration = FunctionDeclaration | ClassDeclaration | TopLevelVariableDeclaration; //todo:more
    function isTopLevelDeclaration(node: Node): node is TopLevelDeclaration {
        switch (node.kind) {
            case SyntaxKind.FunctionDeclaration:
            case SyntaxKind.ClassDeclaration:
                return isSourceFile(node.parent);
            case SyntaxKind.VariableDeclaration:
                return isSourceFile((node as VariableDeclaration).parent.parent.parent);
        }
    }

    function forEachTopLevelDeclaration<T>(statement: Statement, cb: (t: TopLevelDeclaration) => T): T {
        switch (statement.kind) {
            case SyntaxKind.FunctionDeclaration:
            case SyntaxKind.ClassDeclaration:
                return cb(statement as FunctionDeclaration | ClassDeclaration);

            case SyntaxKind.VariableStatement:
                return forEach((statement as VariableStatement).declarationList.declarations as ReadonlyArray<TopLevelVariableDeclaration>, cb);

            //todo:more
        }
    }

    function getTopLevelDeclarationStatement(d: TopLevelDeclaration): TopLevelDeclarationStatement {
        switch (d.kind) {
            case SyntaxKind.VariableDeclaration:
                return d.parent.parent;
            default:
                return d;
        }
    }

    function addExport(d: TopLevelDeclarationStatement): TopLevelDeclarationStatement {
        const modifiers = concatenate([createModifier(SyntaxKind.ExportKeyword)], d.modifiers);
        switch (d.kind) {
            case SyntaxKind.FunctionDeclaration:
                return updateFunctionDeclaration(d, d.decorators, modifiers, d.asteriskToken, d.name, d.typeParameters, d.parameters, d.type, d.body);
            case SyntaxKind.ClassDeclaration:
                return updateClassDeclaration(d, d.decorators, modifiers, d.name, d.typeParameters, d.heritageClauses, d.members);
            case SyntaxKind.VariableStatement:
                return updateVariableStatement(d, modifiers, d.declarationList);
            default:
                Debug.assertNever(d);
        }
    }
}
