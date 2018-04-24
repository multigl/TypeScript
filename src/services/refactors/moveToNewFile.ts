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

        tracker.createNewFile(oldFile, newFileName, [...getNewFileImports(oldFile, usage.newFileImports), ...addExports(toMove, usage.newFileNeedExport)]);

        tracker.deleteNodeRange(oldFile, first(toMove), last(toMove));

        //Also, oldFile needs imports from newFile
        const x = oldFileImportsFromNewFile(usage.newFileNeedExport, newFileName); //name
        if (x) {
            //TODO: insert sorted
            tracker.insertNodeBefore(oldFile, oldFile.statements[0], x, /*blankLineBetween*/ true);
        }

        //TODO: also delete unused oldfile imports

        //Also update all uses of things from oldFile to point to newFile
        for (const _file of program.getSourceFiles()) {
            //TODO: Get imports from oldFile and move tem
        }
    }

    function oldFileImportsFromNewFile(newFileNeedExport: ReadonlySymbolSet, newFileName: string): ImportDeclaration | undefined {
        //todo: default imports
        const imports: ImportSpecifier[] = [];
        newFileNeedExport.forEach(symbol => {
            imports.push(createImportSpecifier(undefined, createIdentifier(symbol.name)));
        });
        return imports.length
            ? createImportDeclaration(undefined, undefined, createImportClause(undefined, createNamedImports(imports)), createLiteral(getBaseFileName(newFileName)))
            : undefined;
    }

    function addExports(toMove: ReadonlyArray<Statement>, needExport: ReadonlySymbolSet): ReadonlyArray<Statement> {

        return toMove.map(s2 => {
            const clone = ts.getSynthesizedDeepClone(s2);
            return isDeclarationStatement(clone) && needExport.has(clone.symbol) && !hasModifier(clone, ModifierFlags.Export)
                ? addExport(clone as DeclarationStatementLike)
                : clone;
        });
    }

    type DeclarationStatementLike = FunctionDeclaration | ClassDeclaration; //todo:more

    function addExport(d: DeclarationStatementLike): DeclarationStatement {
        const modifiers = [...d.modifiers, createModifier(SyntaxKind.ExportKeyword)];
        switch (d.kind) {
            case SyntaxKind.FunctionDeclaration:
                return updateFunctionDeclaration(d, d.decorators, modifiers, d.asteriskToken, d.name, d.typeParameters, d.parameters, d.type, d.body);

            case SyntaxKind.ClassDeclaration:
                return updateClassDeclaration(d, d.decorators, modifiers, d.name, d.typeParameters, d.heritageClauses, d.members);

            default:
                Debug.assertNever(d);
        }
    }

    function getNewFileImports(oldFile: SourceFile, newFileImports: ReadonlySymbolSet): ReadonlyArray<ImportDeclaration> {
        //Just copy the old file's imports!
        const a = mapDefined(oldFile.statements, oldStatement => {
            if (!isImportDeclaration(oldStatement)) return;

            const oldImportClause = oldStatement.importClause;
            const newDefaultImport = newFileImports.has(oldImportClause.name.symbol) ? oldImportClause.name : undefined;
            //const newNamedBindings = ...
            let newNamedBindings: NamedImportBindings;
            if (oldImportClause.namedBindings.kind === SyntaxKind.NamespaceImport) {
                newNamedBindings = newFileImports.has(oldImportClause.namedBindings.name.symbol) ? oldImportClause.namedBindings : undefined;
            } else {
                const newElements = oldImportClause.namedBindings.elements.filter(e => newFileImports.has(e.name.symbol));
                newNamedBindings = newElements.length === 0 ? undefined : createNamedImports(newElements);
            }
            const newImportClause = updateImportClause(oldImportClause, newDefaultImport, newNamedBindings);
            return newDefaultImport || newNamedBindings
                ? updateImportDeclaration(oldStatement, oldStatement.decorators, oldStatement.modifiers, newImportClause, oldStatement.moduleSpecifier)
                : undefined;
        });

        //Also, any newFileImports that point to *declarations* in oldFile need imports.
        let oldFileDefault: Identifier | undefined;
        const oldFileNamedImports: ImportSpecifier[] = [];
        newFileImports.forEach(symbol => {
            for (const decl of symbol.declarations) {
                if (!isDeclarationStatement(decl)) continue;

                //TODO: ensure it's exported.
                const name = cast(decl.name, isIdentifier);
                if (hasModifier(decl, ModifierFlags.Default)) {
                    oldFileDefault = name;
                } else {
                    oldFileNamedImports.push(createImportSpecifier(undefined, name));
                }
            }
        });

        const x = oldFileDefault || oldFileNamedImports.length //name
            ? [createImportDeclaration(undefined, undefined,
                createImportClause(oldFileDefault, oldFileNamedImports.length ? createNamedImports(oldFileNamedImports) : undefined),
                createLiteral(`./${getBaseFileName(oldFile.fileName)}`))]
            : emptyArray;
        return [...a, ...x];
    }




    interface UsageInfo {
        //readonly oldFileNeedExport: ReadonlyNodeSet; handled ...
        // This will be either an export from the old file, or an import from the old file.
        readonly newFileImports: ReadonlySymbolSet;
        // Declarations in the new file that if not already exported, need to be, because the old file must import them.
        readonly newFileNeedExport: ReadonlySymbolSet;
    }
    //get usage info:
    //* Some things in the old file must now be exported.
    //* Some things that weren't exported before will have to be now.
    //* The moved statements will need imports, of course.
    //TODO: * Some imports in the old file will now be unused.
    function getUses(oldFile: SourceFile, toMove: ReadonlyArray<Statement>, checker: TypeChecker): UsageInfo {
        //* iterate over 'statements' and collect all used symbols.
        //const oldFileNeedExport = new NodeSet();
        const newFileImports = new SymbolSet();

        for (const statement of toMove) {
            forEachReference(statement, checker, symbol => {
                //This should refer to an export of some module, or a top-level declaration in the current file. Or else ignore.
                const decl = find(symbol.declarations, d => isDeclarationStatement(d) && isSourceFile(d.parent)); //todo: ambient modules
                if (!decl) return;
                //if (decl.parent === oldFile) oldFileNeedExport.add(decl);
                newFileImports.add(symbol);
            });
        }

        const symbolsToMove = new SymbolSet();
        for (const s of toMove) {
            if (!isDeclaration(s)) continue;
            const sym = checker.getSymbolAtLocation(s.name);
            symbolsToMove.add(sym);
        }

        const newFileNeedExport = new SymbolSet();

        for (const statement of oldFile.statements) {
            if (contains(toMove, statement)) continue;
            forEachReference(statement, checker, symbol => {
                if (symbolsToMove.has(symbol)) newFileNeedExport.add(symbol);
            });
        }

        return { newFileImports, newFileNeedExport };
    }

    function forEachReference(node: Node, checker: TypeChecker, onReference: (s: Symbol) => void) {
        node.forEachChild(function cb(node) {
            if (isIdentifier(node)) {
                const sym = checker.getSymbolAtLocation(node);
                if (sym) onReference(skipAlias(sym, checker));
            } else {
                node.forEachChild(cb);
            }
        });
    }

    //interface ReadonlyNodeSet { has(node: Node): boolean; }
    //!
    /*class NodeSet implements ReadonlyNodeSet {
        private map = createMap<Node>(); // node id to node
        add(node: Node): void { this.map.set(String(getNodeId(node)), node); }
        has(node: Node): boolean { return this.map.has(String(getNodeId(node))); }
    }*/
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



}

