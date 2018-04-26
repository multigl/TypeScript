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
            const edits = textChanges.ChangeTracker.with(context, t => doit(context.file, context.program, statements, t, context.host));
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

    function makeUniqueModuleName(moduleName: string, extension: string, inDirectory: string, host: LanguageServiceHost): string {
        while (true) {
            const name = combinePaths(inDirectory, moduleName + extension);
            if (!host.fileExists(name)) return moduleName;
            moduleName += "0";
        }
    }

    function getNewModuleName(movedSymbols: ReadonlySymbolSet): string {
        if (movedSymbols.size === 1) {
            let name!: string;
            movedSymbols.forEach(s => { name = s.name; });
            return name;
        }
        return "newFile";
    }

    function doit(oldFile: SourceFile, program: Program, toMove: ReadonlyArray<Statement>, changes: textChanges.ChangeTracker, host: LanguageServiceHost): void {
        const checker = program.getTypeChecker();
        const usage = getUses(oldFile, toMove, checker);

        const currentDirectory = getDirectoryPath(oldFile.fileName);
        const extension = extensionFromPath(oldFile.fileName);
        const newModuleName = makeUniqueModuleName(getNewModuleName(usage.movedSymbols), extension, currentDirectory, host);
        const newFileNameWithExtension = newModuleName + extension;

        const importsFromNewFile = createOldFileImportsFromNewFile(usage.oldFileImportsFromNewFile, newModuleName);
        if (importsFromNewFile) {
            changes.insertNodeBefore(oldFile, oldFile.statements[0], importsFromNewFile, /*blankLineBetween*/ true);
        }

        changes.deleteNodeRange(oldFile, first(toMove), last(toMove));

        changes.createNewFile(oldFile, combinePaths(currentDirectory, newFileNameWithExtension), [
            ...getNewFileImports(oldFile, usage.copyImports, usage.newFileImportsFromOldFile, changes, checker), //also adds 'export' in old file
            ...addExports(toMove, usage.oldFileImportsFromNewFile),
        ]);

        updateImportsInOtherFiles(changes, program, oldFile, usage.movedSymbols, newFileNameWithExtension);
    }

    function updateImportsInOtherFiles(changes: textChanges.ChangeTracker, program: Program, oldFile: SourceFile, movedSymbols: ReadonlySymbolSet, newFileNameWithExtension: string): void {
        //Also update all uses of things from oldFile to point to newFile
        const checker = program.getTypeChecker();
        for (const sourceFile of program.getSourceFiles()) {
            if (sourceFile === oldFile) continue;
            //If some import came from usage.movedSymbols, move it
            //TODO: Get imports from oldFile and move tem
            for (const statement of sourceFile.statements) {
                if (!isImportDeclaration(statement) || !isStringLiteral(statement.moduleSpecifier)) continue;

                const shouldMove = (name: Identifier): boolean => movedSymbols.has(skipAlias(checker.getSymbolAtLocation(name), checker)) //todo: skip *one* alias (see findallrefs)
                deleteUnusedImports(sourceFile, statement, changes, shouldMove);
                const newModuleSpecifier = combinePaths(getDirectoryPath(statement.moduleSpecifier.text), newFileNameWithExtension);
                const newImportDeclaration = filterImport(statement, createLiteral(newModuleSpecifier), shouldMove);
                if (newImportDeclaration) changes.insertNodeAfter(sourceFile, statement, newImportDeclaration);
            }
        }
    }

    function createOldFileImportsFromNewFile(newFileNeedExport: ReadonlySymbolSet, newFileNameWithExtension: string): ImportDeclaration | undefined {
        //todo: default imports
        const imports: ImportSpecifier[] = [];
        newFileNeedExport.forEach(symbol => {
            imports.push(createImportSpecifier(undefined, createIdentifier(symbol.name)));
        });
        return makeImportIfNecessary(undefined, imports, ensurePathIsRelative(newFileNameWithExtension));
    }

    function addExports(toMove: ReadonlyArray<Statement>, needExport: ReadonlySymbolSet): ReadonlyArray<Statement> {
        return toMove.map(statement => {
            const clone = ts.getSynthesizedDeepClone(statement);
            return !hasModifier(statement, ModifierFlags.Export) && forEachTopLevelDeclaration(statement, d => needExport.has(Debug.assertDefined(d.symbol)))
                ? addExport(clone as TopLevelDeclarationStatement)
                : clone;
        });
    }

    //Actually, I only want 2 functions:
    //1) take textChanges and remove unused imports
    //2) filter out and get a new import

    function deleteUnusedImports(sourceFile: SourceFile, i: ImportDeclaration, changes: textChanges.ChangeTracker, isUnused: (name: Identifier) => boolean): void {
        if (!i.importClause) return;
        const { name, namedBindings } = i.importClause;
        const defaultUnused = !name || isUnused(name);
        const namedBindingsUnused = !namedBindings ||
            (namedBindings.kind === SyntaxKind.NamespaceImport ? isUnused(namedBindings.name) : namedBindings.elements.every(e => isUnused(e.name)));
        if (defaultUnused && namedBindingsUnused) {
            changes.deleteNode(sourceFile, i);
        }
        else {
            if (name && defaultUnused) {
                changes.deleteNode(sourceFile, name);
            }
            if (namedBindings) {
                if (namedBindingsUnused) {
                    changes.deleteNode(sourceFile, namedBindings);
                }
                else if (namedBindings.kind === SyntaxKind.NamedImports) {
                    for (const element of namedBindings.elements) {
                        if (isUnused(element.name)) changes.deleteNodeInList(sourceFile, element);
                    }
                }
            }
        }
    }

    function filterImport(i: ImportDeclaration, moduleSpecifier: Expression, keep: (name: Identifier) => boolean): ImportDeclaration | undefined {
        const clause = i.importClause;
        const defaultImport = clause.name && keep(clause.name) ? clause.name : undefined;
        const namedBindings = filterNamedBindings(clause.namedBindings, keep);
        return defaultImport || namedBindings ? createImportDeclaration(undefined, undefined, createImportClause(defaultImport, namedBindings), moduleSpecifier) : undefined;
    }
    function filterNamedBindings(namedBindings: NamedImportBindings, keep: (name: Identifier) => boolean): NamedImportBindings | undefined {
        if (namedBindings.kind === SyntaxKind.NamespaceImport) {
            return keep(namedBindings.name) ? namedBindings : undefined;
        }
        else {
            const x = namedBindings.elements.filter(e => keep(e.name));//name
            return x.length ? createNamedImports(x) : undefined;
        }
    }

    function getNewFileImports(
        oldFile: SourceFile,
        copyImports: ReadonlySymbolSet,
        newFileImportsFromOldFile: ReadonlySymbolSet,
        changes: textChanges.ChangeTracker,
        checker: TypeChecker,
    ): ReadonlyArray<ImportDeclaration> {
        const copiedOldImports = mapDefined(oldFile.statements, oldStatement => {
            if (!isImportDeclaration(oldStatement)) return undefined;
            return filterImport(oldStatement, oldStatement.moduleSpecifier, name => copyImports.has(checker.getSymbolAtLocation(name)));
            //kill
            /*const oldImportClause = oldStatement.importClause;
            const newDefaultImport = oldImportClause.name && copyImports.has(checker.getSymbolAtLocation(oldImportClause.name)) ? oldImportClause.name : undefined;
            const newNamedBindings = splitNamedBindings(oldImportClause.namedBindings, copyImports, checker);
            return newDefaultImport || newNamedBindings
                ? updateImportDeclaration(oldStatement, oldStatement.decorators, oldStatement.modifiers, updateImportClause(oldImportClause, newDefaultImport, newNamedBindings), oldStatement.moduleSpecifier)
                : undefined;*/
        });

        //Also, any newFileImports that point to *declarations* in oldFile need imports.
        let oldFileDefault: Identifier | undefined;
        let oldFileNamedImports: ImportSpecifier[] | undefined;
        newFileImportsFromOldFile.forEach(symbol => {
            for (const decl of symbol.declarations) {
                if (!isTopLevelDeclaration(decl) || !isIdentifier(decl.name)) continue;
                const top = getTopLevelDeclarationStatement(decl);
                if (!hasModifier(top, ModifierFlags.Export)) {
                    changes.insertExportModifier(oldFile, top); //todo: only do this once in case `const x = 0, y = 1` both used
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

        const oldFileImport = makeImportIfNecessary(oldFileDefault, oldFileNamedImports, `./${getBaseFileName(oldFile.fileName)}`);
        return [...copiedOldImports, ...(oldFileImport ? [oldFileImport] : emptyArray)];
    }

    interface UsageInfo {
        //doc
        readonly copyImports: ReadonlySymbolSet;
        // Symbols that must be imported in the new file.
        // Note: Some of these symbols will be in the old file -- those will need to be exported from the old file if not already.
        readonly newFileImportsFromOldFile: ReadonlySymbolSet;
        // Declarations in the new file that if not already exported, need to be, because the old file must import them.
        readonly oldFileImportsFromNewFile: ReadonlySymbolSet;
        //doc
        readonly movedSymbols: ReadonlySymbolSet;
    }
    function getUses(oldFile: SourceFile, toMove: ReadonlyArray<Statement>, checker: TypeChecker): UsageInfo {
        //Map a module declaration to a set of names
        const copyImports = new SymbolSet();
        const newFileImportsFromOldFile = new SymbolSet();
        const movedSymbols = new SymbolSet();

        for (const statement of toMove) {
            forEachTopLevelDeclaration(statement, decl => {
                movedSymbols.add(Debug.assertDefined(decl.symbol));
            });

            forEachReference(statement, checker, symbol => {
                if (!symbol.declarations) return;
                for (const decl of symbol.declarations) {
                    if (isImportSpecifier(decl) || isImportClause(decl.parent)) {
                        copyImports.add(symbol);
                    }
                    else if (isTopLevelDeclaration(decl)) {
                        newFileImportsFromOldFile.add(symbol);
                    }
                }
            });
        }

        const oldFileImportsFromNewFile = new SymbolSet();
        for (const statement of oldFile.statements) {
            if (contains(toMove, statement)) continue;

            forEachReference(statement, checker, symbol => {
                if (movedSymbols.has(symbol)) oldFileImportsFromNewFile.add(symbol);
            });
        }

        return { copyImports, newFileImportsFromOldFile, oldFileImportsFromNewFile, movedSymbols };
    }

    ////////////
    // UTILITIES
    ////////////

    function forEachReference(node: Node, checker: TypeChecker, onReference: (s: Symbol) => void) {
        node.forEachChild(function cb(node) {
            if (isIdentifier(node) && !isDeclarationName(node)) {
                //const sym = checker.resolveName(node.text, node, SymbolFlags.All, /*excludeGlobals*/ true);
                const sym = checker.getSymbolAtLocation(node);
                if (sym) onReference(sym); //skipAlias(sym, checker)
            } else {
                node.forEachChild(cb);
            }
        });
    }

    interface ReadonlySymbolSet {
        readonly size: number;
        has(symbol: Symbol): boolean;
        forEach(cb: (symbol: Symbol) => void): void;
    }
    class SymbolSet implements ReadonlySymbolSet {
        private map = createMap<Symbol>();
        get size() { return this.map.size; }
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
