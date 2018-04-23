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
            const edits = textChanges.ChangeTracker.with(context, t => doit(context.file, context.program.getTypeChecker(), statements, t));
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

    function doit(oldFile: SourceFile, newFileName: string, checker: TypeChecker, toMove: ReadonlyArray<Statement>, tracker: textChanges.ChangeTracker): void {
        //Create a new SourceFile to represent the new file.
        //Add the declarations to it. (Also update them to `export` if that is needed.)
        //And add whatever import statements are needed.
        const usage = getUses(oldFile, toMove, checker);

        //We are creating a new source file.
        const newFileStatements: Statement[] = [];





        tracker.createNewFile(newFileName, newFileStatements);



    }




    interface UsageInfo {
        readonly oldFileNeedExport: ReadonlyNodeSet;
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
        const oldFileNeedExport = new NodeSet();
        const newFileImports = new SymbolSet();

        for (const statement of toMove) {
            forEachReference(statement, checker, symbol => {
                //This should refer to an export of some module, or a top-level declaration in the current file. Or else ignore.
                const decl = find(symbol.declarations, d => isDeclarationStatement(d) && isSourceFile(d.parent)); //todo: ambient modules
                if (!decl) return;
                if (decl.parent === oldFile) oldFileNeedExport.add(decl);
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

        return { oldFileNeedExport, newFileImports, newFileNeedExport };
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

    interface ReadonlyNodeSet { has(node: Node): boolean; }
    //!
    class NodeSet implements ReadonlyNodeSet {
        private map = createMap<Node>(); // node id to node
        add(node: Node): void { this.map.set(String(getNodeId(node)), node); }
        has(node: Node): boolean { return this.map.has(String(getNodeId(node))); }
    }
    interface ReadonlySymbolSet { has(symbol: Symbol): boolean; }
    class SymbolSet implements ReadonlySymbolSet {
        private map = createMap<Symbol>();
        add(s: Symbol): void { this.map.set(String(getSymbolId(s)), s); }
        has(s: Symbol): boolean { return this.map.has(String(getSymbolId(s))); }
    }



}

