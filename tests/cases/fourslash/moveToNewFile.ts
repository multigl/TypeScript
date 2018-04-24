/// <reference path='fourslash.ts' />

// @Filename: /a.ts
////const x = 0;
////[|const y = 1;|]

verify.moveToNewFile({
    newFileContents: {
        "/a.ts": "const x = 0;\n",
        "/newFile.ts": "const y = 1;",
    },
});
