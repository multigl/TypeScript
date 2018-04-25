/// <reference path='fourslash.ts' />

// @Filename: /a.ts
////const x = y;
////[|const y = x;|]

verify.moveToNewFile({
    newFileContents: {
        "/a.ts":
`import { y } from "./newFile.ts";

export const x = y;
`,
        "/newFile.ts":
`import { x } from "./a.ts";
export const y = x;`,
    },
});
