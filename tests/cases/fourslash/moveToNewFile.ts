/// <reference path='fourslash.ts' />

// @Filename: /a.ts
////import { a, b } from "./other";
////const x = a;
////[|const y = x + b;|]
////const z = y;

verify.moveToNewFile({
    newFileContents: {
        "/a.ts":
`import { y } from "./y";

import { a, b } from "./other";
export const x = a;
const z = y;`,

        "/y.ts":
`import { b } from "./other";
import { x } from "./a.ts";
export const y = x + b;`,
    },
});
