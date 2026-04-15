# ecmascript-syntax

Determine the ECMAScript version required to parse the syntax of a JavaScript file.

This doesn't check that the required runtime objects/functions/etc. are available.

## Example usage

See [`test.js`](test.js).

```
$ cargo run -- test.js --show-location
ES2015 required because of:
- Class declaration: (Supported since Chrome 49)
    first encountered at test.js:5:7
- for (... of ...): (Supported since Chrome 38)
    first encountered at test.js:46:0
- Arrow function (() => {}): (Supported since Chrome 45)
    first encountered at test.js:15:11
- Generator (function *): (Supported since Chrome 49)
    first encountered at test.js:16:0
- import statement: (Supported since Chrome 61)
    first encountered at test.js:1:0
- export statement: (Supported since Chrome 61)
    first encountered at test.js:5:0
- let binding: (Supported since Chrome 21)
    first encountered at test.js:41:0
- const binding: (Supported since Chrome 21)
    first encountered at test.js:3:0
- Template literal (`${1 + 1}`): (Supported since Chrome 41)
    first encountered at test.js:27:10
ES2016 required because of:
- Exponentiation (**, **=): (Supported since Chrome 52)
    first encountered at test.js:28:10
ES2017 required because of:
- Async function: (Supported since Chrome 55)
    first encountered at test.js:19:0
- await expression: (Supported since Chrome 55)
    first encountered at test.js:20:4
ES2018 required because of:
- Async generator (async function*): (Supported since Chrome 63)
    first encountered at test.js:22:0
- for await (... of ...): (Supported since Chrome 63)
    first encountered at test.js:49:0
- Spread operator (...): (Supported since Chrome 46)
    first encountered at test.js:33:11
- Rest parameters (...): (Supported since Chrome 47)
    first encountered at test.js:24:12
- RegExp literal flag /s
    first encountered at test.js:34:11
ES2019 required because of:
- try-catch without binding (try { ... } catch { ... }): (Supported since Chrome 66)
    first encountered at test.js:53:2
ES2020 required because of:
- BigInt literal (1n): (Supported since Chrome 67)
    first encountered at test.js:29:10
- Optional chaining (?.): (Supported since Chrome 80)
    first encountered at test.js:39:10
- Nullish coalescing operator (??): (Supported since Chrome 80)
    first encountered at test.js:39:10
- Dynamic import (await import("...")): (Supported since Chrome 63)
    first encountered at test.js:3:17
ES2021 required because of:
- Nullish coalescing assignment (??=): (Supported since Chrome 85)
    first encountered at test.js:42:0
- Logical and assignment (&&=): (Supported since Chrome 85)
    first encountered at test.js:43:0
- Logical or assignment (||=): (Supported since Chrome 85)
    first encountered at test.js:44:0
- Numeric separator (10_000): (Supported since Chrome 75)
    first encountered at test.js:30:13
ES2022 required because of:
- Field declaration in class: (Supported since Chrome 72)
    first encountered at test.js:6:4
- Private field or method (#foo): (Supported since Chrome 74)
    first encountered at test.js:6:4
- Static class initializer (static { ... }): (Supported since Chrome 94)
    first encountered at test.js:10:4
- Top-level await expression: (Supported since Chrome 89)
    first encountered at test.js:3:11
- RegExp literal flag /d
    first encountered at test.js:35:11
ES2024 required because of:
- RegExp literal flag /v
    first encountered at test.js:36:11
ES2025 required because of:
- RegExp inline modifier ((?i:...)): (Supported since Chrome 125)
    first encountered at test.js:37:11

Minimum Chrome version required: 125
```
