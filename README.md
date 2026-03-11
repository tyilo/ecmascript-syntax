# ecmascript-syntax

Determine the ECMAScript version required to parse the syntax of a JavaScript file.

This doesn't check that the required runtime objects/functions/etc. are available.

## Example usage

See [`test.js`](test.js).

```
$ cargo run -- test.js
ES2015 required because of:
- Class declaration: (Supported since Chrome 49)
- for (... of ...): (Supported since Chrome 38)
- Arrow function (() => {}): (Supported since Chrome 45)
- Generator (function *): (Supported since Chrome 49)
- import statement: (Supported since Chrome 61)
- export statement: (Supported since Chrome 61)
- let binding: (Supported since Chrome 21)
- const binding: (Supported since Chrome 21)
- Template literal (`${1 + 1}`): (Supported since Chrome 41)
ES2016 required because of:
- Exponentiation (**, **=): (Supported since Chrome 52)
ES2017 required because of:
- Async function: (Supported since Chrome 55)
- await expression: (Supported since Chrome 55)
ES2018 required because of:
- Async generator (async function*): (Supported since Chrome 63)
- for await (... of ...): (Supported since Chrome 63)
- Spread operator (...): (Supported since Chrome 46)
- Rest parameters (...): (Supported since Chrome 47)
- RegExp literal flag /s
ES2019 required because of:
- try-catch without binding (try { ... } catch { ... }): (Supported since Chrome 66)
ES2020 required because of:
- BigInt literal (1n): (Supported since Chrome 67)
- Optional chaining (?.): (Supported since Chrome 80)
- Nullish coalescing operator (??): (Supported since Chrome 80)
- Dynamic import (await import("...")): (Supported since Chrome 63)
ES2021 required because of:
- Nullish coalescing assignment (??=): (Supported since Chrome 85)
- Logical and assignment (&&=): (Supported since Chrome 85)
- Logical or assignment (||=): (Supported since Chrome 85)
- Numeric separator (10_000): (Supported since Chrome 75)
ES2022 required because of:
- Field declaration in class: (Supported since Chrome 72)
- Private field or method (#foo): (Supported since Chrome 74)
- Static class initializer (static { ... }): (Supported since Chrome 94)
- Top-level await expression: (Supported since Chrome 89)
- RegExp literal flag /d
ES2024 required because of:
- RegExp literal flag /v
ES2025 required because of:
- RegExp inline modifier ((?i:...)): (Supported since Chrome 125)
```
