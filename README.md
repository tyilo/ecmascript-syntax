# ecmascript-syntax

Determine the ECMAScript version required to parse the syntax of a JavaScript file.

This doesn't check that the required runtime objects/functions/etc. are available.

## Example usage

See [`test.js`](test.js).

```
$ cargo run -- test.js
ES2015 required because of:
- Class declaration
- for (... of ...)
- Arrow function (() => {})
- Generator (function *)
- import statement
- export statement
- let binding
- const binding
- Template literal (`${1 + 1}`)
ES2016 required because of:
- Exponentiation (**, **=)
ES2017 required because of:
- Async function
- await expression
ES2018 required because of:
- Async generator (async function*)
- for await (... of ...)
- Spread operator (...)
- Rest parameters (...)
- RegExp literal flag /s
ES2019 required because of:
- try-catch without binding (try { ... } catch { ... })
ES2020 required because of:
- BigInt literal (1n)
- Optional chaining (?.)
- Nullish coalescing operator (??)
- Dynamic import (await import("..."))
ES2021 required because of:
- Nullish coalescing assignment (??=)
- Logical and assignment (&&=)
- Logical or assignment (||=)
ES2022 required because of:
- Field declaration in class
- Private field or method (#foo)
- Static class initializer (static { ... })
- Top-level await expression
- RegExp literal flag /d
ES2024 required because of:
- RegExp literal flag /v
```
