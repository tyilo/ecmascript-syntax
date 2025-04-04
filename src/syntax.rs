use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Version {
    ES2015,
    ES2016,
    ES2017,
    ES2018,
    ES2019,
    ES2020,
    ES2021,
    ES2022,
    ES2024,
}

impl Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Version::ES2015 => write!(f, "ES2015"),
            Version::ES2016 => write!(f, "ES2016"),
            Version::ES2017 => write!(f, "ES2017"),
            Version::ES2018 => write!(f, "ES2018"),
            Version::ES2019 => write!(f, "ES2019"),
            Version::ES2020 => write!(f, "ES2020"),
            Version::ES2021 => write!(f, "ES2021"),
            Version::ES2022 => write!(f, "ES2022"),
            Version::ES2024 => write!(f, "ES2024"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Syntax {
    // ES2015
    Class,
    ForOfLoop,
    ArrowFunction,
    Generator,
    Import,
    Export,
    Let,
    Const,
    TemplateLiteral,

    // ES2016
    Exponentiation,

    // ES2017
    AsyncFn,
    Await,

    // ES2018
    AsyncGenerator,
    ForAwait,
    SpreadOperator,
    RestParameter,
    RegExpFlagS,

    // ES2019
    CatchWithoutBinding,

    // ES2020
    BigIntLiteral,
    OptionalChaining,
    NullishCoalescingOperator,
    DynamicImport,

    // ES2021
    NullishCoalescingAssignment,
    LogicalAndAssignment,
    LogicalOrAssignment,

    // boa_ast doesn't save enough information to determine if a number literal
    // contains an underscore
    #[allow(dead_code)]
    NumericSeparator,

    // ES2022
    ClassFieldDeclaration,
    PrivateField,
    StaticBlock,
    TopLevelAwait,
    RegExpFlagD,

    // ES2024
    RegExpFlagV,
}

impl Syntax {
    pub fn version_required(self) -> Version {
        match self {
            Syntax::Class
            | Syntax::ForOfLoop
            | Syntax::ArrowFunction
            | Syntax::Generator
            | Syntax::Import
            | Syntax::Export
            | Syntax::Let
            | Syntax::Const
            | Syntax::TemplateLiteral => Version::ES2015,
            Syntax::Exponentiation => Version::ES2016,
            Syntax::AsyncFn | Syntax::Await => Version::ES2017,
            Syntax::AsyncGenerator
            | Syntax::ForAwait
            | Syntax::SpreadOperator
            | Syntax::RestParameter
            | Syntax::RegExpFlagS => Version::ES2018,
            Syntax::CatchWithoutBinding => Version::ES2019,
            Syntax::BigIntLiteral
            | Syntax::OptionalChaining
            | Syntax::NullishCoalescingOperator
            | Syntax::DynamicImport => Version::ES2020,
            Syntax::NullishCoalescingAssignment
            | Syntax::LogicalAndAssignment
            | Syntax::LogicalOrAssignment
            | Syntax::NumericSeparator => Version::ES2021,
            Syntax::ClassFieldDeclaration
            | Syntax::PrivateField
            | Syntax::StaticBlock
            | Syntax::TopLevelAwait
            | Syntax::RegExpFlagD => Version::ES2022,
            Syntax::RegExpFlagV => Version::ES2024,
        }
    }
}

impl Display for Syntax {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Syntax::Class => write!(f, "Class declaration"),
            Syntax::ForOfLoop => write!(f, "for (... of ...)"),
            Syntax::ArrowFunction => write!(f, "Arrow function (() => {{}})"),
            Syntax::Generator => write!(f, "Generator (function *)"),
            Syntax::Import => write!(f, "import statement"),
            Syntax::Export => write!(f, "export statement"),
            Syntax::Let => write!(f, "let binding"),
            Syntax::Const => write!(f, "const binding"),
            Syntax::TemplateLiteral => write!(f, "Template literal (`${{1 + 1}}`)"),
            Syntax::Exponentiation => write!(f, "Exponentiation (**, **=)"),
            Syntax::AsyncFn => write!(f, "Async function"),
            Syntax::Await => write!(f, "await expression"),
            Syntax::AsyncGenerator => write!(f, "Async generator (async function*)"),
            Syntax::ForAwait => write!(f, "for await (... of ...)"),
            Syntax::SpreadOperator => write!(f, "Spread operator (...)"),
            Syntax::RestParameter => write!(f, "Rest parameters (...)"),
            Syntax::RegExpFlagS => write!(f, "RegExp literal flag /s"),
            Syntax::CatchWithoutBinding => write!(
                f,
                "try-catch without binding (try {{ ... }} catch {{ ... }})"
            ),
            Syntax::BigIntLiteral => write!(f, "BigInt literal (1n)"),
            Syntax::OptionalChaining => write!(f, "Optional chaining (?.)"),
            Syntax::NullishCoalescingOperator => write!(f, "Nullish coalescing operator (??)"),
            Syntax::DynamicImport => write!(f, "Dynamic import (await import(\"...\"))"),
            Syntax::NullishCoalescingAssignment => write!(f, "Nullish coalescing assignment (??=)"),
            Syntax::LogicalAndAssignment => write!(f, "Logical and assignment (&&=)"),
            Syntax::LogicalOrAssignment => write!(f, "Logical or assignment (||=)"),
            Syntax::NumericSeparator => write!(f, "Numeric separator (10_000)"),
            Syntax::ClassFieldDeclaration => write!(f, "Field declaration in class"),
            Syntax::PrivateField => write!(f, "Private field or method (#foo)"),
            Syntax::StaticBlock => write!(f, "Static class initializer (static {{ ... }})"),
            Syntax::TopLevelAwait => write!(f, "Top-level await expression"),
            Syntax::RegExpFlagD => write!(f, "RegExp literal flag /d"),
            Syntax::RegExpFlagV => write!(f, "RegExp literal flag /v"),
        }
    }
}
