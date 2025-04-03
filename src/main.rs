use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
    ops::ControlFlow,
    path::PathBuf,
};

use boa_ast::{
    declaration::LexicalDeclaration,
    expression::{
        literal::{Literal, PropertyDefinition},
        operator::{
            assign::AssignOp,
            binary::{ArithmeticOp, BinaryOp, LogicalOp},
        },
    },
    function::ClassElement,
    scope::Scope,
    visitor::{VisitWith, Visitor},
};
use boa_interner::Interner;
use boa_parser::{Parser, Source};
use clap::Parser as ClapParser;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Version {
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
enum Syntax {
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
    fn version_required(self) -> Version {
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

#[derive(Debug)]
struct SyntaxVersionVisitor<'a> {
    interner: &'a Interner,
    syntax_found: BTreeSet<Syntax>,
    inside_fn: bool,
}

impl<'a> SyntaxVersionVisitor<'a> {
    fn new(interner: &'a Interner) -> Self {
        Self {
            interner,
            syntax_found: BTreeSet::new(),
            inside_fn: false,
        }
    }
}

impl<'a> Visitor<'a> for SyntaxVersionVisitor<'a> {
    type BreakTy = ();

    fn visit_lexical_declaration(
        &mut self,
        node: &'a LexicalDeclaration,
    ) -> ControlFlow<Self::BreakTy> {
        let syntax = match node {
            LexicalDeclaration::Const(_) => Syntax::Const,
            LexicalDeclaration::Let(_) => Syntax::Let,
        };
        self.syntax_found.insert(syntax);
        node.visit_with(self)
    }

    fn visit_import_declaration(
        &mut self,
        node: &'a boa_ast::declaration::ImportDeclaration,
    ) -> ControlFlow<Self::BreakTy> {
        self.syntax_found.insert(Syntax::Import);
        node.visit_with(self)
    }

    fn visit_import_call(
        &mut self,
        node: &'a boa_ast::expression::ImportCall,
    ) -> ControlFlow<Self::BreakTy> {
        self.syntax_found.insert(Syntax::DynamicImport);
        node.visit_with(self)
    }

    fn visit_export_declaration(
        &mut self,
        node: &'a boa_ast::declaration::ExportDeclaration,
    ) -> ControlFlow<Self::BreakTy> {
        self.syntax_found.insert(Syntax::Export);
        node.visit_with(self)
    }

    fn visit_class_declaration(
        &mut self,
        node: &'a boa_ast::function::ClassDeclaration,
    ) -> ControlFlow<Self::BreakTy> {
        self.syntax_found.insert(Syntax::Class);
        node.visit_with(self)
    }

    fn visit_class_expression(
        &mut self,
        node: &'a boa_ast::function::ClassExpression,
    ) -> ControlFlow<Self::BreakTy> {
        self.syntax_found.insert(Syntax::Class);
        node.visit_with(self)
    }

    fn visit_class_element(&mut self, node: &'a ClassElement) -> ControlFlow<Self::BreakTy> {
        match node {
            ClassElement::FieldDefinition(_)
            | ClassElement::StaticFieldDefinition(_)
            | ClassElement::PrivateFieldDefinition(_)
            | ClassElement::PrivateStaticFieldDefinition(_, _) => {
                self.syntax_found.insert(Syntax::ClassFieldDeclaration);
            }
            ClassElement::StaticBlock(_) => {
                self.syntax_found.insert(Syntax::StaticBlock);
            }
            ClassElement::MethodDefinition(_) => {}
        }
        node.visit_with(self)
    }

    fn visit_private_name(
        &mut self,
        node: &'a boa_ast::function::PrivateName,
    ) -> ControlFlow<Self::BreakTy> {
        self.syntax_found.insert(Syntax::PrivateField);
        node.visit_with(self)
    }

    fn visit_function_body(
        &mut self,
        node: &'a boa_ast::function::FunctionBody,
    ) -> ControlFlow<Self::BreakTy> {
        let already_inside = self.inside_fn;
        self.inside_fn = true;
        let res = node.visit_with(self);
        self.inside_fn = already_inside;
        res
    }

    fn visit_for_of_loop(
        &mut self,
        node: &'a boa_ast::statement::ForOfLoop,
    ) -> ControlFlow<Self::BreakTy> {
        if node.r#await() {
            self.syntax_found.insert(Syntax::ForAwait);
        } else {
            self.syntax_found.insert(Syntax::ForOfLoop);
        }
        node.visit_with(self)
    }

    fn visit_generator_declaration(
        &mut self,
        node: &'a boa_ast::function::GeneratorDeclaration,
    ) -> ControlFlow<Self::BreakTy> {
        self.syntax_found.insert(Syntax::Generator);
        node.visit_with(self)
    }

    fn visit_generator_expression(
        &mut self,
        node: &'a boa_ast::function::GeneratorExpression,
    ) -> ControlFlow<Self::BreakTy> {
        self.syntax_found.insert(Syntax::Generator);
        node.visit_with(self)
    }

    fn visit_arrow_function(
        &mut self,
        node: &'a boa_ast::function::ArrowFunction,
    ) -> ControlFlow<Self::BreakTy> {
        self.syntax_found.insert(Syntax::ArrowFunction);
        node.visit_with(self)
    }

    fn visit_async_function_declaration(
        &mut self,
        node: &'a boa_ast::function::AsyncFunctionDeclaration,
    ) -> ControlFlow<Self::BreakTy> {
        self.syntax_found.insert(Syntax::AsyncFn);
        node.visit_with(self)
    }

    fn visit_async_function_expression(
        &mut self,
        node: &'a boa_ast::function::AsyncFunctionExpression,
    ) -> ControlFlow<Self::BreakTy> {
        self.syntax_found.insert(Syntax::AsyncFn);
        node.visit_with(self)
    }

    fn visit_async_arrow_function(
        &mut self,
        node: &'a boa_ast::function::AsyncArrowFunction,
    ) -> ControlFlow<Self::BreakTy> {
        self.syntax_found.insert(Syntax::AsyncFn);
        node.visit_with(self)
    }

    fn visit_async_generator_declaration(
        &mut self,
        node: &'a boa_ast::function::AsyncGeneratorDeclaration,
    ) -> ControlFlow<Self::BreakTy> {
        self.syntax_found.insert(Syntax::AsyncGenerator);
        node.visit_with(self)
    }

    fn visit_async_generator_expression(
        &mut self,
        node: &'a boa_ast::function::AsyncGeneratorExpression,
    ) -> ControlFlow<Self::BreakTy> {
        self.syntax_found.insert(Syntax::AsyncGenerator);
        node.visit_with(self)
    }

    fn visit_await(&mut self, node: &'a boa_ast::expression::Await) -> ControlFlow<Self::BreakTy> {
        if self.inside_fn {
            self.syntax_found.insert(Syntax::Await);
        } else {
            self.syntax_found.insert(Syntax::TopLevelAwait);
        }
        node.visit_with(self)
    }

    fn visit_optional(
        &mut self,
        node: &'a boa_ast::expression::Optional,
    ) -> std::ops::ControlFlow<Self::BreakTy> {
        self.syntax_found.insert(Syntax::OptionalChaining);
        node.visit_with(self)
    }

    fn visit_binary(
        &mut self,
        node: &'a boa_ast::expression::operator::Binary,
    ) -> ControlFlow<Self::BreakTy> {
        match node.op() {
            BinaryOp::Logical(LogicalOp::Coalesce) => {
                self.syntax_found.insert(Syntax::NullishCoalescingOperator);
            }
            BinaryOp::Arithmetic(ArithmeticOp::Exp) => {
                self.syntax_found.insert(Syntax::Exponentiation);
            }
            _ => {}
        }
        node.visit_with(self)
    }

    fn visit_assign(
        &mut self,
        node: &'a boa_ast::expression::operator::Assign,
    ) -> ControlFlow<Self::BreakTy> {
        fn syntax(node: &boa_ast::expression::operator::Assign) -> Option<Syntax> {
            let v = match node.op() {
                AssignOp::Coalesce => Syntax::NullishCoalescingAssignment,
                AssignOp::BoolAnd => Syntax::LogicalAndAssignment,
                AssignOp::BoolOr => Syntax::LogicalOrAssignment,
                AssignOp::Exp => Syntax::Exponentiation,
                _ => return None,
            };
            Some(v)
        }
        if let Some(syntax) = syntax(node) {
            self.syntax_found.insert(syntax);
        }

        node.visit_with(self)
    }

    fn visit_spread(
        &mut self,
        node: &'a boa_ast::expression::Spread,
    ) -> ControlFlow<Self::BreakTy> {
        self.syntax_found.insert(Syntax::SpreadOperator);
        node.visit_with(self)
    }

    fn visit_property_definition(
        &mut self,
        node: &'a PropertyDefinition,
    ) -> ControlFlow<Self::BreakTy> {
        if let PropertyDefinition::SpreadObject(_) = node {
            self.syntax_found.insert(Syntax::SpreadOperator);
        }
        node.visit_with(self)
    }

    fn visit_template_literal(
        &mut self,
        node: &'a boa_ast::expression::literal::TemplateLiteral,
    ) -> ControlFlow<Self::BreakTy> {
        self.syntax_found.insert(Syntax::TemplateLiteral);
        node.visit_with(self)
    }

    fn visit_literal(&mut self, node: &'a Literal) -> ControlFlow<Self::BreakTy> {
        if let Literal::BigInt(_) = node {
            self.syntax_found.insert(Syntax::BigIntLiteral);
        }

        node.visit_with(self)
    }

    fn visit_reg_exp_literal(
        &mut self,
        node: &'a boa_ast::expression::RegExpLiteral,
    ) -> ControlFlow<Self::BreakTy> {
        let flags = self.interner.resolve_expect(node.flags()).utf16();
        for c in flags {
            let Some(c) = char::from_u32(u32::from(*c)) else {
                continue;
            };
            let syntax = match c {
                's' => Syntax::RegExpFlagS,
                'd' => Syntax::RegExpFlagD,
                'v' => Syntax::RegExpFlagV,
                _ => continue,
            };
            self.syntax_found.insert(syntax);
        }

        node.visit_with(self)
    }

    fn visit_catch(&mut self, node: &'a boa_ast::statement::Catch) -> ControlFlow<Self::BreakTy> {
        if node.parameter().is_none() {
            self.syntax_found.insert(Syntax::CatchWithoutBinding);
        }

        node.visit_with(self)
    }

    fn visit_formal_parameter_list(
        &mut self,
        node: &'a boa_ast::function::FormalParameterList,
    ) -> ControlFlow<Self::BreakTy> {
        if node.has_rest_parameter() {
            self.syntax_found.insert(Syntax::RestParameter);
        }
        node.visit_with(self)
    }
}

#[derive(ClapParser)]
struct Cli {
    input: PathBuf,
    #[arg(long)]
    ast: bool,
}

fn main() {
    let args = Cli::parse();

    let source = Source::from_filepath(&args.input).unwrap();
    let mut interner = Interner::new();
    let mut parser = Parser::new(source);
    let script = parser
        .parse_module(&Scope::new_global(), &mut interner)
        .unwrap();

    if args.ast {
        eprintln!("{script:#?}");
    }

    let mut visitor = SyntaxVersionVisitor::new(&interner);
    script.visit_with(&mut visitor);

    let mut syntax_by_version: BTreeMap<Version, BTreeSet<Syntax>> = BTreeMap::new();
    for syntax in visitor.syntax_found {
        syntax_by_version
            .entry(syntax.version_required())
            .or_default()
            .insert(syntax);
    }

    for (version, syntaxes) in syntax_by_version {
        println!("{version} required because of:");
        for syntax in syntaxes {
            println!("- {syntax}");
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn syntax_required(source: &'static str) -> BTreeSet<Syntax> {
        let source = Source::from_bytes(source.as_bytes());
        let mut interner = Interner::new();
        let mut parser = Parser::new(source);
        let script = parser
            .parse_module(&Scope::new_global(), &mut interner)
            .unwrap();
        // dbg!(&script);
        let mut visitor = SyntaxVersionVisitor::new(&interner);
        script.visit_with(&mut visitor);
        visitor.syntax_found
    }

    #[test]
    fn empty() {
        assert_eq!(syntax_required(""), BTreeSet::new());
    }

    #[test]
    fn import() {
        assert_eq!(
            syntax_required("import 'a';"),
            BTreeSet::from_iter([Syntax::Import])
        );
    }

    #[test]
    fn import_one() {
        assert_eq!(
            syntax_required("import Foo from 'a';"),
            BTreeSet::from_iter([Syntax::Import])
        );
    }

    #[test]
    fn import_multi() {
        assert_eq!(
            syntax_required("import {Foo, Bar} from 'a';"),
            BTreeSet::from_iter([Syntax::Import])
        );
    }

    #[test]
    fn import_all() {
        assert_eq!(
            syntax_required("import * as Foo from 'a';"),
            BTreeSet::from_iter([Syntax::Import])
        );
    }

    #[test]
    fn import_dynamic() {
        assert_eq!(
            syntax_required("import('a');"),
            BTreeSet::from_iter([Syntax::DynamicImport])
        );
    }

    #[test]
    fn export_fn() {
        assert_eq!(
            syntax_required("export function f() {};"),
            BTreeSet::from_iter([Syntax::Export])
        );
    }

    #[test]
    fn export_const() {
        assert_eq!(
            syntax_required("export const x = 1;"),
            BTreeSet::from_iter([Syntax::Const, Syntax::Export])
        );
    }

    #[test]
    fn export_obj() {
        assert_eq!(
            syntax_required("function f() {}; export {f};"),
            BTreeSet::from_iter([Syntax::Export])
        );
    }

    #[test]
    fn export_default() {
        assert_eq!(
            syntax_required("export default {};"),
            BTreeSet::from_iter([Syntax::Export])
        );
    }

    #[test]
    fn let_binding() {
        assert_eq!(
            syntax_required("let x = 1;"),
            BTreeSet::from_iter([Syntax::Let])
        );
    }

    #[test]
    fn const_binding() {
        assert_eq!(
            syntax_required("const x = 1;"),
            BTreeSet::from_iter([Syntax::Const])
        );
    }

    #[test]
    fn class_decl() {
        assert_eq!(
            syntax_required("class C {}"),
            BTreeSet::from_iter([Syntax::Class])
        );
    }

    #[test]
    fn class_expr() {
        assert_eq!(
            syntax_required("var x = class {};"),
            BTreeSet::from_iter([Syntax::Class])
        );
    }

    #[test]
    fn for_of() {
        assert_eq!(
            syntax_required("for (var i of []) {}"),
            BTreeSet::from_iter([Syntax::ForOfLoop])
        );
    }

    #[test]
    fn for_await() {
        assert_eq!(
            syntax_required("for await (var i of []) {}"),
            BTreeSet::from_iter([Syntax::ForAwait])
        );
    }

    #[test]
    fn arrow_fn() {
        assert_eq!(
            syntax_required("var f = () => {};"),
            BTreeSet::from_iter([Syntax::ArrowFunction])
        );
    }

    #[test]
    fn exp_op() {
        assert_eq!(
            syntax_required("var x = 2 ** 3;"),
            BTreeSet::from_iter([Syntax::Exponentiation])
        );
    }

    #[test]
    fn exp_assign() {
        assert_eq!(
            syntax_required("var x = 2; x **= 3;"),
            BTreeSet::from_iter([Syntax::Exponentiation])
        );
    }

    #[test]
    fn template_literal() {
        assert_eq!(
            syntax_required("var x = `${1 + 1}`;"),
            BTreeSet::from_iter([Syntax::TemplateLiteral])
        );
    }

    #[test]
    fn bigint_literal() {
        assert_eq!(
            syntax_required("var x = 1n;"),
            BTreeSet::from_iter([Syntax::BigIntLiteral])
        );
    }

    #[test]
    fn optional_chain() {
        assert_eq!(
            syntax_required("var x = a?.b;"),
            BTreeSet::from_iter([Syntax::OptionalChaining])
        );
    }

    #[test]
    fn nullish_coalescing_op() {
        assert_eq!(
            syntax_required("var x = a ?? b;"),
            BTreeSet::from_iter([Syntax::NullishCoalescingOperator])
        );
    }

    #[test]
    fn nullish_coalescing_assign() {
        assert_eq!(
            syntax_required("var x = a; x ??= b;"),
            BTreeSet::from_iter([Syntax::NullishCoalescingAssignment])
        );
    }

    #[test]
    fn logical_and_assign() {
        assert_eq!(
            syntax_required("var x = a; x &&= b;"),
            BTreeSet::from_iter([Syntax::LogicalAndAssignment])
        );
    }

    #[test]
    fn logical_or_assign() {
        assert_eq!(
            syntax_required("var x = a; x ||= b;"),
            BTreeSet::from_iter([Syntax::LogicalOrAssignment])
        );
    }

    #[test]
    fn regexp_no_flags() {
        assert_eq!(syntax_required("var x = /foo/;"), BTreeSet::new());
    }

    #[test]
    fn regexp_flag_d() {
        assert_eq!(
            syntax_required("var x = /foo/d;"),
            BTreeSet::from_iter([Syntax::RegExpFlagD])
        );
    }

    #[test]
    fn regexp_flag_s() {
        assert_eq!(
            syntax_required("var x = /foo/s;"),
            BTreeSet::from_iter([Syntax::RegExpFlagS])
        );
    }

    #[test]
    fn regexp_flag_v() {
        assert_eq!(
            syntax_required("var x = /foo/v;"),
            BTreeSet::from_iter([Syntax::RegExpFlagV])
        );
    }

    #[test]
    fn regexp_flag_all_u() {
        assert_eq!(
            syntax_required("var x = /foo/dgimsu;"),
            BTreeSet::from_iter([Syntax::RegExpFlagS, Syntax::RegExpFlagD])
        );
    }

    #[test]
    fn regexp_flag_all_v() {
        assert_eq!(
            syntax_required("var x = /foo/dgimsv;"),
            BTreeSet::from_iter([
                Syntax::RegExpFlagS,
                Syntax::RegExpFlagD,
                Syntax::RegExpFlagV
            ])
        );
    }

    #[test]
    fn only_rest_param() {
        assert_eq!(
            syntax_required("function f(...a) {}"),
            BTreeSet::from_iter([Syntax::RestParameter])
        );
    }

    #[test]
    fn rest_param() {
        assert_eq!(
            syntax_required("function f(a, b, ...c) {}"),
            BTreeSet::from_iter([Syntax::RestParameter])
        );
    }

    #[test]
    fn gen_decl() {
        assert_eq!(
            syntax_required("function* f() {}"),
            BTreeSet::from_iter([Syntax::Generator])
        );
    }

    #[test]
    fn gen_expr() {
        assert_eq!(
            syntax_required("var f = function*() {};"),
            BTreeSet::from_iter([Syntax::Generator])
        );
    }

    #[test]
    fn async_fn_decl() {
        assert_eq!(
            syntax_required("async function f() {}"),
            BTreeSet::from_iter([Syntax::AsyncFn])
        );
    }

    #[test]
    fn async_fn_decl_rest_param() {
        assert_eq!(
            syntax_required("async function f(a, ...b) {}"),
            BTreeSet::from_iter([Syntax::AsyncFn, Syntax::RestParameter])
        );
    }

    #[test]
    fn async_fn_expr() {
        assert_eq!(
            syntax_required("var f = async function() {};"),
            BTreeSet::from_iter([Syntax::AsyncFn])
        );
    }

    #[test]
    fn async_arrow_fn() {
        assert_eq!(
            syntax_required("var f = async () => {};"),
            BTreeSet::from_iter([Syntax::AsyncFn])
        );
    }

    #[test]
    fn await_expr() {
        assert_eq!(
            syntax_required("async function f(a) { await a; }"),
            BTreeSet::from_iter([Syntax::AsyncFn, Syntax::Await])
        );
    }

    #[test]
    fn async_gen_decl() {
        assert_eq!(
            syntax_required("async function* f() {}"),
            BTreeSet::from_iter([Syntax::AsyncGenerator])
        );
    }

    #[test]
    fn async_gen_expr() {
        assert_eq!(
            syntax_required("var f = async function*() {};"),
            BTreeSet::from_iter([Syntax::AsyncGenerator])
        );
    }

    #[test]
    fn top_level_await() {
        assert_eq!(
            syntax_required("await a;"),
            BTreeSet::from_iter([Syntax::TopLevelAwait])
        );
    }

    #[test]
    fn private_field() {
        assert_eq!(
            syntax_required("class C { #a; }"),
            BTreeSet::from_iter([
                Syntax::Class,
                Syntax::ClassFieldDeclaration,
                Syntax::PrivateField
            ])
        );
    }

    #[test]
    fn private_method() {
        assert_eq!(
            syntax_required("class C { #f() {} }"),
            BTreeSet::from_iter([Syntax::Class, Syntax::PrivateField])
        );
    }

    #[test]
    fn field_decl() {
        assert_eq!(
            syntax_required("class C { a; }"),
            BTreeSet::from_iter([Syntax::Class, Syntax::ClassFieldDeclaration])
        );
    }

    #[test]
    fn field_init() {
        assert_eq!(
            syntax_required("class C { a = 1; }"),
            BTreeSet::from_iter([Syntax::Class, Syntax::ClassFieldDeclaration])
        );
    }

    #[test]
    fn static_field_decl() {
        assert_eq!(
            syntax_required("class C { static a; }"),
            BTreeSet::from_iter([Syntax::Class, Syntax::ClassFieldDeclaration])
        );
    }

    #[test]
    fn static_field_init() {
        assert_eq!(
            syntax_required("class C { static a = 1; }"),
            BTreeSet::from_iter([Syntax::Class, Syntax::ClassFieldDeclaration])
        );
    }

    #[test]
    fn static_block() {
        assert_eq!(
            syntax_required("class C { static { var x = 1 + 1; } }"),
            BTreeSet::from_iter([Syntax::Class, Syntax::StaticBlock])
        );
    }

    #[test]
    fn catch_binding() {
        assert_eq!(
            syntax_required("try { throw new Error(); } catch(e) {}"),
            BTreeSet::new()
        );
    }

    #[test]
    fn catch_no_binding() {
        assert_eq!(
            syntax_required("try { throw new Error(); } catch {}"),
            BTreeSet::from_iter([Syntax::CatchWithoutBinding])
        );
    }

    #[test]
    fn spread_obj() {
        assert_eq!(
            syntax_required("var x = {...a};"),
            BTreeSet::from_iter([Syntax::SpreadOperator])
        );
    }

    #[test]
    fn spread_arr() {
        assert_eq!(
            syntax_required("var x = [...a];"),
            BTreeSet::from_iter([Syntax::SpreadOperator])
        );
    }

    #[test]
    fn spread_call() {
        assert_eq!(
            syntax_required("f(...a);"),
            BTreeSet::from_iter([Syntax::SpreadOperator])
        );
    }

    #[test]
    fn all_syntax() {
        assert_eq!(
            syntax_required(include_str!("../test.js")),
            BTreeSet::from_iter([
                Syntax::Class,
                Syntax::ForOfLoop,
                Syntax::ArrowFunction,
                Syntax::Generator,
                Syntax::Import,
                Syntax::Export,
                Syntax::Let,
                Syntax::Const,
                Syntax::TemplateLiteral,
                Syntax::Exponentiation,
                Syntax::AsyncFn,
                Syntax::Await,
                Syntax::AsyncGenerator,
                Syntax::ForAwait,
                Syntax::SpreadOperator,
                Syntax::RestParameter,
                Syntax::RegExpFlagS,
                Syntax::CatchWithoutBinding,
                Syntax::BigIntLiteral,
                Syntax::OptionalChaining,
                Syntax::NullishCoalescingOperator,
                Syntax::DynamicImport,
                Syntax::NullishCoalescingAssignment,
                Syntax::LogicalAndAssignment,
                Syntax::LogicalOrAssignment,
                Syntax::ClassFieldDeclaration,
                Syntax::PrivateField,
                Syntax::StaticBlock,
                Syntax::TopLevelAwait,
                Syntax::RegExpFlagD,
                Syntax::RegExpFlagV,
            ])
        );
    }
}
