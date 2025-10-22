use std::{
    collections::{BTreeMap, BTreeSet},
    path::PathBuf,
    sync::LazyLock,
};

use boa_ast::scope::Scope;
use boa_interner::Interner;
use boa_parser::{Parser, Source};
use clap::Parser as ClapParser;
use ecmascript_syntax::{
    browser_compat_data::{self, VersionAdded},
    syntax::{Syntax, Version},
    visitor::find_syntax_used,
};

#[derive(ClapParser)]
struct Cli {
    input: PathBuf,
    #[arg(long)]
    ast: bool,
}

fn main() {
    let data = LazyLock::force(&browser_compat_data::DATA);
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

    let syntax_used = find_syntax_used(&interner, script);

    let mut syntax_by_version: BTreeMap<Version, BTreeSet<Syntax>> = BTreeMap::new();
    for syntax in syntax_used {
        syntax_by_version
            .entry(syntax.version_required())
            .or_default()
            .insert(syntax);
    }

    for (version, syntaxes) in syntax_by_version {
        println!("{version} required because of:");
        for syntax in syntaxes {
            print!("- {syntax}");
            if let Some(path) = syntax.browser_compat_path() {
                let compat_data = data.compat_data(path);
                let chrome_data = &compat_data.support.chrome;
                let chrome_data = chrome_data.first().unwrap();
                assert!(!chrome_data.partial_implementation);

                let version = match &chrome_data.version_added {
                    VersionAdded::Version(v) => v,
                    VersionAdded::NotAdded(_) => panic!("unexpected not added"),
                };

                println!(": (Supported since Chrome {version})");
            } else {
                println!();
            }
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
        find_syntax_used(&interner, script)
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
    fn class_decl_with_keyword_name() {
        assert_eq!(
            syntax_required("class of {}"),
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
