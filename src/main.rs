#![deny(clippy::pedantic)]

use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
    path::PathBuf,
    sync::LazyLock,
};

use clap::Parser as ClapParser;
use ecmascript_syntax::{
    browser_compat_data::{self, VersionAdded},
    syntax::{Syntax, Version},
    visitor::find_syntax_used,
};
use swc_common::{
    SourceMap, Span,
    errors::{ColorConfig, Handler},
    sync::Lrc,
};
use swc_core::ecma::visit::swc_ecma_ast::EsVersion;
use swc_ecma_parser::{EsSyntax, StringInput, lexer::Lexer};

#[derive(ClapParser)]
struct Cli {
    input: PathBuf,
    #[arg(long)]
    ast: bool,
}

fn format_span(source_map: &SourceMap, span: Span) -> impl Display {
    std::fmt::from_fn(move |f| {
        let lo = source_map.lookup_char_pos(span.lo);
        write!(f, "{}:{}:{}", lo.file.name, lo.line, lo.col_display)
    })
}

fn main() {
    LazyLock::force(&browser_compat_data::DATA);
    let args = Cli::parse();

    let source_map = Lrc::<SourceMap>::default();

    let handler =
        Handler::with_tty_emitter(ColorConfig::Auto, true, false, Some(source_map.clone()));
    let source_file = source_map.load_file(&args.input).unwrap();
    let lexer = Lexer::new(
        swc_ecma_parser::Syntax::Es(EsSyntax::default()),
        EsVersion::default(),
        StringInput::from(&*source_file),
        None,
    );

    let mut parser = swc_ecma_parser::Parser::new_from(lexer);

    for e in parser.take_errors() {
        e.into_diagnostic(&handler).emit();
    }

    let module = parser
        .parse_module()
        .map_err(|e| e.into_diagnostic(&handler).emit())
        .unwrap();

    if args.ast {
        println!("{module:#?}");
    }

    let syntax_used = find_syntax_used(&source_map, &module);

    let mut syntax_by_version: BTreeMap<Version, BTreeSet<(Syntax, Span)>> = BTreeMap::new();
    for (syntax, span) in syntax_used {
        syntax_by_version
            .entry(syntax.version_required())
            .or_default()
            .insert((syntax, span));
    }

    let mut max_version = None;

    for (version, syntaxes) in syntax_by_version {
        println!("{version} required because of:");
        for (syntax, span) in syntaxes {
            print!("- {syntax}");
            if let Some(version) = minimum_chrome_version(syntax) {
                println!(": (Supported since Chrome {version})");
                max_version = max_version.max(Some(version));
            } else {
                println!();
            }
            println!("  First encountered at {}", format_span(&source_map, span));
        }
    }

    if let Some(max_version) = max_version {
        println!();
        println!("Minimum Chrome version required: {max_version}");
    }
}

fn minimum_chrome_version(syntax: Syntax) -> Option<&'static browser_compat_data::Version> {
    let path = syntax.browser_compat_path()?;
    let compat_data = browser_compat_data::DATA.compat_data(path);
    let chrome_data = &compat_data.support.chrome;
    let chrome_data = chrome_data.first().unwrap();
    assert!(!chrome_data.partial_implementation);

    match &chrome_data.version_added {
        VersionAdded::Version(v) => Some(v),
        VersionAdded::NotAdded(_) => panic!("unexpected not added"),
    }
}

#[cfg(test)]
mod test {
    use strum::VariantArray;
    use swc_common::FileName;

    use super::*;

    fn syntax_required(source: &'static str) -> BTreeSet<Syntax> {
        let source_map = Lrc::<SourceMap>::default();

        let handler =
            Handler::with_tty_emitter(ColorConfig::Auto, true, false, Some(source_map.clone()));
        let source_file =
            source_map.new_source_file(FileName::Real("test.js".into()).into(), source);
        let lexer = Lexer::new(
            swc_ecma_parser::Syntax::Es(EsSyntax::default()),
            EsVersion::default(),
            StringInput::from(&*source_file),
            None,
        );

        let mut parser = swc_ecma_parser::Parser::new_from(lexer);

        for e in parser.take_errors() {
            e.into_diagnostic(&handler).emit();
        }

        let module = parser
            .parse_module()
            .map_err(|e| e.into_diagnostic(&handler).emit())
            .unwrap();

        //dbg!(&module);

        find_syntax_used(&source_map, &module).into_keys().collect()
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
    fn numeric_separator_int() {
        assert_eq!(
            syntax_required("var x = 1_000_000;"),
            BTreeSet::from_iter([Syntax::NumericSeparator])
        );
    }

    #[test]
    fn numeric_separator_float() {
        assert_eq!(
            syntax_required("var x = 1.2_3;"),
            BTreeSet::from_iter([Syntax::NumericSeparator])
        );
    }

    #[test]
    fn numeric_separator_bigint() {
        assert_eq!(
            syntax_required("var x = 1_2_3n;"),
            BTreeSet::from_iter([Syntax::BigIntLiteral, Syntax::NumericSeparator])
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
    fn regexp_no_modifiers() {
        assert_eq!(
            syntax_required("var x = /(?:foo)/;"),
            BTreeSet::from_iter([])
        );
    }

    #[test]
    fn regexp_ignore_modifier() {
        assert_eq!(
            syntax_required("var x = /(?i:foo)/;"),
            BTreeSet::from_iter([Syntax::RegExpInlineModifier])
        );
    }

    #[test]
    fn regexp_nested_modifier() {
        assert_eq!(
            syntax_required("var x = /(?:(?:(?i:foo)))/;"),
            BTreeSet::from_iter([Syntax::RegExpInlineModifier])
        );
    }

    #[test]
    fn regexp_disable_ignore_modifier() {
        assert_eq!(
            syntax_required("var x = /(?-i:foo)/;"),
            BTreeSet::from_iter([Syntax::RegExpInlineModifier])
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
            BTreeSet::from_iter([Syntax::ArrowFunction, Syntax::AsyncFn])
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
    fn await_expr_in_async_arrow() {
        assert_eq!(
            syntax_required("async (a) => { await a; }"),
            BTreeSet::from_iter([Syntax::ArrowFunction, Syntax::AsyncFn, Syntax::Await])
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
            Syntax::VARIANTS.iter().copied().collect(),
        );
    }

    #[test]
    fn compat_data() {
        for &syntax in Syntax::VARIANTS {
            _ = minimum_chrome_version(syntax);
        }
    }
}
