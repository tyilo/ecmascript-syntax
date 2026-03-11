use std::collections::BTreeSet;

use swc_common::{SourceMap, Spanned};
use swc_core::ecma::{
    self,
    visit::{
        Visit, VisitWith,
        swc_ecma_ast::{
            ArrowExpr, AssignOp, AwaitExpr, BigInt, BinaryOp, Callee, CatchClause, ClassDecl,
            ClassExpr, ClassMember, ExprOrSpread, ForOfStmt, Function, Lit, ModuleDecl,
            OptChainExpr, Param, Pat, PrivateProp, Regex, SpreadElement, VarDeclKind,
        },
    },
};

use crate::syntax::Syntax;

pub fn find_syntax_used<'a>(
    source_map: &'a SourceMap,
    ast: &impl VisitWith<SyntaxVersionVisitor<'a>>,
) -> BTreeSet<Syntax> {
    let mut visitor = SyntaxVersionVisitor::new(source_map);
    ast.visit_with(&mut visitor);
    visitor.syntax_found
}

pub struct SyntaxVersionVisitor<'a> {
    source_map: &'a SourceMap,
    syntax_found: BTreeSet<Syntax>,
    inside_fn: bool,
}

impl<'a> SyntaxVersionVisitor<'a> {
    fn new(source_map: &'a SourceMap) -> Self {
        Self {
            source_map,
            syntax_found: BTreeSet::new(),
            inside_fn: false,
        }
    }
}

impl Visit for SyntaxVersionVisitor<'_> {
    fn visit_var_decl_kind(&mut self, node: &VarDeclKind) {
        let syntax = match node {
            VarDeclKind::Var => None,
            VarDeclKind::Let => Some(Syntax::Let),
            VarDeclKind::Const => Some(Syntax::Const),
        };
        if let Some(syntax) = syntax {
            self.syntax_found.insert(syntax);
        }
        node.visit_children_with(self);
    }

    fn visit_module_decl(&mut self, node: &ModuleDecl) {
        match node {
            ModuleDecl::Import(_) => {
                self.syntax_found.insert(Syntax::Import);
            }
            ModuleDecl::ExportDecl(_)
            | ModuleDecl::ExportNamed(_)
            | ModuleDecl::ExportDefaultDecl(_)
            | ModuleDecl::ExportDefaultExpr(_)
            | ModuleDecl::ExportAll(_) => {
                self.syntax_found.insert(Syntax::Export);
            }
            ModuleDecl::TsImportEquals(_)
            | ModuleDecl::TsExportAssignment(_)
            | ModuleDecl::TsNamespaceExport(_) => panic!("TypeScript syntax not supported"),
        }
        node.visit_children_with(self);
    }

    fn visit_callee(&mut self, node: &Callee) {
        match node {
            Callee::Import(_) => {
                self.syntax_found.insert(Syntax::DynamicImport);
            }
            Callee::Super(_) | Callee::Expr(_) => {}
        }
        node.visit_children_with(self);
    }

    fn visit_class_decl(&mut self, node: &ClassDecl) {
        self.syntax_found.insert(Syntax::Class);
        node.visit_children_with(self);
    }

    fn visit_class_expr(&mut self, node: &ClassExpr) {
        self.syntax_found.insert(Syntax::Class);
        node.visit_children_with(self);
    }

    fn visit_class_member(&mut self, node: &ClassMember) {
        match node {
            ClassMember::ClassProp(_) | ClassMember::PrivateProp(_) => {
                self.syntax_found.insert(Syntax::ClassFieldDeclaration);
            }
            ClassMember::StaticBlock(_) => {
                self.syntax_found.insert(Syntax::StaticBlock);
            }
            ClassMember::PrivateMethod(_) => {
                self.syntax_found.insert(Syntax::PrivateField);
            }
            ClassMember::Constructor(_)
            | ClassMember::Method(_)
            | ClassMember::Empty(_)
            | ClassMember::AutoAccessor(_) => {}
            ClassMember::TsIndexSignature(_) => panic!("TypeScript syntax not supported"),
        }

        node.visit_children_with(self);
    }

    fn visit_private_prop(&mut self, node: &PrivateProp) {
        self.syntax_found.insert(Syntax::PrivateField);
        node.visit_children_with(self);
    }

    fn visit_function(&mut self, node: &Function) {
        match (node.is_async, node.is_generator) {
            (true, true) => {
                self.syntax_found.insert(Syntax::AsyncGenerator);
            }
            (true, false) => {
                self.syntax_found.insert(Syntax::AsyncFn);
            }
            (false, true) => {
                self.syntax_found.insert(Syntax::Generator);
            }
            (false, false) => {}
        }

        let already_inside = self.inside_fn;
        self.inside_fn = true;
        node.visit_children_with(self);
        self.inside_fn = already_inside;
    }

    fn visit_await_expr(&mut self, node: &AwaitExpr) {
        if self.inside_fn {
            self.syntax_found.insert(Syntax::Await);
        } else {
            self.syntax_found.insert(Syntax::TopLevelAwait);
        }
        node.visit_children_with(self);
    }

    fn visit_for_of_stmt(&mut self, node: &ForOfStmt) {
        if node.is_await {
            self.syntax_found.insert(Syntax::ForAwait);
        } else {
            self.syntax_found.insert(Syntax::ForOfLoop);
        }
        node.visit_children_with(self);
    }

    fn visit_arrow_expr(&mut self, node: &ArrowExpr) {
        self.syntax_found.insert(Syntax::ArrowFunction);
        if node.is_async {
            self.syntax_found.insert(Syntax::AsyncFn);
        }
        node.visit_children_with(self);
    }

    fn visit_opt_chain_expr(&mut self, node: &OptChainExpr) {
        self.syntax_found.insert(Syntax::OptionalChaining);
        node.visit_children_with(self);
    }

    fn visit_binary_op(&mut self, node: &BinaryOp) {
        match node {
            BinaryOp::Exp => {
                self.syntax_found.insert(Syntax::Exponentiation);
            }
            BinaryOp::NullishCoalescing => {
                self.syntax_found.insert(Syntax::NullishCoalescingOperator);
            }
            BinaryOp::EqEq
            | BinaryOp::NotEq
            | BinaryOp::EqEqEq
            | BinaryOp::NotEqEq
            | BinaryOp::Lt
            | BinaryOp::LtEq
            | BinaryOp::Gt
            | BinaryOp::GtEq
            | BinaryOp::LShift
            | BinaryOp::RShift
            | BinaryOp::ZeroFillRShift
            | BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::Mod
            | BinaryOp::BitOr
            | BinaryOp::BitXor
            | BinaryOp::BitAnd
            | BinaryOp::LogicalOr
            | BinaryOp::LogicalAnd
            | BinaryOp::In
            | BinaryOp::InstanceOf => {}
        }
        node.visit_children_with(self);
    }

    fn visit_assign_op(&mut self, node: &AssignOp) {
        match node {
            AssignOp::ExpAssign => {
                self.syntax_found.insert(Syntax::Exponentiation);
            }
            AssignOp::AndAssign => {
                self.syntax_found.insert(Syntax::LogicalAndAssignment);
            }
            AssignOp::OrAssign => {
                self.syntax_found.insert(Syntax::LogicalOrAssignment);
            }
            AssignOp::NullishAssign => {
                self.syntax_found
                    .insert(Syntax::NullishCoalescingAssignment);
            }
            AssignOp::Assign
            | AssignOp::AddAssign
            | AssignOp::SubAssign
            | AssignOp::MulAssign
            | AssignOp::DivAssign
            | AssignOp::ModAssign
            | AssignOp::LShiftAssign
            | AssignOp::RShiftAssign
            | AssignOp::ZeroFillRShiftAssign
            | AssignOp::BitOrAssign
            | AssignOp::BitXorAssign
            | AssignOp::BitAndAssign => {}
        }
    }

    fn visit_spread_element(&mut self, node: &SpreadElement) {
        self.syntax_found.insert(Syntax::SpreadOperator);
        node.visit_children_with(self);
    }

    fn visit_expr_or_spread(&mut self, node: &ExprOrSpread) {
        if node.spread.is_some() {
            self.syntax_found.insert(Syntax::SpreadOperator);
        }
        node.visit_children_with(self);
    }

    fn visit_tpl(&mut self, node: &ecma::visit::swc_ecma_ast::Tpl) {
        self.syntax_found.insert(Syntax::TemplateLiteral);
        node.visit_children_with(self);
    }

    fn visit_regex(&mut self, node: &Regex) {
        let parser = swc_ecma_regexp::LiteralParser::new(
            node.exp.as_str(),
            Some(node.flags.as_str()),
            swc_ecma_regexp::Options {
                pattern_span_offset: 0,
                flags_span_offset: 0,
            },
        );
        let pattern = parser.parse().unwrap();
        swc_ecma_regexp_visit::VisitWith::visit_with(&pattern, self);

        for c in node.flags.chars() {
            let syntax = match c {
                's' => Syntax::RegExpFlagS,
                'd' => Syntax::RegExpFlagD,
                'v' => Syntax::RegExpFlagV,
                _ => continue,
            };
            self.syntax_found.insert(syntax);
        }
        node.visit_children_with(self);
    }

    fn visit_catch_clause(&mut self, node: &CatchClause) {
        if node.param.is_none() {
            self.syntax_found.insert(Syntax::CatchWithoutBinding);
        }
        node.visit_children_with(self);
    }

    fn visit_params(&mut self, node: &[Param]) {
        if node.iter().any(|param| matches!(param.pat, Pat::Rest(..))) {
            self.syntax_found.insert(Syntax::RestParameter);
        }
        node.visit_children_with(self);
    }

    fn visit_lit(&mut self, node: &Lit) {
        match node {
            Lit::Num(_) | Lit::BigInt(_) => {
                let contains_underscore = self
                    .source_map
                    .with_snippet_of_span(node.span(), |s| s.contains('_'))
                    .unwrap();
                if contains_underscore {
                    self.syntax_found.insert(Syntax::NumericSeparator);
                }
            }
            Lit::Str(_) | Lit::Bool(_) | Lit::Null(_) | Lit::Regex(_) | Lit::JSXText(_) => {}
        }

        node.visit_children_with(self);
    }

    fn visit_big_int(&mut self, node: &BigInt) {
        self.syntax_found.insert(Syntax::BigIntLiteral);
        node.visit_children_with(self);
    }
}

impl swc_ecma_regexp_visit::Visit for SyntaxVersionVisitor<'_> {
    fn visit_ignore_group(&mut self, node: &swc_ecma_regexp::ast::IgnoreGroup) {
        if let Some(modifiers) = &node.modifiers
            && !(modifiers.enabling.is_empty() && modifiers.disabling.is_empty())
        {
            self.syntax_found.insert(Syntax::RegExpInlineModifier);
        }

        swc_ecma_regexp_visit::VisitWith::visit_children_with(node, self);
    }
}
