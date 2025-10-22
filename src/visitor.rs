use std::{collections::BTreeSet, ops::ControlFlow};

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
    visitor::{VisitWith, Visitor},
};
use boa_interner::Interner;

use crate::syntax::Syntax;

pub fn find_syntax_used(interner: &Interner, ast: impl VisitWith) -> BTreeSet<Syntax> {
    let mut visitor = SyntaxVersionVisitor::new(interner);
    _ = ast.visit_with(&mut visitor);
    visitor.syntax_found
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
