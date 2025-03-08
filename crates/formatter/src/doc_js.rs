use bumpalo::{collections::Vec, Bump};
use javascript::ast::{
    ArrayExpression, ArrayPattern, AssignmentExpression, BinaryExpression, BooleanLiteral, Expression::{self, *}, Identifier, Import, NullLiteral, NumericLiteral, ObjectExpression, ObjectExpressionPropertyKind, ObjectPattern, ObjectPatternPropertyKind, Pattern, Statement::{self, *}, StringLiteral, VariableDeclaration, VariableDeclarator
};

use crate::{
    array,
    commands::{
        Command::{self, *},
        GroupOptions,
    },
    group, indent, text,
};

#[macro_export]
macro_rules! slice {
    ($code:expr, $start:expr, $end:expr) => {
        &$code[$start as usize..$end as usize]
    };
}

pub struct Doc<'a> {
    arena: &'a Bump,
    code: &'a str,
    ast: Vec<'a, Statement<'a>>,
}

impl<'a> Doc<'a> {
    fn build_from_array_pattern(&self, pattern: &ArrayPattern<'a>) -> Command<'a> {
        let mut array_cmd = bumpalo::vec![in self.arena];

        let it = pattern
            .elements
            .iter()
            .take((pattern.elements.len() as isize - 1).max(0) as usize);
        for el in it {
            array_cmd.push(self.build_from_pattern(el));
            array_cmd.push(text!(","));
            array_cmd.push(Line);
        }

        if let Some(last_el) = pattern.elements.last() {
            array_cmd.push(self.build_from_pattern(last_el));
        }

        group!(
            self.arena,
            [
                text!("["),
                indent!(self.arena, [Softline, Array(array_cmd)]),
                Softline,
                text!("]")
            ]
        )
    }

    fn build_from_obj_pattern_property(
        &self,
        property_kind: &ObjectPatternPropertyKind<'a>,
    ) -> (Command<'a>, bool) {
        match property_kind {
            ObjectPatternPropertyKind::Property(p) => {
                if p.computed {
                    (
                        array!(
                            self.arena,
                            [
                                text!("["),
                                self.build_from_expression(&p.key),
                                text!(": "),
                                self.build_from_pattern(&p.value),
                                text!("]")
                            ]
                        ),
                        false,
                    )
                } else {
                    let mut array_cmd =
                        bumpalo::vec![in self.arena; self.build_from_expression(&p.key)];

                    let add_hardline;

                    if !p.shorthand {
                        array_cmd.push(text!(": "));
                        array_cmd.push(self.build_from_pattern(&p.value));

                        if let Pattern::ObjectPattern(_) = p.value {
                            add_hardline = true
                        } else {
                            add_hardline = false
                        }
                    } else {
                        add_hardline = false
                    }

                    (Array(array_cmd), add_hardline)
                }
            }
            ObjectPatternPropertyKind::RestElement(rest_el) => (
                array!(
                    self.arena,
                    [text!("..."), self.build_from_pattern(&rest_el.argument)]
                ),
                false,
            ),
        }
    }

    fn build_from_object_pattern(&self, pattern: &ObjectPattern<'a>) -> Command<'a> {
        let mut array_cmd = bumpalo::vec![in self.arena];

        let it = pattern
            .properties
            .iter()
            .take((pattern.properties.len() as isize - 1).max(0) as usize);

        for property_kind in it {
            let (cmd, add_hardline) = self.build_from_obj_pattern_property(property_kind);

            array_cmd.push(cmd);
            array_cmd.push(text!(","));

            if add_hardline {
                array_cmd.push(Hardline);
            } else {
                array_cmd.push(Line);
            }
        }

        let mut softline_or_hardline = Softline;

        if let Some(property_kind) = pattern.properties.last() {
            let (cmd, add_hardline) = self.build_from_obj_pattern_property(property_kind);

            array_cmd.push(cmd);

            if add_hardline {
                softline_or_hardline = Hardline;
            }
        }

        group!(
            self.arena,
            [
                text!("{"),
                indent!(self.arena, [Softline, Array(array_cmd)]),
                softline_or_hardline,
                text!("}")
            ]
        )
    }

    fn build_from_pattern(&self, pattern: &Pattern<'a>) -> Command<'a> {
        match pattern {
            Pattern::IdentifierPattern(ident) => {
                text!(slice!(self.code, ident.start, ident.end))
            }

            Pattern::ArrayPattern(array) => self.build_from_array_pattern(array.as_ref()),

            Pattern::ObjectPattern(obj) => self.build_from_object_pattern(obj.as_ref()),

            Pattern::AssignmentPattern(ass) => todo!(),
        }
    }

    fn build_from_variable_declarator(&self, declarator: &VariableDeclarator<'a>) -> Command<'a> {
        todo!()
    }

    fn build_from_variable_declaration(
        &self,
        declaration: &VariableDeclaration<'a>,
    ) -> Command<'a> {
        let kind = declaration.kind.as_str();
        let mut array_cmd = bumpalo::vec![in self.arena; text!(kind), text!(" ")];

        let mut it = declaration.declarations.iter();

        array_cmd
            .push(self.build_from_variable_declarator(unsafe { it.next().unwrap_unchecked() }));

        let mut ident_cmd = Vec::new_in(self.arena);

        for declarator in it {
            ident_cmd.push(text!(","));
            ident_cmd.push(Hardline);
            ident_cmd.push(self.build_from_variable_declarator(declarator))
        }

        Array(array_cmd)
    }

    // fn build_assignment_exp(&self, exp: &AssignmentExpression<'a>) -> DocBuilder<'a> {
    //     todo!()
    // }

    fn build_from_identifier(&self, ident: &Identifier) -> Command<'a> {
        let text = slice!(self.code, ident.start, ident.end);
        text!(text)
    }

    fn build_from_numeric_literal(&self, literal: &NumericLiteral) -> Command<'a> {
        let text = slice!(self.code, literal.start, literal.end);
        text!(text)
    }

    fn build_from_string_literal(&self, literal: &StringLiteral) -> Command<'a> {
        let text = slice!(self.code, literal.start, literal.end);
        group!(self.arena, [text!("\""), text!(text), text!("\"")])
    }

    fn build_from_boolean_literal(&self, literal: &BooleanLiteral) -> Command<'a> {
        let text = slice!(self.code, literal.start, literal.end);
        text!(text)
    }

    fn build_from_null_literal(&self, literal: &NullLiteral) -> Command<'a> {
        let text = slice!(self.code, literal.start, literal.end);
        text!(text)
    }

    fn build_from_import(&self, exp: &Import) -> Command<'a> {
        todo!()
    }

    fn build_from_array_exp(&self, exp: &ArrayExpression<'a>) -> Command<'a> {
        let mut array_cmd = bumpalo::vec![in self.arena];

        let it = exp
            .elements
            .iter()
            .take((exp.elements.len() as isize - 1).max(0) as usize);

        for el in it {
            array_cmd.push(self.build_from_expression(el));
            array_cmd.push(text!(","));
            array_cmd.push(Line);
        }

        if let Some(last_el) = exp.elements.last() {
            array_cmd.push(self.build_from_expression(last_el));
        }

        group!(
            self.arena,
            [
                text!("["),
                indent!(self.arena, [Softline, Array(array_cmd)]),
                Softline,
                text!("]")
            ]
        )
    }

    pub fn build_from_obj_exp_property(
        &self,
        property_kind: &ObjectExpressionPropertyKind<'a>,
    ) -> Command<'a> {
        match property_kind {
            ObjectExpressionPropertyKind::Property(p) => {
                if p.computed {
                    array!(
                        self.arena,
                        [
                            text!("["),
                            self.build_from_expression(&p.key),
                            text!(": "),
                            self.build_from_expression(&p.value),
                            text!("]")
                        ]
                    )
                } else {
                    let mut array_cmd =
                        bumpalo::vec![in self.arena; self.build_from_expression(&p.key)];

                    if !p.shorthand {
                        array_cmd.push(text!(": "));
                        array_cmd.push(self.build_from_expression(&p.value));
                    }

                    Array(array_cmd)
                }
            }
            ObjectExpressionPropertyKind::SpreadElement(spread_el) => array!(
                self.arena,
                [
                    text!("..."),
                    self.build_from_expression(&spread_el.argument)
                ]
            ),
        }
    }

    fn build_from_object_exp(&self, exp: &ObjectExpression<'a>) -> Command<'a> {
        let mut array_cmd = bumpalo::vec![in self.arena];

        let it = exp
            .properties
            .iter()
            .take((exp.properties.len() as isize - 1).max(0) as usize);

        for property_kind in it {
            let cmd = self.build_from_obj_exp_property(property_kind);

            array_cmd.push(cmd);
            array_cmd.push(text!(","));
            array_cmd.push(Line);
        }

        if let Some(property_kind) = exp.properties.last() {
            array_cmd.push(self.build_from_obj_exp_property(property_kind));
        }

        group!(
            self.arena,
            [
                text!("{"),
                indent!(self.arena, [Softline, Array(array_cmd)]),
                Softline,
                text!("}")
            ]
        )
    }

    fn build_from_binary_exp(&self, exp: &BinaryExpression<'a>) -> Command<'a> {
        todo!()
    }
    
    fn build_from_expression(&self, exp: &Expression<'a>) -> Command<'a> {
        use Expression::*;

        match exp {
            Identifier(inner) => self.build_from_identifier(inner),
            NumericLiteral(inner) => self.build_from_numeric_literal(inner),
            StringLiteral(inner) => self.build_from_string_literal(inner),
            BooleanLiteral(inner) => self.build_from_boolean_literal(inner),
            NullLiteral(inner) => self.build_from_null_literal(inner),
            Import(inner) => self.build_from_import(inner),
            ArrayExpression(inner) => self.build_from_array_exp(inner),
            ObjectExpression(inner) => self.build_from_object_exp(inner),
            BinaryExpression(inner) => self.build_from_binary_exp(inner),
            _ => todo!(),
        }
    }

    fn build_from_statement(&self, statement: &Statement<'a>) -> Command<'a> {
        match statement {
            VariableDeclaration(vd) => self.build_from_variable_declaration(vd),
            ExpressionStatement(exp_stmt) => self.build_from_expression(&exp_stmt.exp),
            _ => todo!(),
        }
    }

    pub fn build(&self) -> Vec<'a, Command<'a>> {
        let mut doc = bumpalo::vec![in self.arena];

        for statement in self.ast.iter() {
            doc.push(self.build_from_statement(statement));
        }

        doc
    }

    pub fn new(arena: &'a Bump, code: &'a str, ast: Vec<'a, Statement<'a>>) -> Self {
        Self { arena, code, ast }
    }
}
