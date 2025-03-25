use bumpalo::{collections::Vec, Bump};
use javascript::ast::{
    ArrayExpression, ArrayPattern, ArrowFunction, ArrowFunctionBody, AssignmentExpression,
    AssignmentExpressionLHS, AwaitExpression, BinaryExpression, BooleanLiteral, CallExpression,
    Class, ConditionalExpression,
    Expression::{self, *},
    Function, FunctionParams, Identifier, Import, LogicalExpression, MemberExpression,
    NewExpression, NullLiteral, NumericLiteral, ObjectExpression, ObjectExpressionPropertyKind,
    ObjectPattern, ObjectPatternPropertyKind, Pattern, Regexp, SequenceExpression,
    Statement::{self, *},
    StringLiteral, TemplateLiteral, ThisExpression, UnaryExpression, UpdateExpression,
    VariableDeclaration, VariableDeclarator,
};

use crate::{
    array, break_parent, group, hardline, indent, line, softline, text,
    utils::commands::{
        Command::{self, *},
        *,
    },
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
            array_cmd.push(line!());
        }

        if let Some(last_el) = pattern.elements.last() {
            array_cmd.push(self.build_from_pattern(last_el));
        }

        group!(
            self.arena,
            [
                text!("["),
                indent!(self.arena, [softline!(), array!(array_cmd)]),
                softline!(),
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

                    (array!(array_cmd), add_hardline)
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
                array_cmd.push(hardline!());
            } else {
                array_cmd.push(line!());
            }
        }

        let mut softline_or_hardline = softline!();

        if let Some(property_kind) = pattern.properties.last() {
            let (cmd, add_hardline) = self.build_from_obj_pattern_property(property_kind);

            array_cmd.push(cmd);

            if add_hardline {
                softline_or_hardline = hardline!();
            }
        }

        group!(
            self.arena,
            [
                text!("{"),
                indent!(self.arena, [softline!(), array!(array_cmd)]),
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
            ident_cmd.push(hardline!());
            ident_cmd.push(self.build_from_variable_declarator(declarator))
        }

        array!(array_cmd)
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
            array_cmd.push(line!());
        }

        if let Some(last_el) = exp.elements.last() {
            array_cmd.push(self.build_from_expression(last_el));
        }

        group!(
            self.arena,
            [
                text!("["),
                indent!(self.arena, [softline!(), array!(array_cmd)]),
                softline!(),
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

                    array!(array_cmd)
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
            array_cmd.push(line!());
        }

        if let Some(property_kind) = exp.properties.last() {
            array_cmd.push(self.build_from_obj_exp_property(property_kind));
        }

        group!(
            self.arena,
            [
                text!("{"),
                indent!(self.arena, [softline!(), array!(array_cmd)]),
                softline!(),
                text!("}")
            ]
        )
    }

    fn build_from_binary_exp(&self, exp: &BinaryExpression<'a>) -> Command<'a> {
        let left = self.build_from_expression(&exp.left);
        let right = self.build_from_expression(&exp.right);

        group!(
            self.arena,
            [
                left,
                softline!(),
                text!(exp.operator.as_str()),
                line!(),
                right
            ]
        )
    }

    fn build_from_logical_exp(&self, exp: &LogicalExpression<'a>) -> Command<'a> {
        let left = self.build_from_expression(&exp.left);
        let right = self.build_from_expression(&exp.right);

        group!(
            self.arena,
            [
                left,
                softline!(),
                text!(exp.operator.as_str()),
                line!(),
                right
            ]
        )
    }

    fn build_from_ass_exp(&self, exp: &AssignmentExpression<'a>) -> Command<'a> {
        let left = match &exp.left {
            AssignmentExpressionLHS::Pattern(p) => self.build_from_pattern(p),
            AssignmentExpressionLHS::Expression(exp) => self.build_from_expression(exp),
        };

        let right = self.build_from_expression(&exp.right);

        array!(
            self.arena,
            [
                left,
                text!(" "),
                text!(exp.operator.as_str()),
                line!(),
                right
            ]
        )
    }

    fn build_from_update_exp(&self, exp: &UpdateExpression<'a>) -> Command<'a> {
        let mut array_cmd = bumpalo::vec![in self.arena];
        let arg = self.build_from_expression(&exp.argument);

        if exp.prefix {
            array_cmd.push(text!(exp.operator.as_str()));
            array_cmd.push(arg)
        } else {
            array_cmd.push(arg);
            array_cmd.push(text!(exp.operator.as_str()))
        }

        array!(array_cmd)
    }

    fn build_from_unary_exp(&self, exp: &UnaryExpression<'a>) -> Command<'a> {
        let arg = self.build_from_expression(&exp.argument);
        array!(self.arena, [text!(exp.operator.as_str()), arg])
    }

    fn build_from_conditional_exp(&self, exp: &ConditionalExpression<'a>) -> Command<'a> {
        let test = self.build_from_expression(&exp.test);
        let consequent = self.build_from_expression(&exp.consequent);
        let alternate = self.build_from_expression(&exp.alternate);

        array!(
            self.arena,
            [
                test,
                line!(),
                text!("? "),
                consequent,
                line!(),
                text!(": "),
                alternate
            ]
        )
    }

    fn build_from_this_exp(&self, _: &ThisExpression) -> Command<'a> {
        array!(self.arena, [text!("this")])
    }

    fn build_from_new_exp(&self, exp: &NewExpression<'a>) -> Command<'a> {
        let callee = self.build_from_expression(&exp.callee);

        array!(self.arena, [text!("new "), callee, text!("()")])
    }

    fn build_from_function_params(&self, params: &Vec<'a, FunctionParams<'a>>) -> Command<'a> {
        let mut indent_cmd = bumpalo::vec![in self.arena];
        indent_cmd.push(softline!());

        let it = params
            .iter()
            .take((params.len() as isize - 1).max(0) as usize);

        let from_array_pattern = |param: &FunctionParams<'a>| -> Command<'a> {
            match param {
                FunctionParams::Pattern(p) => self.build_from_pattern(p),
                FunctionParams::RestElement(rest_el) => {
                    array!(
                        self.arena,
                        [text!("..."), self.build_from_pattern(&rest_el.argument)]
                    )
                }
            }
        };

        for param in it {
            indent_cmd.push(from_array_pattern(param));
            indent_cmd.push(text!(","));
            indent_cmd.push(line!());
        }

        if let Some(last_param) = params.last() {
            indent_cmd.push(from_array_pattern(last_param));
        }

        array!(
            self.arena,
            [text!("("), indent!(indent_cmd), softline!(), text!(")")]
        )
    }

    fn build_from_function(&self, exp: &Function<'a>) -> Command<'a> {
        let mut array_cmd = bumpalo::vec![in self.arena];

        if exp.async_ {
            array_cmd.push(text!("async "));
        }

        if let Some(id) = &exp.id {
            array_cmd.push(self.build_from_identifier(id));
        }

        if exp.generator {
            array_cmd.push(text!("*"));
        }

        array_cmd.push(text!(" "));

        let params = self.build_from_function_params(&exp.params);
        array_cmd.push(params);

        array_cmd.push(text!(" {"));
        array_cmd.push(hardline!());

        for statement in exp.body.body.iter() {
            array_cmd.push(self.build_from_statement(statement));
            array_cmd.push(hardline!());
        }

        array_cmd.push(text!("}"));

        array!(array_cmd)
    }

    fn build_from_arrow_function(&self, exp: &ArrowFunction<'a>) -> Command<'a> {
        let mut array_cmd = bumpalo::vec![in self.arena];

        if exp.async_ {
            array_cmd.push(text!("async "));
        }

        let params = self.build_from_function_params(&exp.params);
        array_cmd.push(params);

        array_cmd.push(text!(" => "));

        array_cmd.push(text!("{"));
        array_cmd.push(hardline!());

        match &exp.body {
            ArrowFunctionBody::Expression(exp) => {
                array_cmd.push(self.build_from_expression(exp));
                array_cmd.push(hardline!());
            }
            ArrowFunctionBody::Block(exp) => {
                for statement in exp.body.iter() {
                    array_cmd.push(self.build_from_statement(statement));
                    array_cmd.push(hardline!());
                }
            }
        }

        array_cmd.push(text!("}"));

        array!(array_cmd)
    }

    fn build_from_class(&self, exp: &Class<'a>) -> Command<'a> {
        todo!()
    }

    fn build_from_func_arguments(&self, args: &Vec<'a, Expression<'a>>) -> Command<'a> {
        let mut indent_cmd = bumpalo::vec![in self.arena];
        indent_cmd.push(softline!());

        let it = args.iter().take((args.len() as isize - 1).max(0) as usize);

        for arg in it {
            indent_cmd.push(self.build_from_expression(arg));
            indent_cmd.push(text!(","));
            indent_cmd.push(line!());
        }

        if let Some(last_arg) = args.last() {
            indent_cmd.push(self.build_from_expression(last_arg));
        }

        indent!(indent_cmd)
    }

    fn build_from_call_exp(&self, exp: &CallExpression<'a>) -> Command<'a> {
        let callee = slice!(self.code, exp.start, exp.end);
        let mut array_cmd = bumpalo::vec![in self.arena; text!(callee)];

        if exp.optional {
            array_cmd.push(text!("?."));
        }

        array_cmd.push(text!(" ("));

        let args = self.build_from_func_arguments(&exp.arguments);
        array_cmd.push(args);

        array_cmd.push(softline!());
        array_cmd.push(text!(")"));

        array!(array_cmd)
    }

    fn build_from_member_exp(&self, exp: &MemberExpression<'a>) -> Command<'a> {
        let mut array_cmd = bumpalo::vec![in self.arena];

        array_cmd.push(self.build_from_expression(&exp.object));

        if exp.computed {
            if exp.optional {
                array_cmd.push(text!("?."));
            }

            array_cmd.push(text!("["));
            array_cmd.push(self.build_from_expression(&exp.property));
            array_cmd.push(text!("]"));
        } else {
            if exp.optional {
                array_cmd.push(text!("?"));
            }
            array_cmd.push(text!("."));
            array_cmd.push(self.build_from_expression(&exp.property));
        }

        array!(array_cmd)
    }

    fn build_from_sequence_exp(&self, exp: &SequenceExpression<'a>) -> Command<'a> {
        let mut indent_cmd = bumpalo::vec![in self.arena];
        indent_cmd.push(softline!());

        let it = exp.expressions.iter().take(
            (exp.expressions.len() as isize - 1)
                .max(0)
                .min(exp.expressions.len() as isize - 1) as usize,
        );

        for exp in it {
            indent_cmd.push(self.build_from_expression(exp));
            indent_cmd.push(text!(","));
            indent_cmd.push(line!());
        }

        if let Some(last_exp) = exp.expressions.last() {
            indent_cmd.push(self.build_from_expression(last_exp));
        }

        array!(
            self.arena,
            [text!("("), indent!(indent_cmd), softline!(), text!(")")]
        )
    }

    fn build_from_regular_exp(&self, exp: &Regexp<'a>) -> Command<'a> {
        todo!()
    }

    fn build_from_template_literal(&self, exp: &TemplateLiteral<'a>) -> Command<'a> {
        let mut array_cmd = bumpalo::vec![in self.arena];

        array_cmd.push(text!("`"));

        let mut exp_it = exp.expressions.iter();

        for quasi in exp.quasis.iter() {
            let text = slice!(self.code, quasi.start, quasi.end);
            array_cmd.push(text!(text));

            if let Some(exp) = exp_it.next() {
                array_cmd.push(text!("${"));
                array_cmd.push(self.build_from_expression(exp));
                array_cmd.push(text!("}"));
            }
        }

        array_cmd.push(text!("`"));

        array!(array_cmd)
    }

    fn build_from_await_exp(&self, exp: &AwaitExpression<'a>) -> Command<'a> {
        let arg = self.build_from_expression(&exp.argument);

        array!(self.arena, [text!("await"), arg])
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
            LogicalExpression(inner) => self.build_from_logical_exp(inner),
            AssignmentExpression(inner) => self.build_from_ass_exp(inner),
            UpdateExpression(inner) => self.build_from_update_exp(inner),
            UnaryExpression(inner) => self.build_from_unary_exp(inner),
            ConditionalExpression(inner) => self.build_from_conditional_exp(inner),
            ThisExpression(inner) => self.build_from_this_exp(inner),
            NewExpression(inner) => self.build_from_new_exp(inner),
            FunctionExpression(inner) => self.build_from_function(&inner),
            ArrowFunctionExpression(inner) => self.build_from_arrow_function(inner),
            ClassExpression(inner) => self.build_from_class(inner),
            CallExpression(inner) => self.build_from_call_exp(&inner),
            MemberExpression(inner) => self.build_from_member_exp(inner),
            SequenceExpression(inner) => self.build_from_sequence_exp(inner),
            RegularExpression(inner) => self.build_from_regular_exp(inner),
            TemplateLiteral(inner) => self.build_from_template_literal(inner),
            AwaitExpression(inner) => self.build_from_await_exp(inner),
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
