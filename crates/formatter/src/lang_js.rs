mod array;
mod assignment;
mod class;
mod function;
mod import;
mod literal;
mod member_exp;
mod object;
mod pattern;

use bumpalo::collections::Vec as BumpVec;
use javascript::ast::{
    ArrayExpression, ArrayPattern, ArrayPatternKind, ArrowFunction, ArrowFunctionBody,
    AssignmentExpression, AssignmentExpressionLHS, AwaitExpression, BinaryExpression,
    BooleanLiteral, CallExpression, Class, ConditionalExpression,
    Expression::{self, *},
    Function, FunctionParams, Identifier, Import, LogicalExpression, MemberExpression,
    NewExpression, NullLiteral, NumericLiteral, ObjectExpression, ObjectExpressionPropertyKind,
    ObjectPattern, ObjectPatternPropertyKind, Pattern, Regexp, SequenceExpression,
    Statement::{self, *},
    StringLiteral, TemplateLiteral, ThisExpression, UnaryExpression, UpdateExpression,
    VariableDeclaration, VariableDeclarator,
};

use crate::{
    array, break_parent, group, hardline, indent, join, line, softline, text,
    utils::commands::{
        Command::{self, *},
        *,
    },
};

pub fn build_from_expression<'a>(code: &'a str, exp: &Expression<'a>) -> Command<'a> {
    use Expression::*;

    match exp {
        Identifier(inner) => build_from_identifier(inner),
        NumericLiteral(inner) => build_from_numeric_literal(inner),
        StringLiteral(inner) => build_from_string_literal(inner),
        BooleanLiteral(inner) => build_from_boolean_literal(inner),
        NullLiteral(inner) => build_from_null_literal(inner),
        Import(inner) => build_from_import(inner),
        ArrayExpression(inner) => build_from_array_exp(inner),
        ObjectExpression(inner) => build_from_object_exp(inner),
        BinaryExpression(inner) => build_from_binary_exp(inner),
        LogicalExpression(inner) => build_from_logical_exp(inner),
        AssignmentExpression(inner) => build_from_ass_exp(inner),
        UpdateExpression(inner) => build_from_update_exp(inner),
        UnaryExpression(inner) => build_from_unary_exp(inner),
        ConditionalExpression(inner) => build_from_conditional_exp(inner),
        ThisExpression(inner) => build_from_this_exp(inner),
        NewExpression(inner) => build_from_new_exp(inner),
        FunctionExpression(inner) => build_from_function(&inner),
        ArrowFunctionExpression(inner) => build_from_arrow_function(inner),
        ClassExpression(inner) => build_from_class(inner),
        CallExpression(inner) => build_from_call_exp(&inner),
        MemberExpression(inner) => build_from_member_exp(inner),
        SequenceExpression(inner) => build_from_sequence_exp(inner),
        RegularExpression(inner) => build_from_regular_exp(inner),
        TemplateLiteral(inner) => build_from_template_literal(inner),
        AwaitExpression(inner) => build_from_await_exp(inner),
        _ => todo!(),
    }
}

//     if exp.prefix {
//         array_cmd.push(text!(exp.operator.as_str()));
//         array_cmd.push(arg)
//     } else {
//         array_cmd.push(arg);
//         array_cmd.push(text!(exp.operator.as_str()))
//     }

//     array!(array_cmd)
// }

// fn build_from_unary_exp(&self, exp: &UnaryExpression<'a>) -> Command<'a> {
//     let arg = self.build_from_expression(&exp.argument);
//     array!(vec![text!(exp.operator.as_str()), arg])
// }

// fn build_from_conditional_exp(&self, exp: &ConditionalExpression<'a>) -> Command<'a> {
//     let test = self.build_from_expression(&exp.test);
//     let consequent = self.build_from_expression(&exp.consequent);
//     let alternate = self.build_from_expression(&exp.alternate);

//     array!(vec![
//         test,
//         line!(),
//         text!("? "),
//         consequent,
//         line!(),
//         text!(": "),
//         alternate
//     ])
// }

// fn build_from_this_exp(&self, _: &ThisExpression) -> Command<'a> {
//     text!("this")
// }

// fn build_from_new_exp(&self, exp: &NewExpression<'a>) -> Command<'a> {
//     let callee = self.build_from_expression(&exp.callee);

//     array!(vec![text!("new "), callee, text!("()")])
// }

// fn build_from_class(&self, exp: &Class<'a>) -> Command<'a> {
//     todo!()
// }

// fn build_from_func_arguments(&self, args: &BumpVec<'a, Expression<'a>>) -> Command<'a> {
//     let mut indent_cmd = vec![];
//     indent_cmd.push(softline!());

//     let it = args.iter().take((args.len() as isize - 1).max(0) as usize);

//     for arg in it {
//         indent_cmd.push(self.build_from_expression(arg));
//         indent_cmd.push(text!(","));
//         indent_cmd.push(line!());
//     }

//     if let Some(last_arg) = args.last() {
//         indent_cmd.push(self.build_from_expression(last_arg));
//     }

//     indent!(indent_cmd)
// }

// fn build_from_call_exp(&self, exp: &CallExpression<'a>) -> Command<'a> {
//     let callee = slice!(self.code, exp.start, exp.end);
//     let mut array_cmd = vec![text!(callee)];

//     if exp.optional {
//         array_cmd.push(text!("?."));
//     }

//     array_cmd.push(text!(" ("));

//     let args = self.build_from_func_arguments(&exp.arguments);
//     array_cmd.push(args);

//     array_cmd.push(softline!());
//     array_cmd.push(text!(")"));

//     array!(array_cmd)
// }

// fn build_from_sequence_exp(&self, exp: &SequenceExpression<'a>) -> Command<'a> {
//     let mut indent_cmd = vec![];
//     indent_cmd.push(softline!());

//     let it = exp.expressions.iter().take(
//         (exp.expressions.len() as isize - 1)
//             .max(0)
//             .min(exp.expressions.len() as isize - 1) as usize,
//     );

//     for exp in it {
//         indent_cmd.push(self.build_from_expression(exp));
//         indent_cmd.push(text!(","));
//         indent_cmd.push(line!());
//     }

//     if let Some(last_exp) = exp.expressions.last() {
//         indent_cmd.push(self.build_from_expression(last_exp));
//     }

//     array!(vec![
//         text!("("),
//         indent!(indent_cmd),
//         softline!(),
//         text!(")")
//     ])
// }

// fn build_from_regular_exp(&self, exp: &Regexp<'a>) -> Command<'a> {
//     array!(vec![
//         text!("/"),
//         text!(exp.pattern),
//         text!("/"),
//         text!(exp.flag)
//     ])
// }

// fn build_from_template_literal(&self, exp: &TemplateLiteral<'a>) -> Command<'a> {
//     let mut array_cmd = vec![];

//     array_cmd.push(text!("`"));

//     let mut exp_it = exp.expressions.iter();

//     for quasi in exp.quasis.iter() {
//         let text = slice!(self.code, quasi.start, quasi.end);
//         array_cmd.push(text!(text));

//         if let Some(exp) = exp_it.next() {
//             array_cmd.push(text!("${"));
//             array_cmd.push(self.build_from_expression(exp));
//             array_cmd.push(text!("}"));
//         }
//     }

//     array_cmd.push(text!("`"));

//     array!(array_cmd)
// }

// fn build_from_await_exp(&self, exp: &AwaitExpression<'a>) -> Command<'a> {
//     let arg = self.build_from_expression(&exp.argument);

//     array!(vec![text!("await"), arg])
// }

fn build_from_statement<'a>(code: &'a str, statement: &Statement<'a>) -> Command<'a> {
    match statement {
        // VariableDeclaration(vd) => build_from_variable_declaration(vd),
        // ExpressionStatement(exp_stmt) => build_from_expression(&exp_stmt.exp),
        // FunctionDeclaration(function) => build_from_function(function),
        _ => todo!(),
    }
}

pub fn build<'a>(code: &'a str, ast: BumpVec<'a, Statement<'a>>) -> Vec<Command<'a>> {
    let mut doc = vec![];

    for statement in ast.iter() {
        doc.push(build_from_statement(code, statement));
    }

    doc
}
