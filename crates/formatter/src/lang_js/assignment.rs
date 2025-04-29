use javascript::ast::{AssignmentExpression, AssignmentExpressionLHS, VariableDeclaration, VariableDeclarator};

use crate::{
    array, group, hardline, indent, join,
    lang_js::{build_from_expression, pattern::build_from_pattern},
    line, slice, softline, text,
    utils::commands::Command,
};

pub fn build_from_ass_exp<'a>(code: &'a str, exp: &AssignmentExpression<'a>) -> Command<'a> {
    let left = match &exp.left {
        AssignmentExpressionLHS::Pattern(p) => build_from_pattern(code, p),
        AssignmentExpressionLHS::Expression(exp) => build_from_expression(code, exp),
    };

    let right = build_from_expression(code, &exp.right);

    array!(vec![
        left,
        text!(" "),
        text!(exp.operator.as_str()),
        line!(),
        right
    ])
}

pub fn build_from_variable_declarator<'a>(
    code: &'a str,
    declarator: &VariableDeclarator<'a>,
) -> Command<'a> {
    let pattern = build_from_pattern(code, &declarator.id);

    if let Some(exp) = &declarator.init {
        group!(vec![
            pattern,
            text![" ="],
            indent!(vec![line!(), build_from_expression(code, exp)])
        ])
    } else {
        group!(vec![pattern])
    }
}

pub fn build_from_variable_declaration<'a>(
    code: &'a str,
    declaration: &VariableDeclaration<'a>,
) -> Command<'a> {
    let kind = declaration.kind.as_str();
    let mut array_cmd = vec![text!(kind), text!(" ")];

    let join_separator = indent!(vec![text!(","), hardline!()]);
    let mut join_cmds = vec![];

    for declarator in declaration.declarations.iter() {
        join_cmds.push(build_from_variable_declarator(code, declarator))
    }

    array_cmd.push(join!(join_separator, join_cmds));
    array!(array_cmd)
}
