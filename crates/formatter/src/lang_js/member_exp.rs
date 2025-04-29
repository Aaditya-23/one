use javascript::ast::MemberExpression;

use crate::{
    array, group, indent, lang_js::build_from_expression, softline, text, utils::commands::Command,
};





pub fn build_from_member_exp<'a>(code: &'a str, exp: &MemberExpression<'a>) -> Command<'a> {
    let mut array_cmd = vec![
        build_from_expression(code, &exp.object),
    ];

    let mut group_cmd = group!(vec![indent!(softline!())]);

    // if exp.computed {
    //     if exp.optional {
    //         indent_cmd.push(text!("?."));
    //     }

    //     indent_cmd.push(text!("["));
    //     indent_cmd.push(build_from_expression(code, &exp.property));
    //     indent_cmd.push(text!("]"));
    // } else {
    //     if exp.optional {
    //         indent_cmd.push(text!("?"));
    //     }
    //     indent_cmd.push(text!("."));
    //     indent_cmd.push(build_from_expression(code, &exp.property));
    // }

    array!(vec![
        build_from_expression(code, &exp.object),
    ])
}
