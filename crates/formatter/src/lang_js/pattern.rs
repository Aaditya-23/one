use javascript::ast::{
    ArrayPattern, ArrayPatternKind, ObjectPattern, ObjectPatternPropertyKind, Pattern,
};

use crate::{array, break_parent, group, hardline, indent, join, line, slice, softline};
use crate::{text, utils::commands::Command};

use super::build_from_expression;


fn build_from_array_pattern<'a>(code: &'a str, pattern: &ArrayPattern<'a>) -> Command<'a> {
    let join_separator = array!(vec![text!(","), line!()]);
    let mut join_cmds = vec![];

    for el in pattern.elements.iter() {
        match el {
            Some(inner) => match inner {
                ArrayPatternKind::Pattern(inner) => join_cmds.push(build_from_pattern(code, inner)),
                ArrayPatternKind::RestElement(inner) => join_cmds.push(array!(vec![
                    text!("..."),
                    build_from_pattern(code, &inner.argument)
                ])),
            },
            None => join_cmds.push(text!("")),
        }
    }

    group!(
        vec![
            text!("["),
            indent!(vec![softline!(), join!(join_separator, join_cmds)]),
            softline!(),
            text!("]")
        ],
        false,
        "b"
    )
}

fn build_from_obj_pattern_property<'a>(
    code: &'a str,
    property_kind: &ObjectPatternPropertyKind<'a>,
) -> (Command<'a>, bool) {
    match property_kind {
        ObjectPatternPropertyKind::Property(p) => {
            if p.computed {
                (
                    array!(vec![
                        text!("["),
                        build_from_expression(code, &p.key),
                        text!(": "),
                        build_from_pattern(code, &p.value),
                        text!("]")
                    ]),
                    false,
                )
            } else {
                let mut array_cmd = vec![build_from_expression(code, &p.key)];

                let should_break;

                if !p.shorthand {
                    array_cmd.push(text!(": "));
                    array_cmd.push(build_from_pattern(code, &p.value));

                    if let Pattern::ObjectPattern(_) = p.value {
                        should_break = true
                    } else {
                        should_break = false
                    }
                } else {
                    should_break = false
                }

                (array!(array_cmd), should_break)
            }
        }
        ObjectPatternPropertyKind::RestElement(rest_el) => (
            array!(vec![
                text!("..."),
                build_from_pattern(code, &rest_el.argument)
            ]),
            false,
        ),
    }
}

fn build_from_object_pattern<'a>(code: &'a str, pattern: &ObjectPattern<'a>) -> Command<'a> {
    let join_separator = array!(vec![text!(","), line!()]);
    let mut join_cmds = vec![];
    let mut should_break = false;

    for property_kind in pattern.properties.iter() {
        let (cmd, break_group) = build_from_obj_pattern_property(code, property_kind);

        if break_group {
            should_break = true;
        }

        join_cmds.push(cmd);
    }

    group!(
        vec![
            text!("{"),
            indent!(vec![line!(), join!(join_separator, join_cmds)]),
            line!(),
            text!("}")
        ],
        should_break
    )
}

pub fn build_from_pattern<'a>(code: &'a str, pattern: &Pattern<'a>) -> Command<'a> {
    match pattern {
        Pattern::IdentifierPattern(ident) => {
            text!(slice!(code, ident.start, ident.end))
        }

        Pattern::ArrayPattern(array) => build_from_array_pattern(code, &array),

        Pattern::ObjectPattern(obj) => build_from_object_pattern(code, &obj),

        Pattern::AssignmentPattern(ass) => todo!(),
    }
}
