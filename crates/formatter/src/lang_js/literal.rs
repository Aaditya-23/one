use javascript::ast::{BooleanLiteral, Identifier, NullLiteral, NumericLiteral, StringLiteral};

use crate::{group, slice, text, utils::commands::Command};

fn build_from_identifier<'a>(code: &'a str, ident: &Identifier) -> Command<'a> {
    let text = slice!(code, ident.start, ident.end);
    text!(text)
}

fn build_from_numeric_literal<'a>(code: &'a str, literal: &NumericLiteral) -> Command<'a> {
    let text = slice!(code, literal.start, literal.end);
    text!(text)
}

fn build_from_string_literal<'a>(code: &'a str, literal: &StringLiteral) -> Command<'a> {
    let text = slice!(code, literal.start, literal.end);
    group!(vec![text!("\""), text!(text), text!("\"")])
}

fn build_from_boolean_literal<'a>(code: &'a str, literal: &BooleanLiteral) -> Command<'a> {
    let text = slice!(code, literal.start, literal.end);
    text!(text)
}

fn build_from_null_literal<'a>(code: &'a str, literal: &NullLiteral) -> Command<'a> {
    let text = slice!(code, literal.start, literal.end);
    text!(text)
}
