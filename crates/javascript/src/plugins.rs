use crate::{nodes::expressions::Expression, parser::Parser};

pub mod jsx;

#[derive(Debug, Clone)]
pub struct Plugins {
    pub jsx: bool,
    pub typescript: bool,
}

impl Plugins {
    pub fn default() -> Self {
        Self {
            jsx: true,
            typescript: true,
        }
    }
}

pub fn use_jsx(parser: &mut Parser) -> Expression {
    let mut plugin = jsx::JSX::new(parser);
    Expression::JSXElement(plugin.parse())
}