use bumpalo::{collections::Vec, Bump};
use javascript::ast::Statement;

mod commands;
mod doc_js;
mod formatter;

pub fn format_js<'a>(arena: &'a Bump, code: &'a str, ast: Vec<'a, Statement<'a>>) {
   let mut doc_builder: doc_js::Doc<'_> = doc_js::Doc::new(arena, code, ast);
   let doc = doc_builder.build();
}

#[cfg(test)]
mod test {
    use std::fs::read_to_string;

    use javascript::parser::Parser;

    use super::*;

    #[test]
    fn test_format() {
        let arena = Bump::new();
        let code = read_to_string("../../data/input.js").unwrap();
        let mut p = Parser::new(&arena, code.as_str());

        let ast = p.parse();

        format_js(&arena, code.as_str(), ast);
    }
}
