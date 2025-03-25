use bumpalo::{collections::Vec, Bump};
use javascript::ast::Statement;
use utils::{
    commands::*,
    printer::{FormatterOptions, Printer},
};

mod doc_js;
mod formatter;
mod utils;

pub fn format_js<'a>(arena: &'a Bump, code: &'a str, ast: Vec<'a, Statement<'a>>) -> String {
    let doc_builder: doc_js::Doc<'a> = doc_js::Doc::new(arena, code, ast);
    let mut doc: Vec<'a, Command<'a>> = doc_builder.build();

    let fmt_opts = FormatterOptions::default();
    let mut printer: Printer<'a> = Printer::new(arena, fmt_opts);
    printer.print(&mut doc)
}

#[cfg(test)]
mod test {
    use std::fs::{read_to_string, write};

    use javascript::parser::Parser;

    use super::*;

    #[test]
    fn test_format() {
        let arena = Bump::new();
        let code = read_to_string("../../data/input.js").unwrap();
        let mut parser = Parser::new(&arena, &code);
        let ast = parser.parse();
        
        let clean_code = format_js(&arena, &code, ast);

        let result = write("../../data/output.js", clean_code);

        if let Err(err) = result {
            println!("An error occurred: {}", err);
        }
    }
}
