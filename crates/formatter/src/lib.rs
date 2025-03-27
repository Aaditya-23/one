use bumpalo::{collections::Vec, Bump};
use javascript::ast::Statement;
use utils::{
    commands::*,
    printer::{FormatterOptions, Printer},
};

mod doc_js;
mod formatter;
mod utils;

pub fn format_js<'a>(code: &'a str, ast: Vec<'a, Statement<'a>>) -> String {
    let doc_builder = doc_js::Doc::new(code, ast);
    let mut doc = doc_builder.build();

    let fmt_opts = FormatterOptions::default();
    let printer = Printer::new(fmt_opts);
    
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

        let clean_code = format_js(&code, ast);

        let result = write("../../data/output.js", clean_code);

        if let Err(err) = result {
            println!("An error occurred: {}", err);
        }
    }
}
