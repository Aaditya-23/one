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
    println!("ast: {:#?}", ast);
    let doc_builder = doc_js::Doc::new(code, ast);
    let mut doc = doc_builder.build();
    println!("doc: {:#?}", doc);

    let fmt_opts = FormatterOptions::default();
    let printer = Printer::new(fmt_opts);

    printer.print(&mut doc)
}

#[cfg(test)]
mod test {
    use bumpalo::collections::vec;
    use javascript::parser::Parser;
    use std::fs::{read_to_string, write};

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

    #[test]
    fn custom_format() {
        let mut doc = vec![
            text!("sumdi-"),
            self::line!(),
            text!("me"),
            group!(vec![text!("gumdi"), self::line!()]),
        ];

        let fmt_opts = FormatterOptions::default();
        let printer = Printer::new(fmt_opts);

        let clean_code = printer.print(&mut doc);
        write("../../data/output.js", clean_code).unwrap();
    }
}
