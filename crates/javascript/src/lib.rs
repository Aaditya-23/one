use std::ops::Range;

use crate::kind::Kind;

// pub mod nodes;
mod allocator;
pub mod ast;
mod jsx;
pub mod kind;
mod lookup_table;
pub mod parser;
pub mod tokenizer;
mod typescript;

#[derive(Debug, Clone)]
pub enum DiagnosticError<'a> {
    UnexpectedToken { expected: Kind, found: Kind },
    Other(&'a str),
}

#[derive(Debug, Clone)]
pub struct ParserDiagnostics<'a> {
    pub error: DiagnosticError<'a>,
    pub position: Range<usize>,
    pub line_number: u32,
}

// pub fn build_doc(code: String, options: Options) -> DocTree {
//     doc_builder::build(code, options)
// }
