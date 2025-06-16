use std::ops::Range;

use crate::kind::Kind;

// pub mod nodes;
mod allocator;
pub mod ast;
pub mod kind;
mod lookup_table;
pub mod parser;
pub mod tokenizer;
mod typescript;
mod jsx;

#[derive(Debug, Clone)]
pub enum DiagnosticError<'a> {
    UnexpectedToken { expected: Kind, found: Kind },
    Other(&'a str),
}

#[derive(Debug, Clone)]
pub struct ParserDiagnostics<'a> {
    pub error: DiagnosticError<'a>,
    pub position: Range<usize>,
}

// pub fn build_doc(code: String, options: Options) -> DocTree {
//     doc_builder::build(code, options)
// }
