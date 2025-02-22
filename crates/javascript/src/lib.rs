// mod doc_builder;
// pub mod nodes;
pub mod parser;
// mod plugins;
pub mod tokenizer;
mod lookup_table;
mod kind;
mod ast;
mod allocator;

use doc::{DocTree, Options};

// pub fn build_doc(code: String, options: Options) -> DocTree {
//     doc_builder::build(code, options)
// }
