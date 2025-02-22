// use std::collections::HashMap;

// use javascript::{
//     nodes::{declarations::Declaration, statements::Statement, Body, DeclarationOperations, AST},
//     parser::Parser,
// };

// trait Rule {
//     fn check(&self);
//     fn fix(&self);
// }

// struct NoUnusedVars;


// pub fn lint(code: String) {
//     let ast = Parser::new(code).parse();
//     let uv = NoUnusedVars;
// }

// #[cfg(test)]
// mod tests {
//     use std::fs::read_to_string;

//     use super::*;

//     #[test]
//     fn test_linter() {
//         let code = read_to_string("../../data/input.js").unwrap();
//         lint(code);
//     }
// }
