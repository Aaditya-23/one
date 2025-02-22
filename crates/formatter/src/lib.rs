// pub fn format(code: String, extension: String, options: doc::Options) -> String {
//     let doc = match extension.as_str() {
//         ".js" => javascript::build_doc(code, options),
//         "css" => panic!("Not implemented yet"),
//         "html" => panic!("Not implemented yet"),
//         _ => {
//             panic!("Unsupported file extension: {}", extension);
//         }
//     };

//     let formatted_code = doc::print(doc);

//     formatted_code
// }

// #[cfg(test)]
// mod tests {
//     use std::fs::{read_to_string, write};

//     use super::*;

//     #[test]
//     fn test_format() {
//         let code = read_to_string("./data/input.jsx").unwrap();
//         let extension = ".js";
//         let formatted_code = format(code, extension.to_string(), doc::Options::default());

//         write("./data/output.js", formatted_code).unwrap();
//     }
// }
