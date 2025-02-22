use std::{fs::read_to_string, time::Instant};

use javascript::tokenizer::{Token, Tokenizer};

fn main(){
    //  let start = Instant::now();

    //     let code = read_to_string("./data/input.js").unwrap();
    //     let mut tokenizer = Tokenizer::new(code);

    //     loop {
    //         let result = tokenizer.get_next_token();
    //         match result {
    //             Ok(token) => {
    //                 if let Token::EOF = token.type_ {
    //                     break;
    //                 }
    //             }
    //             Err(error) => {
    //                 break;
    //             }
    //         }
    //     }

    //     let end = start.elapsed();
    //     println!("{}", end.as_millis());
}