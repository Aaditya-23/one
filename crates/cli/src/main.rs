use std::env::args;

fn main() {

    let args: Vec<String> = args().collect();
    println!("--------");
    println!("{:?}", args);
    println!("--------");
}
