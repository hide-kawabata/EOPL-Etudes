#[macro_use] extern crate lalrpop_util;

//lalrpop_mod!(pub lexer);
mod tests;
mod word;
mod parser;
mod check;
mod asmgen;
// mod cgen;
mod ast;

use crate::parser::parse;
use crate::check::check;
// use crate::cgen::cgen;
use crate::asmgen::asmgen;

fn main() {
    println!("Hello, world!");
//    get_tokens();
    let l = parse();
    println!("(main) parse OK.");
    println!("(main) tree={:#?}", l);
    check(&l);
    println!("(main) check OK.");
    asmgen(&l);
    // cgen(&l);
    println!("(main) asmgen OK.");
    // println!("(main) cgen OK.");
}


