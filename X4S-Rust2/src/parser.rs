//#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub lexer);

use std::rc::Rc;
use std::cell::RefCell;
use crate::word::Word;
use crate::word::Word::*; // Word::Empty -> Empty
use crate::ast::{L_t, C_t, E_t, Id_t};


thread_local!{
    static VBUF: RefCell<Vec<Word>> = RefCell::new(Vec::new());
    static LASTTK: RefCell<Rc<Word>> = RefCell::new(Rc::new(Empty));
}

pub fn parse() -> Box<L_t> {
    let t = parse_L();
    if *nexttoken2() != EOF {
        panic!("(parse) Syntax Error!");
    };
    t
}

fn parse_L() -> Box<L_t> {
    match *nexttoken2() {
        EOF => {
            Box::new(L_t::EPS)
        },
        Print => {
            let c = parse_C();
            let l = parse_L();
            Box::new(L_t::CL(c, l))
        },
        Def => {
            let c = parse_C();
            let l = parse_L();
            Box::new(L_t::CL(c, l))
        },
        _ => {
            panic!("(parse_L) Syntax Error!");
        }
    }
}

fn parse_C() -> Box<C_t> {
    match *nexttoken2() {
        Print => {
            eat_token();
            let e = parse_E();
            match *nexttoken2() {
                Semic => { eat_token(); Box::new(C_t::Print(e))},
                _ => { panic!("(parse_C Print) Syntax Error!"); }
            }
        },
        Def => {
            eat_token();
            match &*nexttoken2() {
                Id(s) => {
                    eat_token();
                    let e = parse_E();
                    match *nexttoken2() {
                        Semic => { eat_token(); Box::new(C_t::Def(
                            Box::new(Id_t{name:s.to_string()}), e))},
                        _ => { panic!("(parse_C, Def) Syntax Error!"); }
                    }
                },
                _ => { panic!("(parse_C, Def) Syntax Error!"); }
            }
        },
        _ => {
            panic!("(parse_C) Syntax Error!");
        }
    }
}

fn parse_E() -> Box::<E_t> {
    match &*nexttoken2() {
        Id(s) => {
            eat_token();
            Box::new(E_t::Var(Box::new(Id_t{name:s.to_string()})))
        },
        Num(n) => {
            eat_token();
            Box::new(E_t::Num(n.parse().unwrap()))
        },
        Add => {
            eat_token();
            let e1 = parse_E();
            let e2 = parse_E();
            Box::new(E_t::Add(e1, e2))
        },
        Sub => {
            eat_token();
            let e1 = parse_E();
            let e2 = parse_E();
            Box::new(E_t::Sub(e1, e2))
        },
        _ => {
            panic!("(parse_E) Syntax Error!");
        }
    }
}

fn get_tokens() {
//    let mut token_buf: Vec<Word> = Vec::new();
    loop {
//        let tk = yylex(&mut token_buf);
        let tk = nexttoken();
        println!("tk={:#?}", tk);
        if tk == EOF { // EOF
            break;
        }
    }
}

fn eat_token() {
    LASTTK.with(|last_tk| {
        *last_tk.borrow_mut() = Rc::new(Empty);
    });
}

/*
/* Ver.1 */
fn nexttoken2() -> Rc<Word> {
    let mut w: Rc<Word> = Rc::new(Dummy);
    LASTTK.with(|last_tk| {
        w = Rc::clone(&*last_tk.borrow());
        if *w == Empty {
            w = Rc::new(nexttoken());
            *last_tk.borrow_mut() = Rc::clone(&w);
        }
    });
    println!("(nexttoken2) return {:?}", w);
    w
}
*/

/*
/* Ver.2 */
fn nexttoken2() -> Rc<Word> {
    let mut w: Rc<Word> = Rc::new(Dummy);
    LASTTK.with(|last_tk| {
        let mut g = last_tk.borrow_mut(); // RefMut<Rc<>>
        w = Rc::clone(&*g);
        if *w == Empty {
            println!("(nexttoken2) calling nexttoken()");
            w = Rc::new(nexttoken());
            *g = Rc::clone(&w);
        }
    });
    println!("(nexttoken2) return {:?}", w);
    w
}
*/

/* Ver.3 */
fn nexttoken2() -> Rc<Word> {
    let mut w: Rc<Word> = Rc::new(Dummy);
    LASTTK.with(|last_tk| {
        let mut g = last_tk.borrow_mut(); // RefMut<Rc<>>
        if **g == Empty {
            println!("(nexttoken2) calling nexttoken()");
            w = Rc::new(nexttoken()); // newly obtained entity
            *g = Rc::clone(&w); // replace the content of the buffer
        } else {
            w = Rc::clone(&g); // reuse buffered entity
        }
    });
    println!("(nexttoken2) strong_ref {:?}", Rc::strong_count(&w));
    println!("(nexttoken2) return {:?}", w);
    w
}



fn nexttoken() -> Word {
    let mut w = EOF;
    VBUF.with(|vbuf| {
        w = yylex(&mut *vbuf.borrow_mut());
    });
    w
}

// tokens: buffer that should be global
// ret: a token of type Word
fn yylex(tokens: &mut Vec<Word>) -> Word {
    // if TOKENS is empty:
    //   reat a line
    //   extract tokens to make TOKENS (by using an LALRPOP Parser)
    // pop a token from TOKENS
    // return one

    if tokens.len() == 0 {
        loop {
            let mut in_str: String = String::new();
            let len = std::io::stdin().read_line(&mut in_str)
                .expect("Failed to read line");
            if in_str.len() == 0 { // len=0 -> EOF
                return EOF;
            }
            in_str = in_str.trim().to_string(); // remove whitespaces
            if in_str.len() >= 1 {
                *tokens = lexer::TListParser::new().parse(&in_str).unwrap();
                break;
            }
            // TOKENS was empty but can not be prepared -> repeat
        }
    }
    tokens.pop().unwrap()
}