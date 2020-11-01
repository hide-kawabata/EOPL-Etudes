use std::rc::Rc;
use std::cell::RefCell;
use crate::word::Word;
// use crate::word::Word::*; // Word::Empty -> Empty
use crate::ast::{L_t, C_t, E_t, Id_t};
use crate::ast::L_t::*;
use crate::ast::C_t::*;
use crate::ast::E_t::*;

#[derive(Debug)]
pub struct SymTbl {
    syms: Vec<String>,
    nsym: usize,
}


thread_local!{
    pub static SYMTBL: RefCell<SymTbl> = RefCell::new(SymTbl { syms: Vec::new(), nsym: 0 });
}

pub fn check(tree: &L_t) {
    check_L(tree);
    SYMTBL.with(|symtbl| {
        println!("symtbl={:?}", symtbl);
    });
}

fn check_L(tree: &L_t) {
    println!("(check_L)");
    match &*tree {
        EPS => {
            println!("(check_L) EPS");
        },
        CL(c, l) => {
            check_C(&c);
            check_L(&l);
        }
    }
}

fn check_C(c: &C_t) {
    println!("(check_C)");
    match &*c {
        Print(e) => {
            check_E(&e);
        },
        Def(id, e) => {
            println!("def Id({:?})", id);
            register_id(&id.name);
            check_E(&e);
        },
    }
}

fn check_E(e: &E_t) {
    println!("(check_E)");
    match &*e {
        Var(id) => {
            println!("use Var({:?}) {}", id, lookup_id(&id.name));
        },
        Num(i) => {
            println!("use Num({:?})", i);
        },
        Add(e1, e2) => {
            check_E(&e1);
            check_E(&e2);
        },
        Sub(e1, e2) => {
            check_E(&e1);
            check_E(&e2);
        },
    }
}


pub fn register_id(id: &str) -> usize {
    // println!("(register_id) {}", id);
    let mut count = 999;
    SYMTBL.with(|symtbl| {
        let mut tbl = symtbl.borrow_mut(); // RefMut<SymTbl>
        tbl.syms.push(id.to_string());
        tbl.nsym += 1;
        count = tbl.nsym;
    });
    count
}

pub fn lookup_id(name: &str) -> usize {
    // println!("(lookup_id) {}", name);
    let mut index: usize = 999;
    SYMTBL.with(|symtbl| {
        let tbl = symtbl.borrow(); // Ref<SymTbl>
        if tbl.nsym == 0 {
            println!("symtbl={:?}", symtbl);
            panic!("(lookup_id) {} : Empty", name);
        }
        index = tbl.nsym - 1;
        loop {
            if tbl.syms[index] == name {
                break;
            }
            if index == 0 {
                println!("symtbl={:?}", symtbl);
                panic!("(lookup_id) {} : Not found", name);
            }
            index -= 1;
        }
    });
    index
}