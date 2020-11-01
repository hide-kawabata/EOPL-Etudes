use crate::check::lookup_id;
use crate::check::register_id;
use std::rc::Rc;
use std::cell::RefCell;
use crate::word::Word;
// use crate::word::Word::*; // Word::Empty -> Empty
use crate::ast::{L_t, C_t, E_t, Id_t};
use crate::ast::L_t::*;
use crate::ast::C_t::*;
use crate::ast::E_t::*;

pub fn asmgen(tree: &L_t) {
    asmgen_L(tree);
}

fn asmgen_L(tree: &L_t) {
    println!("(asmgen_L)");
    match &*tree {
        EPS => {
            println!("(asmgen_L) EPS");
        },
        CL(c, l) => {
            asmgen_C(&c);
            asmgen_L(&l);
        }
    }
}

fn asmgen_C(c: &C_t) {
    println!("(asmgen_C)");
    match &*c {
        Print(e) => {
            asmgen_E(&e);
        },
        Def(id, e) => {
            println!("def Id({:?})", id);
            register_id(&id.name);
            asmgen_E(&e);
        },
    }
}

fn asmgen_E(e: &E_t) {
    println!("(asmgen_E)");
    match &*e {
        Var(id) => {
            println!("use Var({:?}) {}", id, lookup_id(&id.name));
        },
        Num(i) => {
            println!("use Num({:?})", i);
        },
        Add(e1, e2) => {
            asmgen_E(&e1);
            asmgen_E(&e2);
        },
        Sub(e1, e2) => {
            asmgen_E(&e1);
            asmgen_E(&e2);
        },
    }
}


