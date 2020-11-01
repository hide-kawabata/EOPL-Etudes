// use crate::check::lookup_id;

use crate::ast::{L_t, C_t, E_t, Id_t};
use crate::ast::L_t::*;
use crate::ast::C_t::*;
use crate::ast::E_t::*;

pub fn cgen(tree: &L_t) {
    prologue();
    cgen_L(tree);
    epilogue();
}

fn prologue() {
    println!("#include <stdio.h>");
    println!("int main() {{");
}

fn epilogue() {
    println!("return 0;");
    println!("}}");
}

fn cgen_L(tree: &L_t) {
    match &*tree {
        EPS => {
        },
        CL(c, l) => {
            cgen_C(&c);
            cgen_L(&l);
        }
    }
}

fn cgen_C(c: &C_t) {
    match &*c {
        Print(e) => {
            println!("printf(\"%d\", ");
            cgen_E(&e);
            println!(");");
        },
        Def(id, e) => {
            println!("int {} = ", id.name);
            cgen_E(&e);
            println!(";");
        },
    }
}

fn cgen_E(e: &E_t) {
    match &*e {
        Var(id) => {
            println!("({})", &id.name);
        },
        Num(i) => {
            println!("({})", i);
        },
        Add(e1, e2) => {
            println!("(");
            cgen_E(&e1);
            println!(")+(");
            cgen_E(&e2);
            println!(")");
        },
        Sub(e1, e2) => {
            println!("(");
            cgen_E(&e1);
            println!(")-(");
            cgen_E(&e2);
            println!(")");
        },
    }
}

