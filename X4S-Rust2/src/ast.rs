#[derive(Debug, PartialEq)]
pub enum L_t {
    EPS, 
    CL(Box<C_t>, Box<L_t>),
}

#[derive(Debug, PartialEq)]
pub enum C_t {
    Print(Box<E_t>),
    Def(Box<Id_t>, Box<E_t>),
}

#[derive(Debug, PartialEq)]
pub enum E_t {
    Var(Box<Id_t>),
    Num(i32),
    Add(Box<E_t>, Box<E_t>),
    Sub(Box<E_t>, Box<E_t>),
}

#[derive(Debug, PartialEq)]
pub struct Id_t {
    pub name: String,
}
