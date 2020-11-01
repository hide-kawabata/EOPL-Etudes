//#[derive(Debug, PartialEq, Copy, Clone)]
#[derive(Debug, PartialEq)]
// pub enum Word<'a> {
pub enum Word {
    Print,
    Def,
    // Id(&'a str),
    Id(String),
    // Num(&'a str),
    Num(String),
    Add,
    Sub,
    Semic,
    Spc,
    Nl,
    EOF,
    Empty,
    Dummy,
}
