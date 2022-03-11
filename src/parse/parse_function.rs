use crate::{
    error::{ Error, ErrorKind },
    token::Token,
    parse::Ty
};

struct Function<'t, 's> {
    name: &'t Token<'s>,
    args: Vec<(&'t Token<'s>, )>
}
