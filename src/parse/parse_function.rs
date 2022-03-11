use crate::{
    error::{ Error, ErrorKind },
    token::Token,
    parse::{ Statement, TypeKind }
};

pub struct Function<'s, 't> {
    name: &'t Token<'s>,
    args: Vec<(&'t Token<'s>, TypeKind<'s, 't>)>,
    generics: Vec<(&'t Token<'s>, Vec<&'t Token<'t>>)>,
    body: Vec<Statement<'s, 't>>
}

impl<'s, 't> Function<'s, 't> {
    pub fn from_tokens<'s 
}
