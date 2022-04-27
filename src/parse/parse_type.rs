use crate::{
    error::{ Error, ErrorKind },
    token::Token
};

#[derive(Debug)]
pub enum TypeKind<'s, 't> {
    Int,

    Float,

    Char,

    String,

    Bool,

    ObjSelf,

    EmptyList,

    MutRef(Box<TypeKind<'s, 't>>),

    List(Box<TypeKind<'s, 't>>),

    HigherOrder {
        name: &'t Token<'s>,
        args: Vec<TypeKind<'s, 't>>
    }
}

impl<'s, 't> TypeKind<'s, 't> {
    pub fn from_tokens(tokens: &'t [Token<'s>]) -> Result<Self, Vec<Error>> {
        /* Parses a type */

        match tokens {
            [Token::Identifier { string, .. }] => {
                Ok(match *string {
                    "int"   => TypeKind::Int,
                    "float" => TypeKind::Float,
                    "char"  => TypeKind::Char,
                    "str"   => TypeKind::String,
                    "bool"  => TypeKind::Bool,
                    _       => TypeKind::HigherOrder { name: &tokens[0], args: Vec::new() }
                })
            },

            [Token::Identifier { .. }, ..] => {
                let mut args = Vec::new();
                let mut errors = Vec::new();

                for i in 1..tokens.len() {
                    match TypeKind::from_tokens(&tokens[i..=i]) {
                        Ok(tp) => args.push(tp),
                        Err(ref mut es) => errors.append(es)
                    }
                }

                if errors.is_empty() {
                    Ok(TypeKind::HigherOrder { name: &tokens[0], args })
                } else {
                    Err(errors)
                }
            }

            [Token::Block { open_delim, contents, ..}] => {
                match *open_delim {
                    "[" => Ok(TypeKind::List(
                        Box::new(TypeKind::from_tokens(&contents[..])?)
                    )),

                    "(" => TypeKind::from_tokens(&contents[..]),

                    _ => Error::new(ErrorKind::SyntaxError)
                            .set_position(tokens[0].position())
                            .set_message(format!("Unexpected '{}'-enclosed block in type annotation", open_delim))
                            .into()
                }
            },

            [Token::Symbol { string, .. }, ..] => {
                if *string == "@" {
                    Ok(TypeKind::MutRef(
                        Box::new(TypeKind::from_tokens(&tokens[1..])?)
                    ))
                } else {
                    Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("Unrecognised syntax in type annotation")
                        .into()
                }
            }

            _ => Error::new(ErrorKind::SyntaxError)
                    .set_position(tokens[0].position())
                    .set_message("Unrecognised syntax in type annotation")
                    .into()
        }
    }
}

impl<'s, 't> PartialEq for TypeKind<'s, 't> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TypeKind::Bool, TypeKind::Bool)           => true,
            (TypeKind::Char, TypeKind::Char)           => true,
            (TypeKind::Float, TypeKind::Float)         => true,
            (TypeKind::Int, TypeKind::Int)             => true,
            (TypeKind::EmptyList, TypeKind::EmptyList) => true,
            (TypeKind::ObjSelf, TypeKind::ObjSelf)     => true,
            (TypeKind::List(a), TypeKind::List(b)) => *a == *b,
            (TypeKind::MutRef(a), TypeKind::MutRef(b)) => *a == *b,
            (TypeKind::HigherOrder { name: n1, args: a1 },
             TypeKind::HigherOrder { name: n2, args: a2 }) => {
                if n1.to_string() == n2.to_string() && a1.len() == a2.len()  {
                    for (t1, t2) in  a1.iter().zip(a2.iter()) {
                        if t1 != t2 {
                            return false
                        }
                    }

                    true
                } else {
                    false
                }
            },
            _ => false
        }
    }   
}
