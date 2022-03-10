use crate::{
    error::{ Error, ErrorKind },
    token::Token
};

#[derive(Debug)]
pub enum TypeKind<'t, 's> {
    Int,

    Float,

    Char,

    String,

    Bool,

    MutRef(Box<TypeKind<'t, 's>>),

    List(Box<TypeKind<'t, 's>>),

    HigherOrder {
        name: &'t Token<'s>,
        args: Vec<TypeKind<'t, 's>>
    }
}

impl<'t, 's> TypeKind<'t, 's> {
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
