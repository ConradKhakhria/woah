use crate::{
    error::{ Error, ErrorKind },
    token::Token
};
use std::rc::Rc;

#[derive(Debug)]
pub enum TypeKind<'s, 't> {
    Int,

    Float,

    Char,

    String,

    Bool,

    ObjSelf,

    EmptyList,

    MutRef(Rc<TypeKind<'s, 't>>),

    List(Rc<TypeKind<'s, 't>>),

    HigherOrder {
        name: &'t Token<'s>,
        args: Vec<Rc<TypeKind<'s, 't>>>
    },

    Function {
        args: Vec<Rc<TypeKind<'s, 't>>>,
        return_type: Option<Rc<TypeKind<'s, 't>>>
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

                if vec!["int", "float", "char", "str", "bool"].contains(&&tokens[0].to_string()[..]) {
                    return Error::new(ErrorKind::TypeError)
                                .set_position(tokens[0].position())
                                .set_message(format!("Type '{}' cannot have parameters", tokens[0].to_string()))
                                .into()
                }

                for i in 1..tokens.len() {
                    match TypeKind::from_tokens(&tokens[i..=i]) {
                        Ok(tp) => args.push(Rc::new(tp)),
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
                        Rc::new(TypeKind::from_tokens(&contents[..])?)
                    )),

                    "(" => TypeKind::from_tokens(&contents[..]),

                    _ => Error::new(ErrorKind::SyntaxError)
                            .set_position(tokens[0].position())
                            .set_message(format!("Unexpected '{}'-enclosed block in type annotation", open_delim))
                            .into()
                }
            },

            [Token::Block { open_delim: "(", contents, ..}, _, _, .. ] => {
                let mut errors = vec![];

                let mut return_type = TypeKind::from_tokens(&tokens[2..]);

                if let Err(ref mut es) = return_type {
                    errors.append(es);
                }

                let mut return_type = match &tokens[2] {
                    Token::Block { open_delim: "(", contents, .. } => {
                        if contents.len() == 0 {
                            Ok(None)
                        } else {
                            TypeKind::from_tokens(&tokens[2..]).map(|t| Some(Rc::new(t)))
                        }
                    }

                    _ => TypeKind::from_tokens(&tokens[2..]).map(|t| Some(Rc::new(t)))
                };

                if let Err(ref mut es) = return_type {
                    errors.append(es);
                }

                let arg_type_slice = Token::split_tokens(&contents[..], |t| t.to_string() == ",");
                let mut arg_types = vec![];

                for (start, end) in arg_type_slice {
                    match TypeKind::from_tokens(&contents[start..end]) {
                        Ok(tp) => arg_types.push(Rc::new(tp)),
                        Err(ref mut es) => errors.append(es)
                    }
                }

                if errors.is_empty() {
                    Ok(TypeKind::Function {
                        args: arg_types,
                        return_type: return_type.unwrap()
                    })
                } else {
                    Err(errors)
                }
            }

            [Token::Symbol { string, .. }, ..] => {
                if *string == "@" {
                    Ok(TypeKind::MutRef(
                        Rc::new(TypeKind::from_tokens(&tokens[1..])?)
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

impl<'s, 't> std::fmt::Display for TypeKind<'s, 't> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string: String = match self {
            TypeKind::Bool      => "bool".into(),
            TypeKind::Char      => "char".into(),
            TypeKind::EmptyList => "<empty list>".into(),
            TypeKind::Float     => "float".into(),
            TypeKind::Function { args, return_type } => {
                let mut string = String::from("(");

                for arg in args.iter() {
                    string = format!("{}{}, ", string, arg);
                }

                string = format!(
                    "{} -> {}",
                    &string[..string.len()-2],
                    if let Some(tp) = return_type {
                        tp.to_string()
                    } else {
                        String::from("()")
                    }
                );

                string
            },
            TypeKind::HigherOrder { name, args} => {
                let mut string = String::from(name.to_string());

                for arg in args.iter() {
                    string = format!("{} {}", string, arg);
                }

                string
            },
            TypeKind::Int => "int".into(),
            TypeKind::List(t) => format!("[]{}", t),
            TypeKind::MutRef(t) => format!("@{}", t),
            TypeKind::ObjSelf => format!("<Self>"),
            TypeKind::String => "str".into()
        };

        write!(f, "{}", string)
    }
}
