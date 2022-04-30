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
    /* Initialisation */

    fn parse_head_block_type(tokens: &'t [Token<'s>]) -> Result<Self, Vec<Error>> {
        /* Parses a type which begins with a bracketed block */

        match tokens {
            [Token::Block { open_delim: "[", contents, .. }, rest @ .. ] => {
                if rest.is_empty() {
                    Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("Unrecognised type '[]'")
                        .into()
                } else if !contents.is_empty() {
                    Error::new(ErrorKind::SyntaxError)
                        .set_position(contents[0].position())
                        .set_message("List types must have the syntax []<type>")
                        .into()
                } else {
                    Ok(TypeKind::List(Rc::new(
                        Self::from_tokens(rest)?
                    )))
                }
            },

            [Token::Block { open_delim: "(", contents, .. }] => {
                if contents.is_empty() {
                    Error::new(ErrorKind::SyntaxError)
                            .set_position(tokens[0].position())
                            .set_message("unrecognised type '()'")
                            .into()   
                } else {
                    TypeKind::from_tokens(&contents[..])
                }
            },
        
            [Token::Block { open_delim: "(", contents, .. }, rest @ .. ] => {
                let mut errors = vec![];

                let mut return_type = match &rest[1..] {
                    [Token::Block { open_delim: "(", contents, .. }] => {
                        if contents.len() == 0 {
                            Ok(None)
                        } else {
                            TypeKind::from_tokens(&rest[1..]).map(|t| Some(Rc::new(t)))
                        }
                    }

                    _ => TypeKind::from_tokens(&rest[1..]).map(|t| Some(Rc::new(t)))
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
        
            _ => Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("unrecognised syntax in type annotation")
                        .into()
        }
    }


    fn parse_head_identifier_type(tokens: &'t [Token<'s>]) -> Result<Self, Vec<Error>> {
        /* Parses a type whose first token is an identifier */

        let head_name = tokens[0].to_string();
        let builtin_result = match head_name.as_str() {
            "int"   => Ok(TypeKind::Int),
            "float" => Ok(TypeKind::Float),
            "char"  => Ok(TypeKind::Char),
            "str"   => Ok(TypeKind::String),
            "bool"  => Ok(TypeKind::Bool),
            _       => Err(vec![])
        };

        if let Ok(_) = &builtin_result {
            return if tokens.len() == 1 {
                builtin_result
            } else {
                 Error::new(ErrorKind::TypeError)
                        .set_position(tokens[0].position())
                        .set_message(format!("Type '{}' cannot have parameters", tokens[0].to_string()))
                        .into()
            }
        }

        let mut type_parameters = vec![];
        let mut errors = vec![];

        for i in 1..tokens.len() {
            match Self::from_tokens(&tokens[i..=i]) {
                Ok(tp) => type_parameters.push(Rc::new(tp)),
                Err(ref mut es) => errors.append(es)
            }
        }

        if errors.is_empty() {
            Ok(TypeKind::HigherOrder {
                name: &tokens[0],
                args: type_parameters
            })
        } else {
            Err(errors)
        }
    }
    

    fn parse_head_symbol_type(symbol: &str, tokens: &'t [Token<'s>]) -> Result<Self, Vec<Error>> {
        /* Parses a type whose first token is a symbol */

        if symbol == "@" {
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


    pub fn from_tokens(tokens: &'t [Token<'s>]) -> Result<Self, Vec<Error>> {
        /* Parses a type */

        match tokens {
            [Token::Identifier { .. }, ..] => {
                Self::parse_head_identifier_type(tokens)
            }

            [Token::Block { open_delim: "(", contents, ..}] => {
                TypeKind::from_tokens(&contents[..])
            },

            [Token::Block {..}, .. ] => {
                TypeKind::parse_head_block_type(tokens)
            }

            [Token::Symbol { string, .. }, ..] => {
                Self::parse_head_symbol_type(string, tokens)
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
                    string = format!("{} ({})", string, arg);
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
