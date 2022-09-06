use crate::{
    error::{ Error, ErrorKind },
    token::Token
};
use std::rc::Rc;

#[derive(Debug)]
pub enum TypeKind<'s, 't> {
    Bool,
    
    Char,
    
    ClassName(String),

    EmptyList,
    
    Float,

    Function {
        args: Vec<Rc<TypeKind<'s, 't>>>,
        return_type: Rc<TypeKind<'s, 't>>
    },

    Int,

    List(Rc<TypeKind<'s, 't>>),

    HigherOrder {
        name: &'t Token<'s>,
        args: Vec<Rc<TypeKind<'s, 't>>>
    },

    NoneType,

    String,
}


impl<'s, 't> TypeKind<'s, 't> {
    pub fn rc(self) -> Rc<TypeKind<'s, 't>> {
        /* Wraps self in an Rc<> */

        Rc::new(self)
    }
}


impl<'s, 't> PartialEq for TypeKind<'s, 't> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TypeKind::Bool, TypeKind::Bool) => true,
            (TypeKind::Char, TypeKind::Char) => true,
            (TypeKind::ClassName(x), TypeKind::ClassName(y)) => x == y,
            (TypeKind::EmptyList, TypeKind::EmptyList) => true,
            (TypeKind::Float, TypeKind::Float) => true,
            (TypeKind::Int, TypeKind::Int) => true,
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
            (TypeKind::List(a), TypeKind::List(b)) => *a == *b,
            (TypeKind::NoneType, TypeKind::NoneType) => true,
            (TypeKind::String, TypeKind::String) => true,
            _ => false
        }
    }   
}


impl<'s, 't> std::fmt::Display for TypeKind<'s, 't> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string: String = match self {
            TypeKind::Bool => "bool".into(),
            TypeKind::Char => "char".into(),
            TypeKind::ClassName(name) => format!("<Class '{}'>", name),
            TypeKind::EmptyList => "<empty list>".into(),
            TypeKind::Float => "float".into(),
            TypeKind::Function { args, return_type } => {
                let mut string = String::from("fun (");

                for arg in args.iter() {
                    string = format!("{}{}, ", string, arg);
                }

                string = format!(
                    "{}) {}",
                    &string[..string.len()-2],
                    if let TypeKind::NoneType = &**return_type {
                        String::new()
                    } else {
                        return_type.to_string()
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
            TypeKind::NoneType => "<none>".into(),
            TypeKind::String => "str".into()
        };

        write!(f, "{}", string)
    }
}


pub fn parse_type_annotation<'s, 't>(tokens: &'t [Token<'s>]) -> Result<TypeKind<'s, 't>, Vec<Error>> {
    /* Parses a type kind */

    match tokens {
        [] => Error::new(ErrorKind::SyntaxError).set_message("empty type annotation").into(),

        [Token::Identifier { string, .. }] => {
            match *string {
                "bool"  => Ok(TypeKind::Bool),
                "char"  => Ok(TypeKind::Char),
                "float" => Ok(TypeKind::Float),
                "int"   => Ok(TypeKind::Int),
                "str"   => Ok(TypeKind::String),
                _       => Ok(TypeKind::HigherOrder { name: &tokens[0], args: vec![] })
            }
        },

        [Token::Block { open_delim: "[", contents, .. }] => {
            Ok(TypeKind::List(parse_type_annotation(contents)?.rc()))
        },

        [Token::Block { open_delim: "(", contents, .. }] => {
            parse_type_annotation(&contents)
        }

        [Token::Identifier {..}, Token::Block { open_delim: "[", contents, .. }] => {
            let name = &tokens[0];
            let mut args = vec![];

            for (start, end) in Token::split_tokens(contents, |t| t.name() == ",") {
                args.push(parse_type_annotation(&contents[start..end])?.rc());
            }

            Ok(TypeKind::HigherOrder { name, args })
        }

        [kwd_fun, Token::Block { open_delim: "(", contents, ..}, ret_xs @ ..] => {
            if kwd_fun.name() != "fun" {
                return Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("unrecognised syntax in type annotation")
                        .into();
            }

            let mut args = vec![];
            let return_type = if ret_xs.is_empty() {
                TypeKind::NoneType.rc()
            } else {
                parse_type_annotation(ret_xs)?.rc()
            };

            for (start, end) in Token::split_tokens(contents, |t| t.name() == ",") {
                args.push(parse_type_annotation(&contents[start..end])?.rc());
            }

            Ok(TypeKind::Function { args, return_type })
        }

        _ => Error::new(ErrorKind::SyntaxError)
                .set_position(tokens[0].position())
                .set_message("unrecognised syntax in type annotation")
                .into()
    }
}
