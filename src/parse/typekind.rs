use crate::error::*;
use crate::token::Token;
use std::rc::Rc;

#[derive(Debug)]
pub enum TypeKind {
    Bool,
    
    Char,
    
    ClassName(String),

    EmptyList,
    
    Float,

    Function {
        args: Vec<Rc<TypeKind>>,
        return_type: Rc<TypeKind>
    },

    Int,

    List(Rc<TypeKind>),

    HigherOrder {
        name: String,
        args: Vec<Rc<TypeKind>>
    },

    NoneType,

    ReportedError,  // for when there's an error in determining the type
                    // which has already been reported.
    String,
}


impl TypeKind {
    pub fn rc(self) -> Rc<TypeKind> {
        /* Wraps self in an Rc<> */

        Rc::new(self)
    }
}


impl PartialEq for TypeKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TypeKind::ReportedError, _) => true,
            (_, TypeKind::ReportedError) => true,
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


impl std::fmt::Display for TypeKind {
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
            TypeKind::ReportedError => "<type-error>".into(),
            TypeKind::String => "str".into()
        };

        write!(f, "{}", string)
    }
}


pub fn parse_type_annotation(tokens: &[Token], pos: (usize, usize)) -> Result<TypeKind, Vec<Error>> {
    /* Parses a type kind */

    match tokens {
        [] => {
            Error::new(ErrorKind::SyntaxError)
                .set_message("empty type annotation")
                .set_position(pos)
                .into()
        },

        [Token::Identifier { string, .. }] => {
            match *string {
                "bool"  => Ok(TypeKind::Bool),
                "char"  => Ok(TypeKind::Char),
                "float" => Ok(TypeKind::Float),
                "int"   => Ok(TypeKind::Int),
                "str"   => Ok(TypeKind::String),
                n       => Ok(TypeKind::HigherOrder { name: n.to_string(), args: vec![] })
            }
        },

        [Token::Block { open_delim: "[", contents, .. }] => {
            Ok(TypeKind::List(parse_type_annotation(contents, pos)?.rc()))
        },

        [Token::Block { open_delim: "(", contents, .. }] => {
            parse_type_annotation(&contents, pos)
        }

        [Token::Identifier {..}, Token::Block { open_delim: "[", contents, .. }] => {
            let name = if tokens[0].upper_case() {
                tokens[0].to_string()
            } else {
                return Error::new(ErrorKind::SyntaxError)
                            .set_position(tokens[0].position())
                            .set_message("expected (upper-case) name in higher-order type")
                            .into()
            };

            let mut args = vec![];

            for (start, end) in Token::split_tokens(contents, |t| t.to_string() == ",") {
                args.push(parse_type_annotation(&contents[start..end], pos)?.rc());
            }

            Ok(TypeKind::HigherOrder { name, args })
        }

        [kwd_fun, Token::Block { open_delim: "(", contents, ..}, ret_xs @ ..] => {
            if kwd_fun.to_string() != "fun" {
                return Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("unrecognised syntax in type annotation")
                        .into();
            }

            let mut args = vec![];
            let return_type = if ret_xs.is_empty() {
                TypeKind::NoneType.rc()
            } else {
                parse_type_annotation(ret_xs, pos)?.rc()
            };

            for (start, end) in Token::split_tokens(contents, |t| t.to_string() == ",") {
                args.push(parse_type_annotation(&contents[start..end], pos)?.rc());
            }

            Ok(TypeKind::Function { args, return_type })
        }

        _ => Error::new(ErrorKind::SyntaxError)
                .set_position(tokens[0].position())
                .set_message("unrecognised syntax in type annotation")
                .into()
    }
}
