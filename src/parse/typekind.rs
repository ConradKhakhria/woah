use crate::{
    message::{ Message, MsgKind },
    token::Token
};
use std::rc::Rc;

#[derive(Debug)]
pub enum TypeKind {
    Bool,

    Char,

    EmptyList,

    Float,

    Function {
        args: Vec<Rc<TypeKind>>,
        return_type: Option<Rc<TypeKind>>
    },

    HigherOrder {
        name: String,
        args: Vec<Rc<TypeKind>>
    },

    Int,

    List(Rc<TypeKind>),

    Module(String),

    MutRef(Rc<TypeKind>),

    String
}


impl TypeKind {
    /* Initialisation */

    fn parse_head_block_type(tokens: &[Token]) -> Result<Self, Vec<Message>> {
        /* Parses a type which begins with a bracketed block */

        match tokens {
            [Token::Block { open_delim: "[", contents, .. }, rest @ .. ] => {
                if rest.is_empty() {
                    Message::new(MsgKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("Unrecognised type '[]'")
                        .into()
                } else if !contents.is_empty() {
                    Message::new(MsgKind::SyntaxError)
                        .set_position(contents[0].position())
                        .set_message("List types must have the syntax []<type>")
                        .into()
                } else {
                    Ok(TypeKind::List(Self::from_tokens(rest)?.rc()))
                }
            },

            [Token::Block { open_delim: "(", contents, .. }] => {
                if contents.is_empty() {
                    Message::new(MsgKind::SyntaxError)
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
                            TypeKind::from_tokens(&rest[1..]).map(|t| Some(t.rc()))
                        }
                    }

                    _ => TypeKind::from_tokens(&rest[1..]).map(|t| Some(t.rc()))
                };

                if let Err(ref mut es) = return_type {
                    errors.append(es);
                }

                let arg_type_slice = Token::split_tokens(&contents[..], |t| t.to_string() == ",");
                let mut arg_types = vec![];

                for (start, end) in arg_type_slice {
                    match TypeKind::from_tokens(&contents[start..end]) {
                        Ok(tp) => arg_types.push(tp.rc()),
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
        
            _ => Message::new(MsgKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("unrecognised syntax in type annotation")
                        .into()
        }
    }


    fn parse_head_identifier_type(tokens: &[Token]) -> Result<Self, Vec<Message>> {
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
                 Message::new(MsgKind::TypeError)
                        .set_position(tokens[0].position())
                        .set_message(format!("Type '{}' cannot have parameters", tokens[0].to_string()))
                        .into()
            }
        }

        let mut type_parameters = vec![];
        let mut errors = vec![];

        for i in 1..tokens.len() {
            match Self::from_tokens(&tokens[i..=i]) {
                Ok(tp) => type_parameters.push(tp.rc()),
                Err(ref mut es) => errors.append(es)
            }
        }

        if errors.is_empty() {
            Ok(TypeKind::HigherOrder {
                name: tokens[0].to_string(),
                args: type_parameters
            })
        } else {
            Err(errors)
        }
    }
    

    fn parse_head_symbol_type(symbol: &str, tokens: &[Token]) -> Result<Self, Vec<Message>> {
        /* Parses a type whose first token is a symbol */

        if symbol == "@" {
            Ok(TypeKind::MutRef(
                TypeKind::from_tokens(&tokens[1..])?.rc()
            ))
        } else {
            Message::new(MsgKind::SyntaxError)
                .set_position(tokens[0].position())
                .set_message("Unrecognised syntax in type annotation")
                .into()
        }
    }


    pub fn from_tokens(tokens: &[Token]) -> Result<Self, Vec<Message>> {
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

            _ => Message::new(MsgKind::SyntaxError)
                    .set_position(tokens[0].position())
                    .set_message("Unrecognised syntax in type annotation")
                    .into()
        }
    }


    /* Utils */

    pub fn rc(self) -> Rc<TypeKind> {
        /* Wraps self in an Rc<> */

        Rc::new(self)
    }
}


impl PartialEq for TypeKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TypeKind::Bool, TypeKind::Bool) => true,
            (TypeKind::Char, TypeKind::Char) => true,
            (TypeKind::Module(x), TypeKind::Module(y)) => x == y,
            (TypeKind::Float, TypeKind::Float) => true,
            (TypeKind::Int, TypeKind::Int) => true,
            (TypeKind::EmptyList, TypeKind::EmptyList) => true,
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


impl std::fmt::Display for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string: String = match self {
            TypeKind::Bool => "bool".into(),
            TypeKind::Char => "char".into(),
            TypeKind::Module(name) => format!("<Class '{}'>", name),
            TypeKind::EmptyList => "<empty list>".into(),
            TypeKind::Float => "float".into(),
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
            TypeKind::String => "str".into()
        };

        write!(f, "{}", string)
    }
}
