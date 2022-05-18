use crate::{
    error::{ Error, ErrorKind },
    parse::TypeKind,
    token::Token
};
use std::rc::Rc;

#[derive(Debug)]
pub enum ExprKind {
    ArrayIndexing {
        array: Box<Expr>,
        index: Box<Expr>
    },

    ArrayLiteral {
        elems: Vec<Expr>
    },

    AttrRes {
        parent: Box<Expr>,
        attr_name: String
    },

    Compound {
        operator: String,
        left: Box<Expr>,
        right: Box<Expr>
    },

    FunctionCall {
        function: Box<Expr>,
        args: Vec<Expr>
    },

    Float(String),

    Integer(String),

    Identifier(String),

    String(String),

    Unary {
        operator: String,
        operand: Box<Expr>
    },
}

#[derive(Debug)]
pub struct Expr {
    pub expr_kind: ExprKind,
    pub expr_type: Option<Rc<TypeKind>>,
    pub first_position: (usize, usize),
    pub last_position: (usize, usize)
}


impl Expr {
    pub fn from_tokens(tokens: &[Token], pos: (usize, usize)) -> Result<Self, Vec<Error>> {
        /* Parses an expression from a list of tokens */

        let parse_options = [
            parse_atomic_expression,
            parse_compound,
            parse_funcall,
            parse_indexing,
            parse_attr_res
        ];        

        if tokens.is_empty() {
            return Error::new(ErrorKind::SyntaxError)
                    .set_position(pos)
                    .set_message("Received empty expression")
                    .into();
        }
    
        for option in parse_options {
            if let Some(expr) = option(tokens) {
                return expr;
            }
        }
    
        Error::new(ErrorKind::SyntaxError)
            .set_position(tokens[0].position())
            .set_message("Unrecognised syntax in expression")
            .into()
    }
}


impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.expr_kind {
            ExprKind::ArrayIndexing { array, index } => {
                write!(f, "{}[ {} ]", array, index)
            },

            ExprKind::ArrayLiteral { elems } => {
                let mut string = String::from("[");

                for e in elems.iter() {
                    string = format!("{}{}, ", string, e);
                }

                let strlen = string.len();

                write!(f, "{}]", &string[..strlen - 2])
            },

            ExprKind::AttrRes { parent, attr_name } => {
                write!(f, "{}.{}", parent, attr_name)
            },

            ExprKind::Compound { operator, left, right } => {
                write!(f, "{} {} {}", left, operator, right)
            },

            ExprKind::Float(s) => write!(f, "{}", s),

            ExprKind::FunctionCall { function, args } => {
                let mut string = format!("{}(", function);

                for arg in args.iter() {
                    string = format!("{}{}, ", string, arg);
                }

                let strlen = string.len();

                if args.len() == 0 {
                    write!(f, "{}()", function)
                } else {
                    write!(f, "{})", &string[..strlen - 2])
                }
            },

            ExprKind::Identifier(ident) => write!(f, "{}", ident),

            ExprKind::Integer(s) => write!(f, "{}", s),

            ExprKind::String(s) => write!(f, "\"{}\"", s),

            ExprKind::Unary { operator, operand } => {
                write!(f, "{}{}", operator, operand)
            }
        }
    }
}


/* Parsing */

type ParseOption = Option<Result<Expr, Vec<Error>>>;


fn parse_atomic_expression(tokens: &[Token]) -> ParseOption {
    /* Parses an expression consisting of a single token */

    if tokens.len() != 1 {
        return None;
    }

    let expr_kind = match &tokens[0] {
        Token::Identifier {..} => ExprKind::Identifier(tokens[0].to_string()),

        Token::Number { string, ..} => {
            if string.contains(".") {
                ExprKind::Float(string.to_string())
            } else {
                ExprKind::Integer(string.to_string())
            }
        },

        Token::String {..} => ExprKind::String(tokens[0].to_string()),

        Token::Symbol { string, .. } => {
            return Some(
                Error::new(ErrorKind::SyntaxError)
                    .set_position(tokens[0].position())
                    .set_message(format!("Expected expression, received '{}'", string))
                    .into()
            )
        },

        Token::Block { open_delim, contents, .. } => {
            return if *open_delim == "(" {
                 Some(Expr::from_tokens(&contents[..], tokens[0].position()))
            } else if *open_delim == "[" {
                Some(match parse_array(&contents[..], tokens[0].position()) {
                    Ok(es) => Ok(Expr {
                        expr_kind: ExprKind::ArrayLiteral { elems: es },
                        expr_type: None,
                        first_position: tokens[0].position(),
                        last_position: contents.last().unwrap_or(&tokens[0]).position()
                    }),
                    Err(es) => Err(es)
                })
            } else {
                Some(
                    Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("Expected expression, received code block")
                        .into()
                )
            }
        }

        Token::NewlineIndent(_) => unreachable!()
    };

    Some(Ok(
        Expr {
            expr_kind,
            expr_type: None,
            first_position: tokens[0].position(),
            last_position: tokens[0].position()
        }
    ))
}


fn parse_array(contents: &[Token], pos: (usize, usize)) -> Result<Vec<Expr>, Vec<Error>> {
    /* Parses an array literal */

    let mut values = Vec::new();
    let mut errors = Vec::new();
    
    for (start, end) in Token::split_tokens(contents, |t| t.to_string() == ",") {
        match Expr::from_tokens(&contents[start..end], pos) {
            Ok(expr) => values.push(expr),
            Err(ref mut es) => errors.append(es)
        }
    }

    if errors.is_empty() {
        Ok(values)
    } else {
        Err(errors)
    }
}


fn parse_attr_res(tokens: &[Token]) -> ParseOption {
    /* Parses an attribute resolution */

    if tokens.len() < 3 || tokens[tokens.len() - 2].to_string() != "." {
        return None;
    }

    let mut errors = Vec::new();
    let mut parent = Expr::from_tokens(&tokens[..tokens.len()-2], tokens[0].position());
    let attr_name = tokens.last().unwrap().to_string();
    
    if let Token::Identifier {..} = tokens.last().unwrap() {} else {
        errors.push(Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens.last().unwrap().position())
                        .set_message("Expected attribute name"));
    }

    if let Err(ref mut es) = parent {
        errors.append(es);
    }

    Some(if errors.is_empty() {
        Ok(Expr {
            expr_kind: ExprKind::AttrRes {
                parent: Box::new(parent.unwrap()),
                attr_name
            },
            expr_type: None,
            first_position: tokens[0].position(),
            last_position: tokens.last().unwrap().position()
        })
    } else {
        Err(errors)
    })
}


fn parse_compound(tokens: &[Token]) -> ParseOption {
    /* Parses a compound expression */

    let comparison_operators = vec!["==", "!=", "<", ">", "<=", ">="];
    let arithmetic_operators = vec!["+", "-", "*", "/", "%"];
    let mut token_strings = vec![];
    let mut errors = Vec::new();

    for token in tokens.iter() {
        token_strings.push(token.to_string());
    }

    for operator_list in [comparison_operators, arithmetic_operators] {
        for op in operator_list {
            for i in 1..(tokens.len() - 1) {
                if token_strings[i] == op {
                    let left = Expr::from_tokens(&tokens[..i], tokens[0].position());
                    let right = Expr::from_tokens(&tokens[i+1..], tokens[i+1].position());
    
                    return Some(
                        if left.is_ok() && right.is_ok() {
                            let expr_kind = ExprKind::Compound {
                                operator: op.into(),
                                left: Box::new(left.unwrap()),
                                right: Box::new(right.unwrap())
                            };
    
                            Ok(Expr {
                                expr_kind,
                                expr_type: None,
                                first_position: tokens[0].position(),
                                last_position: tokens.last().unwrap().position()
                            })
                        } else {
                            let _ = left.map_err(|mut es| errors.append(&mut es));
                            let _ = right.map_err(|mut es| errors.append(&mut es));
    
                            Err(errors)
                        }
                    )
                }
            }
        }
    }

    if tokens[0].to_string() == "-" && tokens.len() > 1 {
        Some(if let Ok(expr) = Expr::from_tokens(&tokens[1..], tokens[1].position()) {
            Ok(Expr {
                expr_kind: ExprKind::Unary {
                    operator: tokens[0].to_string(),
                    operand: Box::new(expr)
                },
                expr_type: None,
                first_position: tokens[0].position(),
                last_position: tokens.last().unwrap().position()
            })
        } else {
            Error::new(ErrorKind::SyntaxError)
                .set_position(tokens[0].position())
                .set_message("Unrecognised syntax in expression")
                .into()
        })
    } else {
        None
    }
}


fn parse_funcall(tokens: &[Token]) -> ParseOption {
    /* Parses a function call */

    let tokens_len = tokens.len();

    let args = match tokens.last().unwrap() {
        Token::Block { contents, open_delim, .. } => {
            if *open_delim == "(" {
                let mut exprs = Vec::new();
                let mut errors = Vec::new();

                let slices = Token::split_tokens(&contents[..], |t| t.to_string() == ",");

                for (start, end) in slices {
                    match Expr::from_tokens(&contents[start..end], contents[start].position()) {
                        Ok(expr) => exprs.push(expr),
                        Err(ref mut es) => errors.append(es)
                    }
                }

                if errors.is_empty() {
                    exprs
                } else {
                    return Some(Err(errors));
                }
            } else {
                return None;
            }
        },
        _ => return None
    };

    let function = match Expr::from_tokens(&tokens[..tokens_len-1], tokens[0].position()) {
        Ok(fb) => Box::new(fb),
        err => return Some(err)
    };

    Some(Ok(
        Expr {
            expr_kind: ExprKind::FunctionCall { function, args },
            expr_type: None,
            first_position: tokens.first().unwrap().position(),
            last_position:  tokens.last().unwrap().position()
        }
    ))
}


fn parse_indexing(tokens: &[Token]) -> ParseOption {
    /* Parses an array indexing */

    let tokens_len = tokens.len();

    let index = match tokens.last().unwrap() {
        Token::Block { contents, open_delim, .. } => {
            if *open_delim == "[" && contents.len() > 0 {
                match Expr::from_tokens(contents, contents[0].position()) {
                    Ok(expr) => Box::new(expr),
                    err => return Some(err)
                }
            } else {
                return None;
            }
        },
        _ => return None
    };

    let array = match Expr::from_tokens(&tokens[..tokens_len-1], tokens[0].position()) {
        Ok(arr) => Box::new(arr),
        err => return Some(err)
    };

    Some(Ok(
        Expr {
            expr_kind: ExprKind::ArrayIndexing { array, index },
            expr_type: None,
            first_position: tokens.first().unwrap().position(),
            last_position: tokens.last().unwrap().position()
        }
    ))
}
