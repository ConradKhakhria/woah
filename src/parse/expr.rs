use crate::{
    error::{ Error, ErrorKind },
    parse::TypeKind,
    token::Token
};
use std::rc::Rc;

#[derive(Debug)]
pub enum ExprKind<'s, 't> {
    Compound {
        operator: String,
        left: Box<Expr<'s, 't>>,
        right: Box<Expr<'s, 't>>
    },

    Unary {
        operator: String,
        operand: Box<Expr<'s, 't>>
    },

    FunctionCall {
        function: Box<Expr<'s, 't>>,
        args: Vec<Expr<'s, 't>>
    },

    ArrayIndexing {
        array: Box<Expr<'s, 't>>,
        index: Box<Expr<'s, 't>>
    },

    ArrayLiteral {
        elems: Vec<Expr<'s, 't>>
    },

    AttrRes {
        parent: Box<Expr<'s, 't>>,
        attr_name: String
    },

    String,

    Integer,

    Float,

    Identifier,
}

#[derive(Debug)]
pub struct Expr<'s, 't> {
    pub expr_kind: ExprKind<'s, 't>,
    pub expr_type: Option<Rc<TypeKind>>,
    pub first_token: &'t Token<'s>,
    pub last_token: &'t Token<'s>
}


impl<'s, 't> Expr<'s, 't> {
    pub fn from_tokens(tokens: &'t [Token<'s>], pos: (usize, usize)) -> Result<Self, Vec<Error>> {
        /* Parses an expression from a list of tokens */

        if tokens.is_empty() {
            return Error::new(ErrorKind::SyntaxError)
                    .set_position(pos)
                    .set_message("Received empty expression")
                    .into();
        }
    
        for option in PARSE_OPTIONS {
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


/* Parsing */

type ParseOption<'s, 't> = Option<Result<Expr<'s, 't>, Vec<Error>>>;


fn parse_atomic_expression<'s, 't>(tokens: &'t [Token<'s>]) -> ParseOption<'s, 't> {
    /* Parses an expression consisting of a single token */

    if tokens.len() != 1 {
        return None;
    }

    let expr_kind = match &tokens[0] {
        Token::Identifier {..} => ExprKind::Identifier,

        Token::Number { string, ..} => {
            if string.contains(".") {
                ExprKind::Float
            } else {
                ExprKind::Integer
            }
        },

        Token::String {..} => ExprKind::String,

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
                        first_token: &tokens[0],
                        last_token: contents.last().unwrap_or(&tokens[0])
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
            first_token: &tokens[0],
            last_token: &tokens[0]
        }
    ))
}


fn parse_array<'s, 't>(contents: &'t [Token<'s>], pos: (usize, usize)) -> Result<Vec<Expr<'s, 't>>, Vec<Error>> {
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


fn parse_attr_res<'s, 't>(tokens: &'t [Token<'s>]) -> ParseOption<'s, 't> {
    /* Parses an attribute resolution */

    if tokens.len() < 3 || tokens[tokens.len() - 2].to_string() != "." {
        return None;
    }

    let mut errors = Vec::new();
    let mut parent = Expr::from_tokens(&tokens[..tokens.len()-2], tokens[0].position());
    let mut attr_name = tokens.last().unwrap().to_string();
    
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
            first_token: &tokens[0],
            last_token: tokens.last().unwrap()
        })
    } else {
        Err(errors)
    })
}


fn parse_compound<'s, 't>(tokens: &'t [Token<'s>]) -> ParseOption<'s, 't> {
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
                                first_token: &tokens[0],
                                last_token: tokens.last().unwrap()
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
                first_token: &tokens[0],
                last_token: tokens.last().unwrap()
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


fn parse_funcall<'s, 't>(tokens: &'t [Token<'s>]) -> ParseOption<'s, 't> {
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
            first_token: tokens.first().unwrap(),
            last_token:  tokens.last().unwrap()
        }
    ))
}


fn parse_indexing<'s, 't>(tokens: &'t [Token<'s>]) -> ParseOption<'s, 't> {
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
            first_token: tokens.first().unwrap(),
            last_token: tokens.last().unwrap()
        }
    ))
}


// For some reason, rust doesn't like having a list of func ptrs with lifetimes attached to them
// as a function variable
const PARSE_OPTIONS: [for<'s, 't> fn(&'t [Token<'s>]) -> Option<Result<Expr<'s, 't>, Vec<Error>>>; 5] = [
    parse_atomic_expression,
    parse_compound,
    parse_funcall,
    parse_indexing,
    parse_attr_res
];
