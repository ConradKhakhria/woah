use crate::error::*;
use crate::parse::TypeKind;
use crate::token::Token;
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

    ClassAttrRes {
        class_path: Vec<String>,
        attr_name: String
    },

    Compound {
        operator: String,
        left: Box<Expr>,
        right: Box<Expr>
    },

    Float(f64),

    FunctionCall {
        function: Box<Expr>,
        args: Vec<Expr>
    },

    Integer(i64),

    Identifier(String),

    ObjectAttrRes {
        parent: Box<Expr>,
        attr_name: String 
    },

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
    pub position: [(usize, usize); 2]
}


impl Expr {
    pub fn first_position(&self) -> (usize, usize) {
        /* Gets the first position of the expression */

        self.position[0].clone()
    }


    pub fn last_position(&self) -> (usize, usize) {
        /* Gets the last position of the expression */

        self.position[1].clone()
    }
}


/* Parsing */

type ParseOption = Option<Result<Expr, Vec<Error>>>;


pub fn parse_expr(tokens: &[Token], pos: (usize, usize)) -> Result<Expr, Vec<Error>> {
    /* Parses an expression from a list of tokens */

    if tokens.is_empty() {
        return Error::new(ErrorKind::SyntaxError)
                .set_position(pos)
                .set_message("Received empty expression")
                .into();
    }

    let parse_options = [
        parse_atomic_expression,
        parse_compound,
        parse_funcall,
        parse_indexing,
        parse_class_attr_res,
        parse_object_attr_res
    ];

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


fn parse_atomic_expression(tokens: &[Token]) -> ParseOption {
    /* Parses an expression consisting of a single token */

    if tokens.len() != 1 {
        return None;
    }

    let expr_kind = match &tokens[0] {
        Token::Identifier {..} => ExprKind::Identifier(tokens[0].to_string()),

        Token::Number { string, ..} => {
            if string.contains(".") {
                match string.parse::<f64>() {
                    Ok(f) => ExprKind::Float(f),
                    Err(e) => {
                        return Some(
                            Error::new(ErrorKind::SyntaxError)
                                .set_position(tokens[0].position())
                                .set_message(e.to_string())
                                .into()
                        );
                    }
                }
            } else {
                match string.parse::<i64>() {
                    Ok(i) => ExprKind::Integer(i),
                    Err(e) => {
                        return Some(
                            Error::new(ErrorKind::SyntaxError)
                                .set_position(tokens[0].position())
                                .set_message(e.to_string())
                                .into()
                        );
                    }
                }
            }
        },

        Token::String { string, .. } => ExprKind::String(string.to_string()),

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
                 Some(parse_expr(&contents[..], tokens[0].position()))
            } else if *open_delim == "[" {
                Some(match parse_array(&contents[..], tokens[0].position()) {
                    Ok(es) => Ok(Expr {
                        expr_kind: ExprKind::ArrayLiteral { elems: es },
                        expr_type: None,
                        position: [
                            tokens.first().unwrap().position(),
                            tokens.last().unwrap().position()
                        ]
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
            position: [
                tokens.first().unwrap().position(),
                tokens.last().unwrap().position()
            ]
        }
    ))
}


fn parse_array(contents: &[Token], pos: (usize, usize)) -> Result<Vec<Expr>, Vec<Error>> {
    /* Parses an array literal */

    let mut values = Vec::new();
    let mut errors = Vec::new();
    
    for (start, end) in Token::split_tokens(contents, |t| t.to_string() == ",") {
        match parse_expr(&contents[start..end], pos) {
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


fn parse_class_attr_res(tokens: &[Token]) -> ParseOption {
    /* Parses an attribute resolution */

    if tokens.len() < 3 || tokens[tokens.len() - 2].to_string() != "::" {
        return None;
    }

    let attr_name = match &tokens[2] {
        Token::Identifier { string, .. } => string.to_string(),
        token => {
            return Some(
                Error::new(ErrorKind::SyntaxError)
                    .set_position(token.position())
                    .set_message("Expected syntax <module path>::<name>")
                    .into()
            )
        }
    };

    let mut class_path = vec![];
    let error = Error::new(ErrorKind::SyntaxError)
                    .set_message("Expected syntax <module name>::<attribute>");

    for i in 0..(tokens.len() - 2) {
        match &tokens[i] {
            Token::Identifier { string, .. } => class_path.push(string.to_string()),
            _ => return Some(error.set_position(tokens[i].position()).into())
        }

        if tokens[i + 1].to_string() != "::" {
            return Some(error.set_position(tokens[i + 1].position()).into());
        }
    }

    Some(Ok(Expr {
        expr_kind: ExprKind::ClassAttrRes { class_path, attr_name },
        expr_type: None,
        position: [
            tokens.first().unwrap().position(),
            tokens.last().unwrap().position()
        ]
    }))
}


fn parse_compound(tokens: &[Token]) -> ParseOption {
    /* Parses a compound expression */

    let comparison_operators = vec!["==", "!=", "<", ">", "<=", ">="];
    let arithmetic_operators = vec!["+", "-", "*", "/", "%"];
    let mut errors = Vec::new();

    for operator_list in [comparison_operators, arithmetic_operators] {
        for op in operator_list {
            for i in 1..(tokens.len() - 1) {
                if tokens[i].to_string() == op {
                    let left = parse_expr(&tokens[..i], tokens[0].position());
                    let right = parse_expr(&tokens[i+1..], tokens[i+1].position());
    
                    return Some(
                        if left.is_ok() && right.is_ok() {
                            let expr_kind = ExprKind::Compound {
                                operator: op.to_string(),
                                left: Box::new(left.unwrap()),
                                right: Box::new(right.unwrap())
                            };
    
                            Ok(Expr {
                                expr_kind,
                                expr_type: None,
                                position: [
                                    tokens.first().unwrap().position(),
                                    tokens.last().unwrap().position()
                                ]
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
        Some(if let Ok(expr) = parse_expr(&tokens[1..], tokens[1].position()) {
            Ok(Expr {
                expr_kind: ExprKind::Unary {
                    operator: String::from("-"),
                    operand: Box::new(expr)
                },
                expr_type: None,
                position: [
                    tokens.first().unwrap().position(),
                    tokens.last().unwrap().position()
                ]
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
                    match parse_expr(&contents[start..end], contents[start].position()) {
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

    let function = match parse_expr(&tokens[..tokens_len-1], tokens[0].position()) {
        Ok(fb) => Box::new(fb),
        err => return Some(err)
    };

    Some(Ok(
        Expr {
            expr_kind: ExprKind::FunctionCall { function, args },
            expr_type: None,
            position: [
                tokens.first().unwrap().position(),
                tokens.last().unwrap().position()
            ]
        }
    ))
}


fn parse_indexing(tokens: &[Token]) -> ParseOption {
    /* Parses an array indexing */

    let tokens_len = tokens.len();

    let index = match tokens.last().unwrap() {
        Token::Block { contents, open_delim, .. } => {
            if *open_delim == "[" && contents.len() > 0 {
                match parse_expr(contents, contents[0].position()) {
                    Ok(expr) => Box::new(expr),
                    err => return Some(err)
                }
            } else {
                return None;
            }
        },
        _ => return None
    };

    let array = match parse_expr(&tokens[..tokens_len-1], tokens[0].position()) {
        Ok(arr) => Box::new(arr),
        err => return Some(err)
    };

    Some(Ok(
        Expr {
            expr_kind: ExprKind::ArrayIndexing { array, index },
            expr_type: None,
            position: [
                tokens.first().unwrap().position(),
                tokens.last().unwrap().position()
            ]
        }
    ))
}


fn parse_object_attr_res(tokens: &[Token]) -> ParseOption {
    /* Parses an object attribute resolution */

    if tokens.len() < 3 || tokens[tokens.len() - 2].to_string() != "." {
        return None;
    }

    let mut errors = Vec::new();
    let mut parent = parse_expr(&tokens[..tokens.len()-2], tokens[0].position());
    let mut attr_name = match tokens.last().unwrap() {
        Token::Identifier { string, .. } => Ok(string.to_string()),
        token => Error::new(ErrorKind::SyntaxError)
                .set_position(token.position())
                .set_message("Expected syntax <expression>.<name>")
                .into()
    };

    if let Err(ref mut es) = parent {
        errors.append(es);
    }

    if let Err(ref mut es) = attr_name {
        errors.append(es);
    }

    Some(if errors.is_empty() {
        Ok(Expr {
            expr_kind: ExprKind::ObjectAttrRes {
                parent: Box::new(parent.unwrap()),
                attr_name: attr_name.unwrap()
            },
            expr_type: None,
            position: [
                tokens.first().unwrap().position(),
                tokens.last().unwrap().position()
            ]
        })
    } else {
        Err(errors)
    })
}
