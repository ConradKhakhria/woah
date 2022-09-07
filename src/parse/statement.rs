use crate::error::*;
use crate::line::Line;
use crate::parse::Expr;
use crate::parse::ExprKind;
use crate::parse::parse_type_kind;
use crate::parse::TypeKind;
use crate::token::Token;
use std::rc::Rc;


#[derive(Debug)]
pub enum StatementType {
    Assign {
        assigned_to: Expr,
        new_value: Expr,
    },

    Conditional {
        condition: Expr,
        block: Vec<Statement>,
        is_if: bool
    },

    Declare {
        value_name: String,
        value_type: Option<Rc<TypeKind>>,
        value: Option<Expr>,
        constant: bool
    },

    Else {
        block: Vec<Statement>
    },

    IteratorForLoop {
        iterator_name: String,
        range: Expr,
        block: Vec<Statement>
    },

    NumericRangeForLoop {
        iterator_name: String,
        start: Expr,
        end: Expr,
        step: Expr,
        block: Vec<Statement>
    },

    RawExpr {
        expr: Expr
    },

    Return {
        value: Option<Expr>
    },

    WhileLoop {
        condition: Expr,
        block: Vec<Statement>
    }
}

#[derive(Debug)]
pub struct Statement {
    pub stmt_type: StatementType,
    pub first_position: (usize, usize),
    pub last_position: (usize, usize)
}


impl Statement {
    pub fn from_line(line: &Line) -> Result<Self, Vec<Error>> {
        /* Creates a statement from a list of tokens */

        let parse_options = [
            parse_declare,
            parse_conditional,
            parse_else,
            parse_for_loop,
            parse_return,
            parse_assignment,
            parse_expr
        ];
    
        for option in parse_options {
            if let Some(res) = option(line) {
                return res;
            }
        }
    
        Error::new(ErrorKind::SyntaxError)
            .set_position(line.line_tokens[0].position())
            .set_message("Unrecognised syntax in statement")
            .into()
    }
}


/* Parsing */


fn parse_declare(line: &Line) -> Option<Result<Statement, Vec<Error>>> {
    /* Parses a value declaration */

    let tokens = line.line_tokens;

    let constant = match tokens[0].to_string().as_str() {
        s@("let"|"var") => s == "let",
        _ => return None
    };

    if line.line_derivs.len() > 1 {
        return Some(Error::new(ErrorKind::SyntaxError)
                        .set_position(line.line_derivs[0].line_tokens[0].position())
                        .set_message("Declarations cannot have a derivative block")
                        .into());
    }

    if tokens.len() < 4 {
        return Some(Error::new(ErrorKind::SyntaxError)
                .set_position(tokens[0].position())
                .set_message("Declaration must have either a type annotation or a value")
                .into());
    }

    let mut errors = Vec::new();

    let value_name = if tokens[1].lower_case() {
        tokens[1].to_string()
    } else {
        errors.push(
            Error::new(ErrorKind::SyntaxError)
                .set_position(tokens[1].position())
                .set_message("expected a (lower-case) variable name")
        );
        String::new()
    };

    let mut assign_index = None;

    for i in 0..tokens.len() {
        if tokens[i].to_string() == "=" {
            match assign_index {
                None => {
                    assign_index = Some(i);
                    break;
                },
    
                Some(_) => {
                    return Some(Error::new(ErrorKind::SyntaxError)
                                    .set_position(tokens[i].position())
                                    .set_message("Found 2 assignments in declaration")
                                    .into())
                }
            }
        }
    }

    let value_type = if tokens[2].to_string() == ":" {
        match parse_type_kind(&tokens[3..assign_index.unwrap_or(tokens.len())], tokens[3].position()) {
            Ok(tp) => Some(Rc::new(tp)),
            Err(ref mut es) => {
                errors.append(es);
                None
            }
        }
    } else {
        None
    };

    let value = match assign_index {
        Some(i) => {
            match Expr::from_tokens(&tokens[i+1..], tokens[i+1].position()) {
                Ok(expr) => Some(expr),
                Err(ref mut es) => {
                    errors.append(es);
                    None
                }
            }
        },

        None => None
    };

    Some(if errors.is_empty() {
        Ok(Statement {
            stmt_type: StatementType::Declare { value_name, value_type, value, constant },
            first_position: tokens[0].position(),
            last_position: tokens.last().unwrap().position()
        })
    } else {
        Err(errors)
    })
}


fn parse_assignment(line: &Line) -> Option<Result<Statement, Vec<Error>>> {
    /* Parses a value assignment */

    let tokens = line.line_tokens;

    if line.line_derivs.len() != 0 {
        return Some(Error::new(ErrorKind::SyntaxError)
                        .set_position(line.line_derivs[0].line_tokens[0].position())
                        .set_message("Assignments cannot have a derivative block")
                        .into());
    }

    let mut eq_index = 0;
    let mut errors = Vec::new();

    loop {
        if eq_index >= tokens.len() {
            return None;
        } else if tokens[eq_index].to_string() == "=" {
            break;
        } else {
            eq_index += 1;
        }
    }

    let mut assigned_to = Expr::from_tokens(&tokens[..eq_index], tokens[0].position());
    let mut new_value = Expr::from_tokens(&tokens[eq_index+1..], tokens[eq_index + 1].position());

    if let Err(ref mut es) = &mut assigned_to {
        errors.append(es);
    }

    if let Err(ref mut es) = &mut new_value {
        errors.append(es);
    }

    Some(if errors.is_empty() {
        Ok(Statement {
            stmt_type: StatementType::Assign {
                assigned_to: assigned_to.unwrap(),
                new_value: new_value.unwrap()
            },
            first_position: tokens.first().unwrap().position(),
            last_position: tokens.last().unwrap().position()
        })
    } else {
        Err(errors)
    })
}


fn parse_conditional(line: &Line) -> Option<Result<Statement, Vec<Error>>> {
    /* Parses an if/elif/while stmt */

    let tokens = line.line_tokens;
    let mut errors: Vec<Error> = Vec::new();

    let conditional_type = match tokens[0].to_string().as_str() {
        "if"|"elif"|"while" => tokens[0].to_string(),
        _ => return None
    };

    if line.line_derivs.is_empty() {
        return Some(Error::new(ErrorKind::SyntaxError)
                        .set_position(line.line_tokens.last().unwrap().position())
                        .set_message("Conditional statements must have a block")
                        .into());
    }

    let mut condition = Expr::from_tokens(&tokens[1..], tokens[1].position());
    let mut block = parse_statement_block(&line.line_derivs);

    if let Err(ref mut es) = condition {
        errors.append(es);
    }

    if let Err(ref mut es) = block {
        errors.append(es);
    }

    Some(if !errors.is_empty() {
        Err(errors)
    } else if conditional_type == "while" {
        Ok(Statement {
            stmt_type: StatementType::WhileLoop {
                condition: condition.unwrap(),
                block: block.unwrap()
            },
            first_position: tokens[0].position(),
            last_position: tokens[2].position()
        })
    } else {
        Ok(Statement {
            stmt_type: StatementType::Conditional {
                condition: condition.unwrap(),
                block: block.unwrap(),
                is_if: conditional_type == "if"
            },
            first_position: tokens[0].position(),
            last_position: line.line_derivs.last().unwrap().line_tokens.last().unwrap().position() // vile
        })
    })
}


fn parse_else(line: &Line) -> Option<Result<Statement, Vec<Error>>> {
    /* Parses an else statement */

    let tokens = line.line_tokens;

    if tokens[0].to_string() != "else" {
        return None;
    }

    if tokens.len() != 1 {
        return Some(Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("Expected syntax 'else {<body>}")
                        .into());
    } else if line.line_derivs.is_empty() {
        return Some(Error::new(ErrorKind::SyntaxError)
                        .set_position(line.line_tokens.last().unwrap().position())
                        .set_message("Conditional statements must have a block")
                        .into());
    }

    Some(match parse_statement_block(&line.line_derivs) {
        Ok(block) => Ok(Statement {
            stmt_type: StatementType::Else { block },
            first_position: tokens[0].position(),
            last_position: line.line_derivs.last().unwrap().line_tokens.last().unwrap().position()
        }),

        Err(es) => Err(es)
    })
}


fn parse_for_loop(line: &Line) -> Option<Result<Statement, Vec<Error>>> {
    /* Parses a for loop */

    let tokens = line.line_tokens;
    let format_err = Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("Expected syntax 'for <iterator> in <range>");

    let mut errors = Vec::new();

    if tokens[0].to_string() != "for" {
        return None;
    } else if tokens.len() < 4 {
        errors.push(format_err.clone());
    } else if line.line_derivs.len() == 0 {
        return Some(Error::new(ErrorKind::SyntaxError)
                        .set_position(line.line_tokens.last().unwrap().position())
                        .set_message("For loops must have a block")
                        .into());
    }

    /* Iterator and range */

    if errors.is_empty() && tokens[2].to_string() != "in" {
        errors.push(format_err);
    }

    let iterator_name = if tokens[1].lower_case() {
        tokens[1].to_string()
    } else {
        errors.push(
            Error::new(ErrorKind::SyntaxError)
                .set_position(tokens[1].position())
                .set_message("expected (lower-case) name for for loop iterator")
        );
        String::new()
    };

    let mut range = parse_for_loop_range(&tokens[3..]);

    /* For loop body */

    let mut block = parse_statement_block(&line.line_derivs);

    /* Error recording */

    if let Err(ref mut es) = range {
        errors.append(es);
    }

    if let Err(ref mut es) = block {
        errors.append(es);
    }

    if !errors.is_empty() {
        return Some(Err(errors));
    } else {
        Some(Ok(
            Statement {
                stmt_type: match range.unwrap() {
                    Ok([start, end, step]) => {
                        StatementType::NumericRangeForLoop {
                            iterator_name,
                            start,
                            end,
                            step,
                            block: block.unwrap()
                        }
                    },

                    Err(range) => {
                        StatementType::IteratorForLoop {
                            iterator_name,
                            range,
                            block: block.unwrap()
                        }
                    }
                },
                first_position: tokens[0].position(),
                last_position: tokens.last().unwrap().position()
            }
        ))
    }
}


fn parse_for_loop_range(tokens: &[Token]) -> Result<Result<[Expr; 3], Expr>, Vec<Error>> {
   /* Parses the range of a for loop
    *
    * returns:
    * - Ok(Ok([start, end, step])) for numeric range for loops
    * - Ok(Err(range)) for iterator for loops
    * - Err(es) for errors
    */

    match Token::split_tokens(tokens, |t| t.to_string() == ":").as_slice() {
        [_] => {
            match Expr::from_tokens(tokens, tokens[0].position()) {
                Ok(range) => Ok(Err(range)),
                Err(es) => Err(es)
            }
        },

        [start, end] => {
            let start = Expr::from_tokens(&tokens[start.0..start.1], tokens[0].position())?;
            let end = Expr::from_tokens(&tokens[end.0..end.1], tokens[0].position())?;
            let step = Expr {
                expr_kind: ExprKind::Integer(1),
                expr_type: Some(TypeKind::Int.rc()),
                first_position: tokens[0].position(),
                last_position: tokens[0].position()
            };

            Ok(Ok([start, end, step]))
        },

        [start, end, step] => {
            let start = Expr::from_tokens(&tokens[start.0..start.1], tokens[0].position())?;
            let end = Expr::from_tokens(&tokens[end.0..end.1], tokens[0].position())?;
            let step = Expr::from_tokens(&tokens[step.0..step.1], tokens[0].position())?;

            Ok(Ok([start, end, step]))
        },

        _ => {
            Error::new(ErrorKind::SyntaxError)
                .set_position(tokens[0].position())
                .set_message("unrecognised syntax in for loop (too many colons)")
                .into()
        }
    }
}


fn parse_return(line: &Line) -> Option<Result<Statement, Vec<Error>>> {
    /* Parses a return value */

    let tokens = line.line_tokens;

    if tokens[0].to_string() != "return" {
        return None;
    } else if line.line_derivs.len() != 0 {
        return Some(Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("A return statement cannot have a block")
                        .into());
    }

    let value = match &tokens[1..] {
        [] => None,
        ts => {
            match Expr::from_tokens(ts, ts[0].position()) {
                Ok(expr) => Some(expr),
                Err(es) => return Some(Err(es))
            }
        }
    };

    Some(Ok(Statement {
        stmt_type: StatementType::Return { value },
        first_position: tokens[0].position(),
        last_position: tokens.last().unwrap().position()
    }))
}


fn parse_expr(line: &Line) -> Option<Result<Statement, Vec<Error>>> {
    /* Parses a raw expression statement */

    let tokens = line.line_tokens;

    if line.line_derivs.len() > 0 {
        return Some(Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("A raw expr statement cannot have a block")
                        .into());
    }

    Some(match Expr::from_tokens(tokens, tokens[0].position()) {
        Ok(expr) => Ok(Statement {
            stmt_type: StatementType::RawExpr { expr },
            first_position: tokens[0].position(),
            last_position: tokens.last().unwrap().position()
        }),
        Err(es) => Err(es)
    })
}


pub fn parse_statement_block(lines: &[Line]) -> Result<Vec<Statement>, Vec<Error>> {
    /* Parses a block of statements */

    let mut statements = vec![];
    let mut errors = vec![];

    for line in lines.iter() {
        match Statement::from_line(line) {
            Ok(stmt) => statements.push(stmt),
            Err(ref mut es) => errors.append(es)
        }
    }

    if errors.is_empty() {
        Ok(statements)
    } else {
        Err(errors)
    }
}
