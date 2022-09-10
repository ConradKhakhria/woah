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

    ConditionalBlock {
        conditional_blocks: Vec<(Expr, Vec<Statement>)>,
        else_block: Option<Vec<Statement>>
    },

    Declare {
        value_name: String,
        value_type: Option<Rc<TypeKind>>,
        value: Expr,
        constant: bool
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
    positions: [(usize, usize); 2]
}


impl Statement {
    fn from_line(line: &Line) -> Result<Self, Vec<Error>> {
        /* Creates a statement from a list of tokens */

        let parse_options = [
            parse_declare,
            parse_for_loop,
            parse_return,
            parse_while_loop,
            parse_assignment,
            parse_raw_expr
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


    pub fn first_position(&self) -> (usize, usize) {
        /* Gets the first position of the statement */

        self.positions[0].clone()
    }


    pub fn last_position(&self) -> (usize, usize) {
        /* Gets the last position of the statement */

        self.positions[1].clone()
    }
}


/* Parsing */

type ParseResult = Result<Statement, Vec<Error>>;


fn parse_declare(line: &Line) -> Option<ParseResult> {
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

    let mut assign_index = 0;

    for i in 2..tokens.len() {
        if tokens[i].to_string() == "=" {
            if assign_index == 0 {
                return Some(Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[i].position())
                        .set_message("Found 2 assignments in declaration")
                        .into())
            } else {
                assign_index = i;
            }
        }
    }

    let value_type = if tokens[2].to_string() == ":" {
        match parse_type_kind(&tokens[3..assign_index], tokens[3].position()) {
            Ok(tp) => Some(Rc::new(tp)),
            Err(ref mut es) => {
                errors.append(es);
                None
            }
        }
    } else {
        None
    };

    let mut value = Expr::from_tokens(
        &tokens[assign_index + 1..],
        tokens[assign_index + 1].position()
    );

    if let Err(ref mut es) = value {
        errors.append(es);
    }

    Some(if errors.is_empty() {
        Ok(Statement {
            stmt_type: StatementType::Declare {
                value_name,
                value_type,
                value: value.unwrap(),
                constant
            },
            positions: [
                line.first_position(),
                line.last_position()
            ]
        })
    } else {
        Err(errors)
    })
}


fn parse_assignment(line: &Line) -> Option<ParseResult> {
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
            positions: [
                line.first_position(),
                line.last_position()
            ]
        })
    } else {
        Err(errors)
    })
}


fn parse_conditional_block(conditionals: Vec<&Line>, else_statement: Option<&Line>) -> ParseResult {
    /* Parses an if/elif/while stmt */

    let mut conditional_blocks = vec![];
    let mut else_block = None;
    let mut errors: Vec<Error> = vec![];
    let positions = [
        conditionals.first().unwrap().first_position(),
        match else_statement {
            Some(line) => line.last_position(),
            None => conditionals.last().unwrap().last_position()
        }
    ];

    for conditional in conditionals {
        let condition = Expr::from_tokens(
            &conditional.line_tokens[1..],
            conditional.first_position()
        );

        let body = parse_statement_block(&conditional.line_derivs);

        match (condition, body) {
            (Ok(c), Ok(b)) => conditional_blocks.push((c, b)),
            (Err(ref mut es1), Err(ref mut es2)) => {
                errors.append(es1);
                errors.append(es2);
            },
            (Err(ref mut es), _) => errors.append(es),
            (_, Err(ref mut es)) => errors.append(es)
        }
    }

    if let Some(line) = else_statement {
        if line.line_tokens.len() != 1 {
            errors.push(
                Error::new(ErrorKind::SyntaxError)
                    .set_position(line.first_position())
                    .set_message("this line should only contain the word 'else'")
            );
        }

        match parse_statement_block(&line.line_derivs) {
            Ok(body) => else_block = Some(body),
            Err(ref mut es) => errors.append(es)
        }
    }

    if errors.is_empty() {
        Ok(Statement {
            stmt_type: StatementType::ConditionalBlock {
                conditional_blocks,
                else_block
            },
            positions
        })
    } else {
        Err(errors)
    }
}


fn parse_raw_expr(line: &Line) -> Option<ParseResult> {
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
            positions: [
                line.first_position(),
                line.last_position()
            ]
        }),
        Err(es) => Err(es)
    })
}


fn parse_for_loop(line: &Line) -> Option<ParseResult> {
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
                positions: [
                    line.first_position(),
                    line.last_position()
                ]
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


fn parse_return(line: &Line) -> Option<ParseResult> {
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
        positions: [
            line.first_position(),
            line.last_position()
        ]
    }))
}


fn parse_while_loop(line: &Line) -> Option<ParseResult> {
    /* Attempts to parse a while loop statemnet */

    let tokens = line.line_tokens;
    let mut errors = vec![];

    if tokens[0].to_string() != "while" {
        return None;
    }

    if tokens.len() == 1 {
        return Some(
            Error::new(ErrorKind::SyntaxError)
                .set_position(line.first_position())
                .set_message("while loop given empty condition")
                .into()
        );
    }

    let mut condition = Expr::from_tokens(&tokens[1..], tokens[1].position());
    let mut block = parse_statement_block(&line.line_derivs);

    if let Err(ref mut es) = condition {
        errors.append(es);
    }

    if let Err(ref mut es) = block {
        errors.append(es);
    }


    Some(
        if errors.is_empty() {
            Ok(Statement {
                stmt_type: StatementType::WhileLoop {
                    condition: condition.unwrap(),
                    block: block.unwrap()
                },
                positions: [
                    line.first_position(),
                    line.last_position()
                ]    
            })
        } else {
            Err(errors)
        }
    )
}


pub fn parse_statement_block(lines: &[Line]) -> Result<Vec<Statement>, Vec<Error>> {
    /* Parses a block of statements */

    let mut statements = vec![];
    let mut errors = vec![];

    let mut index = 0;

    while index < lines.len() {
        match lines[index].line_tokens[0].to_string().as_str() {
            "if" => {
                let mut conditionals = vec![ &lines[index] ];
                let mut else_statement = None;

                index += 1;

                while lines[index].line_tokens[0].to_string() == "elif" {
                    conditionals.push(&lines[index]);
                    index += 1;
                }

                if lines[index].line_tokens[0].to_string() == "else" {
                    else_statement = Some(&lines[index]);
                }

                index += 1;

                match parse_conditional_block(conditionals, else_statement) {
                    Ok(stmt) => statements.push(stmt),
                    Err(ref mut es) => errors.append(es)
                }
            },

            s@("elif"|"else") => {
                errors.push(
                    Error::new(ErrorKind::SyntaxError)
                        .set_position(lines[index].first_position())
                        .set_message(format!("'{}' statement has no preceding 'if' statement", s))
                );

                index += 1;
            },

            _ => {
                match Statement::from_line(&lines[index]) {
                    Ok(stmt) => statements.push(stmt),
                    Err(ref mut es) => errors.append(es)
                }

                index += 1;
            }
        }
    }

    if errors.is_empty() {
        Ok(statements)
    } else {
        Err(errors)
    }
}
