use crate::{
    error::{ Error, ErrorKind },
    token::Token,
    parse::{ Expr, ExprType, parse_expression, TypeKind }
};

#[derive(Debug)]
pub enum StatementType<'s, 't> {
    Assign {
        assigned_to: Expr<'s, 't>,
        new_value: Expr<'s, 't>,
    },

    Declare {
        value_name: &'t Token<'s>,
        value_type: Option<TypeKind<'s, 't>>,
        value: Option<Expr<'s, 't>>,
        constant: bool
    },

    Conditional {
        condition: Expr<'s, 't>,
        block: Vec<Statement<'s, 't>>,
        is_if: bool
    },

    Else {
        block: Vec<Statement<'s, 't>>
    },

    WhileLoop {
        condition: Expr<'s, 't>,
        block: Vec<Statement<'s, 't>>
    },

    ForLoop {
        iterator_name: &'t Token<'s>,
        range: Expr<'s, 't>,
        block: Vec<Statement<'s, 't>>
    },

    Return {
        value: Option<Expr<'s, 't>>
    },

    RawExpr {
        expr: Expr<'s, 't>
    }
}

#[derive(Debug)]
pub struct Statement<'s, 't> {
    stmt_type: StatementType<'s, 't>,
    first_token: &'t Token<'s>,
    last_token: &'t Token<'s>
}


impl<'s, 't> Statement<'s, 't> {
    pub fn from_tokens(tokens: &'t [Token<'s>], position: (usize, usize)) -> Result<Self, Vec<Error>> {
        /* Creates a statement from a list of tokens */

        if tokens.is_empty() {
            return Error::new(ErrorKind::SyntaxError)
                    .set_position(position)
                    .set_message("Attempt to parse empty statement")
                    .into();
        }
    
        for option in PARSE_OPTIONS {
            if let Some(res) = option(tokens) {
                return res;
            }
        }
    
        Error::new(ErrorKind::SyntaxError)
            .set_position(tokens[0].position())
            .set_message("Unrecognised syntax in statement")
            .into()
    }
}


/* Parsing */

type ParseOption<'s, 't> = Option<Result<Statement<'s, 't>, Vec<Error>>>;


fn parse_declare<'s, 't>(tokens: &'t [Token<'s>]) -> ParseOption<'s, 't> {
    /* Parses a value declaration */

    let constant = if tokens[0].str_equals("let") {
        true
    } else if tokens[0].str_equals("var") {
        false
    } else {
        return None;
    };

    if tokens.len() < 4 {
        return Some(Error::new(ErrorKind::SyntaxError)
                .set_position(tokens[0].position())
                .set_message("Declaration must have either a type annotation or a value")
                .into());
    }

    let value_name = match &tokens[1] {
        tok@Token::Identifier { .. } => tok,
        _ => return None
    };

    let mut assign_index = None;
    let mut errors = Vec::new();

    for i in 0..tokens.len() {
        if tokens[i].str_equals("=") {
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

    let value_type = if tokens[2].str_equals(":") {
        match TypeKind::from_tokens(&tokens[3..assign_index.unwrap_or(tokens.len())]) {
            Ok(tp) => Some(tp),
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
            match parse_expression(&tokens[i+1..], tokens[i+1].position()) {
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
            first_token: &tokens[0],
            last_token: tokens.last().unwrap()
        })
    } else {
        Err(errors)
    })
}


fn parse_assignment<'s, 't>(tokens: &'t [Token<'s>]) -> ParseOption<'s, 't> {
    /* Parses a value assignment */

    let mut eq_index = 0;
    let mut errors = Vec::new();

    loop {
        if eq_index >= tokens.len() {
            return None;
        } else if tokens[eq_index].str_equals("=") {
            break;
        } else {
            eq_index += 1;
        }
    }

    let mut assigned_to = parse_expression(&tokens[..eq_index], tokens[0].position());
    let mut new_value = parse_expression(&tokens[eq_index+1..], tokens[eq_index + 1].position());

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
            first_token: tokens.first().unwrap(),
            last_token: tokens.last().unwrap()
        })
    } else {
        Err(errors)
    })
}


fn parse_conditional<'s, 't>(tokens: &'t [Token<'s>]) -> ParseOption<'s, 't> {
    /* Parses an if/elif/while stmt */

    let mut errors: Vec<Error> = Vec::new();

    let condiitonal_type = if tokens[0].str_equals("if") {
        "if"
    } else if tokens[0].str_equals("elif") {
        "elif"
    } else if tokens[0].str_equals("while") {
        "while"
    } else {
        return None
    };

    let format_err = Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message(
                            format!(
                                "Expected syntax '{} (<condition>) {{<body>}}",
                                condiitonal_type
                            )
                        );

    if tokens.len() != 3 {
        return Some(format_err.into());
    }


    let mut condition = parse_expression(&tokens[1..2], tokens[1].position());
    let mut block = match &tokens[2] {
        Token::Block { contents, .. } => {
            if tokens[2].delim_equals("{") {
                parse_block(contents)
            } else {
                format_err.into()
            }
        },
        _ => format_err.into()
    };

    if let Err(ref mut es) = condition {
        errors.append(es);
    }

    if let Err(ref mut es) = block {
        errors.append(es);
    }

    Some(if !errors.is_empty() {
        Err(errors)
    } else if condiitonal_type == "while" {
        Ok(Statement {
            stmt_type: StatementType::WhileLoop {
                condition: condition.unwrap(),
                block: block.unwrap()
            },
            first_token: &tokens[0],
            last_token: &tokens[2]
        })
    } else {
        Ok(Statement {
            stmt_type: StatementType::Conditional {
                condition: condition.unwrap(),
                block: block.unwrap(),
                is_if: condiitonal_type == "if"
            },
            first_token: &tokens[0],
            last_token:  &tokens[2]
        })
    })
}


fn parse_else<'s, 't>(tokens: &'t [Token<'s>]) -> ParseOption<'s, 't> {
    /* Parses an else statement */

    if !tokens[0].str_equals("else") {
        return None;
    }

    let format_err = Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("Expected syntax 'else {<body>}");

    if tokens.len() != 2 {
        return Some(format_err.into());
    }

    let block_res = match &tokens[1] {
        Token::Block { contents, .. } => {
            if tokens[1].delim_equals("{") {
                parse_block(contents)
            } else {
                format_err.into()
            }
        },
        _ => format_err.into()
    };

    Some(match block_res {
        Ok(block) => Ok(Statement {
            stmt_type: StatementType::Else { block },
            first_token: &tokens[0],
            last_token:  &tokens[1]
        }),

        Err(es) => Err(es)
    })
}


fn parse_for_loop<'s, 't>(tokens: &'t [Token<'s>]) -> ParseOption<'s, 't> {
    /* Parses a for loop */

    if !tokens[0].str_equals("for") {
        return None;
    }

    let format_err = Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("Expected syntax 'for (<iterator> in <range>) {<body>}");

    if tokens.len() != 3 {
        return Some(format_err.into());
    }

    let inner_tokens = match &tokens[1] {
        Token::Block { open_delim, contents, .. } => {
            if *open_delim == "(" && contents.len() >= 3 {
                contents
            } else {
                return Some(format_err.into());
            }
        },

        _ => return Some(format_err.into())
    };

    let mut block = match &tokens[2] {
        Token::Block { open_delim, contents, .. } => {
            if *open_delim == "{" {
                parse_block(&contents[..])
            } else {
                return Some(format_err.into());
            }
        },

        _ => return Some(format_err.into())
    };

    let mut errors = Vec::new();

    if errors.is_empty() && !inner_tokens[1].str_equals("in") {
        errors.push(format_err);
    }

    let iterator_name = &inner_tokens[0];
    let mut range = parse_expression(&inner_tokens[2..], tokens[2].position());

    if let Err(ref mut es) = range {
        errors.append(es);
    }

    if let Err(ref mut es) = block {
        errors.append(es);
    }

    if errors.is_empty() {
        Some(Ok(
            Statement {
                stmt_type: StatementType::ForLoop {
                    iterator_name,
                    range: range.unwrap(),
                    block: block.unwrap()
                },
                first_token: &tokens[0],
                last_token:  &tokens[2]
            }
        ))
    } else {
        Some(Err(errors))
    }
}


fn parse_return<'s, 't>(tokens: &'t [Token<'s>]) -> ParseOption<'s, 't> {
    /* Parses a return value */

    if !tokens[0].str_equals("return") {
        return None;
    }

    let value = match &tokens[1..] {
        [] => None,
        ts => {
            match parse_expression(ts, ts[0].position()) {
                Ok(expr) => Some(expr),
                Err(es) => return Some(Err(es))
            }
        }
    };

    Some(Ok(Statement {
        stmt_type: StatementType::Return { value },
        first_token: &tokens[0],
        last_token: tokens.last().unwrap()
    }))
}


fn parse_expr<'s, 't>(tokens: &'t [Token<'s>]) -> ParseOption<'s, 't> {
    /* Parses a raw expression statement */

    Some(match parse_expression(tokens, tokens[0].position()) {
        Ok(expr) => Ok(Statement {
            stmt_type: StatementType::RawExpr { expr },
            first_token: &tokens[0],
            last_token: tokens.last().unwrap()
        }),
        Err(es) => Err(es)
    })
}


// For some reason, rust doesn't like having a list of func ptrs with lifetimes attached to them
// as a function variable
const PARSE_OPTIONS: [for<'t, 's> fn(&'t [Token<'s>]) -> Option<Result<Statement<'s, 't>, Vec<Error>>>; 7] = [
    parse_declare,
    parse_assignment,
    parse_conditional,
    parse_else,
    parse_for_loop,
    parse_return,
    parse_expr
];


pub fn parse_block<'s, 't>(tokens: &'t [Token<'s>]) -> Result<Vec<Statement<'s, 't>>, Vec<Error>> {
    /* Creates a list of all token slices which contains statements */

    let mut statements = Vec::new();
    let mut errors = Vec::new();

    let slices = Token::split_tokens(tokens, |t| t.str_equals(";") || t.delim_equals("{"));

    for (i, (start, mut end)) in slices.iter().enumerate() {
        if i + 1 == slices.len() {
            if *start < tokens.len() {
                errors.push(
                    Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[*start].position())
                        .set_message("Statement doesn't end in a semicolon or '{'-enclosed block")
                );
            }

            break;
        }

        if end >= tokens.len() {
            end -= 1;
        }

        let stmt_slice = if tokens[end].atom() {
            &tokens[*start..end]
        } else {
            &tokens[*start..=end]
        };

        match Statement::from_tokens(stmt_slice, tokens[*start].position()) {
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
