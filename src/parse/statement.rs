use crate::{
    message::{ Message, MsgKind },
    token::Token,
    line::Line,
    parse::{ Expr, ExprKind, TypeKind }
};
use std::rc::Rc;

#[derive(Debug)]
pub enum StatementType {
    Assign {
        assigned_to: Expr,
        new_value: Expr,
    },

    Declare {
        value_name: String,
        value_type: Option<Rc<TypeKind>>,
        value: Option<Expr>,
        constant: bool
    },

    Conditional {
        cases: Vec<(Expr, Vec<Statement>)>,
        default: Option<Vec<Statement>>
    },

    WhileLoop {
        condition: Expr,
        block: Vec<Statement>
    },

    // e.g. for i in 0 : 100
    RangeForLoop {
        iterator_name: String,
        start_value: Expr,
        end_value: Expr,
        step_value: Expr, // defaults to 1
        block: Vec<Statement>
    },

    // e.g. for x in xs
    IteratorForLoop {
        iterator_name: String,
        range: Expr,
        block: Vec<Statement>
    },

    Return {
        value: Option<Expr>
    },

    RawExpr {
        expr: Expr
    },
}

#[derive(Debug)]
pub struct Statement {
    pub stmt_type: StatementType,
    pub first_position: (usize, usize),
    pub last_position: (usize, usize)
}


impl<'s, 't> Statement {

    /* Instantiation */

    pub fn from_line(line: &Line) -> Result<Self, Vec<Message>> {
        /* Creates a statement from a list of tokens */

        let parse_options: [fn(&Line) -> Option<Result<Statement, Vec<Message>>>; 7] = [
            report_extraneous_elif_else,
            parse_declare,
            parse_for_loop,
            parse_while_loop,
            parse_return,
            parse_assignment,
            parse_expr
        ];
    
        for option in parse_options {
            if let Some(res) = option(line) {
                return res;
            }
        }
    
        Message::new(MsgKind::SyntaxError)
            .set_position(line.line_tokens[0].position())
            .set_message("Unrecognised syntax in statement")
            .into()
    }


    /* Display */

    pub fn display(&self, indent_level: usize) -> String {
        /* Displays a statement with a given indent level */

        let indent: String = vec![' '; indent_level].into_iter().collect();

        match &self.stmt_type {
            StatementType::Assign { assigned_to, new_value } => {
                format!("{}ASSIGN {} = {}\n", indent, assigned_to, new_value)
            },

            StatementType::Conditional { cases, default } => {
                let mut string = String::new();

                for (i, (condition, block)) in cases.iter().enumerate() {
                    let cond_tp = if i == 0 { "IF" } else { "ELIF" };

                    string = format!("{}{}{} {}\n", string, indent, cond_tp, condition);

                    for stmt in block.iter() {
                        string = format!("{}{}", string, stmt.display(indent_level + 4));
                    }
                }

                if let Some(stmts) = default {
                    string = format!("{}{}ELSE\n", string, indent);

                    for stmt in stmts.iter() {
                        string = format!("{}{}", string, stmt.display(indent_level + 4));
                    }
                }

                string
            }

            StatementType::Declare { value_name, value_type, value, constant } => {
                let type_string = match value_type {
                    Some(tp) => tp.to_string(),
                    None => "<no type given>".into()
                };

                let value_string = match value {
                    Some(v) => v.to_string(),
                    None => "<no value given>".into()
                };

                let constant_string = if *constant { "CONSTANT" } else { "VARIABLE" };

                format!("{}DECLARE {} {} : {} = {}\n", indent, constant_string, value_name, type_string, value_string)
            }

            StatementType::IteratorForLoop { iterator_name, range, block } => {
                let mut string = format!("{}FOR {} IN {}\n", indent, iterator_name, range);

                for stmt in block.iter() {
                    string = format!("{}{}", string, stmt.display(indent_level + 4));
                }

                string
            }

            StatementType::RangeForLoop { iterator_name, start_value, end_value, step_value, block } => {
                let mut string = format!(
                    "{}FOR {} IN RANGE (START = {}, END = {}, STEP = {})\n",
                    indent, iterator_name, start_value, end_value, step_value
                );

                for stmt in block.iter() {
                    string = format!("{}{}", string, stmt.display(indent_level + 4));
                }

                string
            }

            StatementType::RawExpr { expr } => format!("{}{}\n", indent, expr),

            StatementType::Return { value } => {
                if let Some(v) = value {
                    format!("{}RETURN {}\n", indent, v)
                } else {
                    format!("{}RETURN <nothing>\n", indent)
                }
            }

            StatementType::WhileLoop { condition, block } => {
                let mut string = format!("{}WHILE {}\n", indent, condition);

                for stmt in block.iter() {
                    string = format!("{}{}", string, stmt.display(indent_level + 4));
                }

                string
            }
        }
    }
}


impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.display(0))
    }
}


/* Parsing */

type ParseOption = Option<Result<Statement, Vec<Message>>>;


fn parse_declare(line: &Line) -> ParseOption {
    /* Parses a value declaration */

    let tokens = line.line_tokens;

    let constant = match tokens[0].to_string().as_str() {
        s@("let"|"var") => s == "let",
        _ => return None
    };

    if line.line_derivs.len() > 1 {
        return Some(Message::new(MsgKind::SyntaxError)
                        .set_position(line.line_derivs[0].line_tokens[0].position())
                        .set_message("Declarations cannot have a derivative block")
                        .into());
    }

    if tokens.len() < 4 {
        return Some(Message::new(MsgKind::SyntaxError)
                .set_position(tokens[0].position())
                .set_message("Declaration must have either a type annotation or a value")
                .into());
    }

    let value_name = match &tokens[1] {
        Token::Identifier { string, .. } => string.to_string(),
        _ => return None
    };

    let mut assign_index = None;
    let mut errors = Vec::new();

    for i in 0..tokens.len() {
        if tokens[i].to_string() == "=" {
            match assign_index {
                None => {
                    assign_index = Some(i);
                    break;
                },
    
                Some(_) => {
                    return Some(Message::new(MsgKind::SyntaxError)
                                    .set_position(tokens[i].position())
                                    .set_message("Found 2 assignments in declaration")
                                    .into())
                }
            }
        }
    }

    let value_type = if tokens[2].to_string() == ":" {
        match TypeKind::from_tokens(&tokens[3..assign_index.unwrap_or(tokens.len())]) {
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
            first_position: tokens.first().unwrap().position(),
            last_position: tokens.last().unwrap().position()
        })
    } else {
        Err(errors)
    })
}


fn parse_assignment(line: &Line) -> ParseOption {
    /* Parses a value assignment */

    let tokens = line.line_tokens;

    if line.line_derivs.len() != 0 {
        return Some(Message::new(MsgKind::SyntaxError)
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


fn report_extraneous_elif_else(line: &Line) -> ParseOption {
    /* Parses an else statement */

    let first_token_string = line.line_tokens[0].to_string();

    if first_token_string == "elif" || first_token_string == "else" {
        let err_res = Message::new(MsgKind::SyntaxError)
                        .set_position(line.line_tokens[0].position())
                        .set_message(format!("'{}' statement has no preceding 'if' statement", first_token_string))
                        .into();

        Some(err_res)
    } else {
        None
    }
}


fn parse_if_elif(line: &Line) -> Result<(Expr, Vec<Statement>), Vec<Message>> {
    /* Parses an individual if or elif statemnet */

    if line.line_tokens.len() < 2 {
        return Message::new(MsgKind::SyntaxError)
                    .set_position(line.line_tokens[0].position())
                    .set_message("Expected conditional expression in expression")
                    .into();
    }

    

    let mut condition = Expr::from_tokens(&line.line_tokens[1..], line.line_tokens[1].position());
    let mut body = parse_statement_block(&line.line_derivs);
    let mut errors = vec![];

    if let Err(ref mut es) = condition {
        errors.append(es);
    }

    if let Err(ref mut es) = body {
        errors.append(es);
    }

    if errors.is_empty() {
        Ok((condition.unwrap(), body.unwrap()))
    } else {
        Err(errors)
    }
}


fn parse_for_loop(line: &Line) -> ParseOption {
    /* Parses a for loop */

    let tokens = line.line_tokens;
    let format_err = Message::new(MsgKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("Expected syntax 'for <iterator> in <range>");

    let mut errors = Vec::new();

    if tokens[0].to_string() != "for" {
        return None;
    } else if tokens.len() < 4 {
        errors.push(format_err.clone());
    } else if line.line_derivs.len() == 0 {
        return Some(Message::new(MsgKind::SyntaxError)
                        .set_position(line.line_tokens.last().unwrap().position())
                        .set_message("For loops must have a block")
                        .into());
    }


    /* Iterator name */

    if errors.is_empty() && tokens[2].to_string() != "in" {
        errors.push(format_err);
    }

    let iterator_name = tokens[1].to_string();

    if let Token::Identifier {..} = &tokens[1] {} else {
        errors.push(Message::new(MsgKind::SyntaxError)
                        .set_position(tokens[1].position())
                        .set_message("Expected identifier"));
    }

    /* Loop body */

    let mut block_result = parse_statement_block(&line.line_derivs);

    if let Err(ref mut es) = block_result {
        errors.append(es);
    }
    
    /* Range or iterator */

    let colon_delimited = Token::split_tokens(&tokens[..], |t| t.to_string() == ":");
    let mut stmt_type = None;

    match colon_delimited[..] {
        [(mut start, end)] => {
            start += 3;

            match Expr::from_tokens(&tokens[start..end], tokens[3].position()) {
                Ok(range) => {
                    if let Ok(block) = block_result {
                        stmt_type = Some(StatementType::IteratorForLoop {
                            iterator_name,
                            range,
                            block
                        });
                    }
                },

                Err(ref mut es) => errors.append(es)
            }
        },

        [(mut s1, s2), (e1, e2)] => {
            s1 += 3;

            let mut range_errors = vec![];
            let mut start = Expr::from_tokens(&tokens[s1..s2], tokens[s1].position());
            let mut end = Expr::from_tokens(&tokens[e1..e2], tokens[e1].position());

            if let Err(ref mut es) = start {
                range_errors.append(es);
            }

            if let Err(ref mut es) = end {
                range_errors.append(es);
            }

            if range_errors.is_empty() {
                if let Ok(block) = block_result {
                    stmt_type = Some(StatementType::RangeForLoop {
                        iterator_name,
                        start_value: start.unwrap(),
                        end_value: end.unwrap(),
                        step_value: Expr {
                            expr_kind: ExprKind::Integer("1".into()),
                            expr_type: None,
                            first_position: (1, 1),
                            last_position: (1, 1)
                        },
                        block
                    });
                }
            } else {
                errors.append(&mut range_errors)
            }
        },

        [(mut s1, s2), (e1, e2), (stp1, stp2)] => {
            s1 += 3;

            let mut range_errors = vec![];
            let mut start = Expr::from_tokens(&tokens[s1..s2], tokens[s1].position());
            let mut end = Expr::from_tokens(&tokens[e1..e2], tokens[e1].position());
            let mut step = Expr::from_tokens(&tokens[stp1..stp2], tokens[stp1].position());

            for res in vec![ &mut start, &mut end, &mut step ] {
                if let Err(ref mut es) = res {
                    range_errors.append(es);
                }
            }

            if range_errors.is_empty() {
                if let Ok(block) = block_result {
                    stmt_type = Some(StatementType::RangeForLoop {
                        iterator_name,
                        start_value: start.unwrap(),
                        end_value: end.unwrap(),
                        step_value: step.unwrap(),
                        block
                    });
                }
            } else {
                errors.append(&mut range_errors)
            }
        },

        _ => {
            errors.push(Message::new(MsgKind::SyntaxError)
                            .set_position(tokens[3].position())
                            .set_message("Malformed syntax in for loop range"));
        }
    }

    if errors.is_empty() {
        Some(Ok(
            Statement {
                stmt_type: stmt_type.unwrap(),
                first_position: tokens[0].position(),
                last_position: tokens[2].position()
            }
        ))
    } else {
        Some(Err(errors))
    }
}


fn parse_return(line: &Line) -> ParseOption {
    /* Parses a return value */

    let tokens = line.line_tokens;

    if tokens[0].to_string() != "return" {
        return None;
    } else if line.line_derivs.len() != 0 {
        return Some(Message::new(MsgKind::SyntaxError)
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
        first_position: tokens.first().unwrap().position(),
        last_position: tokens.last().unwrap().position()
    }))
}


fn parse_expr(line: &Line) -> ParseOption {
    /* Parses a raw expression statement */

    let tokens = line.line_tokens;

    if line.line_derivs.len() > 0 {
        return Some(Message::new(MsgKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("A raw expr statement cannot have a block")
                        .into());
    }

    Some(match Expr::from_tokens(tokens, tokens[0].position()) {
        Ok(expr) => Ok(Statement {
            stmt_type: StatementType::RawExpr { expr },
            first_position: tokens.first().unwrap().position(),
            last_position: tokens.last().unwrap().position()
        }),
        Err(es) => Err(es)
    })
}


fn parse_while_loop(line: &Line) -> ParseOption {
    /* Parses a while loop */

    let tokens = line.line_tokens;
    let mut errors: Vec<Message> = Vec::new();

    if tokens[0].to_string().as_str() != "while" {
        return None;
    }

    if line.line_derivs.is_empty() {
        return Some(Message::new(MsgKind::SyntaxError)
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

    Some(if errors.is_empty() {
        Ok(Statement {
            stmt_type: StatementType::WhileLoop {
                condition: condition.unwrap(),
                block: block.unwrap()
            },
            first_position: tokens[0].position(),
            last_position: line.line_derivs.last().unwrap().line_tokens.last().unwrap().position() // vile
        })
    } else {
        Err(errors)
    })
}


pub fn parse_statement_block(lines: &[Line]) -> Result<Vec<Statement>, Vec<Message>> {
    /* Parses a block of statements */

    let mut statements = vec![];
    let mut errors = vec![];
    let mut index = 0;

    // lands on each statement structure
    while index < lines.len() {
        let first_token = &lines[index].line_tokens[0];

        // collect a whole conditional block
        if first_token.to_string() == "if" {
            let mut cases = vec![];
            let mut default = None;
            let first_position = lines[index].line_tokens[0].position();

            match parse_if_elif(&lines[index]) {
                Ok(pair) => cases.push(pair),
                Err(ref mut es) => errors.append(es)
            }

            index += 1;

            while lines[index].line_tokens[0].to_string() == "elif" {
                match parse_if_elif(&lines[index]) {
                    Ok(pair) => cases.push(pair),
                    Err(ref mut es) => errors.append(es)
                }

                index += 1;
            }

            if lines[index].line_tokens[0].to_string() == "else" {
                if lines[index].line_tokens.len() > 1 {
                    errors.push(Message::new(MsgKind::SyntaxError)
                                    .set_position(lines[index].line_tokens[0].position())
                                    .set_message("Else statement cannot have an expression"));
                }

                match parse_statement_block(&lines[index].line_derivs) {
                    Ok(stmts) => default = Some(stmts),
                    Err(ref mut es) => errors.append(es)
                }

                index += 1;
            }

            statements.push(
                Statement {
                    stmt_type: StatementType::Conditional {
                        cases,
                        default
                    },
                    first_position,
                    last_position: lines[index - 1].line_tokens[0].position()
                }
            );
        } else {
            match Statement::from_line(&lines[index]) {
                Ok(stmt) => statements.push(stmt),
                Err(ref mut es) => errors.append(es)
            }

            index += 1;
        }
    }

    if errors.is_empty() {
        Ok(statements)
    } else {
        Err(errors)
    }
}
