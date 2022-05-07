use crate::{
    error::{ Error, ErrorKind },
    line::Line,
    parse::{ parse_statement_block, Statement, TypeKind },
    token::Token
};
use std::rc::Rc;

#[derive(Debug)]
pub struct Function<'s, 't> {
    pub name: &'t Token<'s>,
    pub args: Vec<Argument<'s, 't>>,
    pub return_type: Option<Rc<TypeKind>>,
    pub body: Vec<Statement<'s, 't>>
}


impl<'s, 't> Function<'s, 't> {
    pub fn from_line(line: &Line<'s, 't>) -> Result<Function<'s, 't>, Vec<Error>> {
        /* Parses a function from a nested line */

        let tokens = line.line_tokens;
        let mut errors = vec![];

        let format_error = Error::new(ErrorKind::SyntaxError)
                                    .set_position(tokens[0].position())
                                    .set_message("Invalid function definition syntax");


        if tokens.len() < 3 || tokens[0].to_string() != "def" || line.line_derivs.is_empty() {
            errors.push(format_error.clone());
        }

        /* Get function name */

        let name = &tokens[1];

        if let Token::Identifier {..} = name {
            // ok
        } else if errors.len() == 0 {
            errors.push(format_error.clone());
        }

        /* Get function arguments */

        let args = match parse_arguments(&tokens[2]) {
            Ok(args) => args,
            Err(ref mut es) => {
                errors.append(es);
                vec![] // placeholder
            }
        };

        /* Get function return type */

        let return_type = if tokens.len() > 3 {
            match TypeKind::from_tokens(&tokens[3..]) {
                Ok(tp) => Some(tp.rc()),
    
                Err(ref mut es) => {
                    errors.append(es);
                    None
                }
            }
        } else {
            None
        };

        /* Get function body */

        let mut body = parse_statement_block(&line.line_derivs);

        if let Err(ref mut es) = body {
            errors.append(es);
        }

        if errors.is_empty() {
            Ok(Function {
                name,
                args,
                return_type,
                body: body.unwrap()
            })
        } else {
            Err(errors)
        }
    }
}

impl<'s, 't> Into<TypeKind> for Function<'s, 't> {
    fn into(self) -> TypeKind {
        let mut args = vec![];

        for arg in self.args.iter() {
            args.push(Rc::clone(&arg.arg_type));
        }
    
        TypeKind::Function {
            args,
            return_type: self.return_type
        }
    }
}


impl<'s, 't> Into<TypeKind> for &Function<'s, 't> {
    fn into(self) -> TypeKind {
        let mut args = vec![];

        for arg in self.args.iter() {
            args.push(Rc::clone(&arg.arg_type));
        }
    
        TypeKind::Function {
            args,
            return_type: if let Some(r) = &self.return_type {
                Some(Rc::clone(r))
            } else {
                None
            }
        }
    }
}


/* Function arguments */


fn parse_arguments<'s, 't>(args_block: &'t Token<'s>) -> Result<Vec<Argument<'s, 't>>, Vec<Error>> {
   /* Attempts to parse a list of function arguments */

    let mut args = vec![];
    let mut errors = vec![];

    let args_block = match args_block {
        Token::Block { contents, open_delim, .. } => {
            if *open_delim == "(" {
                contents
            } else {
                return Error::new(ErrorKind::SyntaxError)
                            .set_position(args_block.position())
                            .set_message("Expected a '('-delimited list of arguments")
                            .into();
            }
        },

        _ => return Error::new(ErrorKind::SyntaxError)
                        .set_position(args_block.position())
                        .set_message("Expected a list of arguments")
                        .into()
    };

    let arg_indices = Token::split_tokens(&args_block[..], |t| t.to_string() == ",");

    for (start, end) in arg_indices {
        match Argument::from_tokens(&args_block[start..end]) {
            Ok(arg) => args.push(arg),
            Err(ref mut es) => errors.append(es)
        }
    }

    if errors.is_empty() {
        Ok(args)
    } else {
        Err(errors)
    }
}

#[derive(Debug)]
pub struct Argument<'s, 't> {
    pub arg_name: &'t Token<'s>,
    pub arg_type: Rc<TypeKind>
}


impl<'s, 't> Argument<'s, 't> {
    fn from_tokens(tokens: &'t [Token<'s>]) -> Result<Argument<'s, 't>, Vec<Error>> {
        /* Parses an argument */

        let arg_name = &tokens[0];

        if let Token::Identifier {..} = arg_name { } else {
            return Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("Expected an argument name")
                        .into();
        }

        if tokens.len() < 3 || tokens[1].to_string() != ":" {
            return Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("Expected syntax <argument name> : <argument type>")
                        .into();
        }

        let arg_type = TypeKind::from_tokens(&tokens[2..])?.rc();

        Ok(Argument { arg_name, arg_type })
    }
}
