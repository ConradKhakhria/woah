use crate::error::*;
use crate::line::Line;
use crate::parse::parse_statement_block;
use crate::parse::parse_type_kind;
use crate::parse::Statement;
use crate::parse::TypeKind;
use crate::token::Token;
use std::rc::Rc;


#[derive(Debug)]
pub struct Function {
    pub public: bool,
    pub variable_instance_method: Option<bool>,
    pub name: String,
    pub args: Vec<Argument>,
    pub return_type: Rc<TypeKind>,
    pub body: Vec<Statement>,
    pub positions: [(usize, usize); 2],
}


impl Function {

    /* Utils */

    pub fn first_position(&self) -> (usize, usize) {
        /* Gets the first position of the function */

        self.positions[0].clone()
    }


    pub fn last_position(&self) -> (usize, usize) {
        /* Gets the last position of the function */

        self.positions[1].clone()
    }

    /* Parse function */

    pub fn parse_function(line: &Line) -> Result<Function, Vec<Error>> {
        /* Parses a function from a line */

        // Function filled with default values to be modified
        let mut func = Function {
            public: false,
            variable_instance_method: None,
            name: String::new(),
            args: vec![],
            return_type: TypeKind::NoneType.rc(),
            body: vec![],
            positions: [ line.first_position(), line.last_position() ]
        };

        match func.parse_function_declaration(line.line_tokens)? {
            Some(i) => {
                if line.line_derivs.is_empty() {
                    let inline_body_virtual_line = Line {
                        line_tokens: &line.line_tokens[i+1..],
                        line_derivs: vec![]
                    };

                    func.body = parse_statement_block(&vec![ inline_body_virtual_line ])?;
                } else {
                    return Error::new(ErrorKind::SyntaxError)
                                .set_position(line.line_derivs[0].line_tokens.first().unwrap().position())
                                .set_message("a function cannot have an inline definition and indented block")
                                .into();
                }
            },
            
            None => func.body = parse_statement_block(&line.line_derivs)?,
        }

        Ok(func)
    }


    fn parse_function_args(&mut self, args_tokens: &Vec<Token>) -> Result<(), Vec<Error>> {
       /* Parses a list of function arguments
        *
        * returns
        * -------
        * a tuple containing:
        * - the function's arguments
        * - whether or not the function is an object method
        */

        if args_tokens.len() == 0 {
            return Ok(());
        }
 
        let mut errors = vec![];
        let args_slices = Token::split_tokens(&args_tokens, |t| t.to_string() == ",");
 
        for (start, end) in args_slices {
            if start == 0 {
                if end == 1 && args_tokens[0].to_string() == "self" {
                    self.variable_instance_method = Some(false);
                    continue;
                } else if end == 2 && args_tokens[0].to_string() == "var" && args_tokens[1].to_string() == "self" {
                    self.variable_instance_method = Some(true);
                    continue;
                }
            }
 
            match Argument::from_tokens(&args_tokens[start..end], args_tokens[0].position()) {
                Ok(a) => self.args.push(a),
                Err(ref mut es) => errors.append(es)
            }
        }
 
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
 

    fn parse_function_declaration(&mut self, tokens: &[Token]) -> Result<Option<usize>, Vec<Error>> {
       /* Parses the opening line of a function
        *
        * returns: a result of
        * - Some(..) of the location of the '=' symbol for an inline function body
        * - None if the function doesn't have an inline body
        */

        let mut index = 0;
        let mut errors = vec![];

        let format_error = Error::new(ErrorKind::SyntaxError)
                                    .set_position(tokens[0].position())
                                    .set_message("Invalid function definition syntax");

        if tokens.is_empty() {
            return format_error.into();
        }

        // check for public method
        if tokens[index].to_string() == "pub" {
            self.public = true;
            index += 1;
        }

        if tokens.len() < 3 + index {
            return format_error.into();
        }

        // check for keyword
        if tokens[index].to_string() != "fun" {
            return format_error.into();
        }

        index += 1;

        // get function name
        if tokens[index].lower_case() {
            self.name = tokens[index].to_string();
        } else {
            errors.push(
                Error::new(ErrorKind::SyntaxError)
                    .set_position(tokens[index].position())
                    .set_message("expected a (lower-case) function name")
                    .into()
            );
        }

        index += 1;

        // get function args
        if let Token::Block { open_delim: "(", contents, .. } = &tokens[index] {
            if let Err(ref mut es) = self.parse_function_args(contents) {
                errors.append(es);
            }
        } else {
            errors.push(
                Error::new(ErrorKind::SyntaxError)
                    .set_position(tokens[index].position())
                    .set_message("expected a list of function arguments")
            );
        }

        index += 1;

        // get return type and potential inline body
        let mut equals_index = index;

        loop {
            if equals_index >= tokens.len() {
                errors.push(
                    Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens.last().unwrap().position())
                        .set_message("no '=' in function definition")
                );

                return Err(errors);
            } else if tokens[equals_index].to_string() == "=" {
                break;
            } else {
                equals_index += 1;
            }
        }

        if index == equals_index {
            self.return_type = TypeKind::NoneType.rc();
        } else if index < tokens.len() {
            match parse_type_kind(&tokens[index..equals_index], tokens[index].position()) {
                Ok(tp) => self.return_type = tp.rc(),
                Err(ref mut es) => errors.append(es)
            }
        }

        if !errors.is_empty() {
            Err(errors)
        } else if equals_index + 1 == tokens.len() {
            Ok(None)
        } else {
            Ok(Some(equals_index))
        }
    }
}

impl Into<TypeKind> for Function {
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


impl Into<TypeKind> for &Function {
    fn into(self) -> TypeKind {
        let mut args = vec![];

        for arg in self.args.iter() {
            args.push(Rc::clone(&arg.arg_type));
        }
    
        TypeKind::Function {
            args,
            return_type: self.return_type.clone()
        }
    }
}


/* Function arguments */


#[derive(Debug)]
pub struct Argument {
    pub arg_name: String,
    pub arg_type: Rc<TypeKind>,
    pub arg_mutable: bool
}


impl Argument {
    fn from_tokens(tokens: &[Token], pos: (usize, usize)) -> Result<Argument, Vec<Error>> {
        /* Parses an argument */

        if tokens.is_empty() {
            return Error::new(ErrorKind::SyntaxError)
                        .set_position(pos)
                        .set_message("received empty function argument")
                        .into();
        }

        let mut index = 0;

        // gets whether the arg is mutable
        let arg_mutable = if tokens[index].to_string() == "var" {
            index += 1;
            true
        } else {
            false
        };

        if tokens.len() < 3 + index {
            return Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("unrecognised syntax in function argument")
                        .into();
        }

        // arg name
        let arg_name = if tokens[index].lower_case() {
            tokens[index].to_string()
        } else {
            return Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[index].position())
                        .set_message("expected (lower-case) name for function argument")
                        .into();
        };

        index += 1;

        if tokens[index].to_string() == ":" {
            index += 1;
        } else {
            return Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("unrecognised syntax in function argument")
                        .into();
        }

        Ok(Argument {
            arg_name,
            arg_mutable,
            arg_type: parse_type_kind(&tokens[index..], tokens[index].position())?.rc()
        })
    }
}
