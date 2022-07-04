use crate::{
    message::{ Message, MsgKind },
    line::Line,
    parse::{ parse_statement_block, Statement, TypeKind },
    token::Token
};
use std::rc::Rc;

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub args: Vec<Argument>,
    pub return_type: Rc<TypeKind>,
    pub body: Vec<Statement>
}


impl Function {
    pub fn from_line(line: &Line) -> Result<Function, Vec<Message>> {
        /* Parses a function from a nested line */

        let tokens = line.line_tokens;
        let mut errors = vec![];

        let format_error = Message::new(MsgKind::SyntaxError)
                                    .set_position(tokens[0].position())
                                    .set_message("Invalid function definition syntax");

        if tokens.len() < 3 || tokens[0].to_string() != "def" || line.line_derivs.is_empty() {
            errors.push(format_error.clone());
        }

        /* Get function name */

        let name = tokens[1].to_string();

        if let Token::Identifier {..} = &tokens[1] {
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
                Ok(tp) => tp.rc(),
    
                Err(ref mut es) => {
                    errors.append(es);
                    TypeKind::NoReturnType.rc()
                }
            }
        } else {
            TypeKind::NoReturnType.rc()
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
            return_type: Rc::clone(&self.return_type)
        }
    }
}

impl Into<Rc<TypeKind>> for Function {
    fn into(self) -> Rc<TypeKind> {
        let tk: TypeKind = self.into();

        tk.rc()
    }
}

impl Into<Rc<TypeKind>> for &Function {
    fn into(self) -> Rc<TypeKind> {
        let tk: TypeKind = self.into();

        tk.rc()
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut string = format!("FUNCTION {}(", self.name);

        for arg in self.args.iter() {
            string = format!("{}{} : {}, ", string, arg.arg_name, arg.arg_type);
        }

        if self.args.len() == 0 {
            string += ")\n";
        } else {
            let strlen = string.len();
            string = format!("{})\n", &string[..strlen - 2]);
        }

        for stmt in self.body.iter() {
            string = format!("{}{}", string, stmt.display(4));
        }

        write!(f, "{}", string)
    }
}


/* Function arguments */


fn parse_arguments<'s, 't>(args_block: &'t Token<'s>) -> Result<Vec<Argument>, Vec<Message>> {
   /* Attempts to parse a list of function arguments */

    let mut args = vec![];
    let mut errors = vec![];

    let args_block = match args_block {
        Token::Block { contents, open_delim, .. } => {
            if *open_delim == "(" {
                contents
            } else {
                return Message::new(MsgKind::SyntaxError)
                            .set_position(args_block.position())
                            .set_message("Expected a '('-delimited list of arguments")
                            .into();
            }
        },

        _ => return Message::new(MsgKind::SyntaxError)
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
pub struct Argument {
    pub arg_name: String,
    pub arg_type: Rc<TypeKind>
}


impl Argument {
    fn from_tokens(tokens: &[Token]) -> Result<Argument, Vec<Message>> {
        /* Parses an argument */

        let arg_name = tokens[0].to_string();

        if let Token::Identifier {..} = &tokens[0] { } else {
            return Message::new(MsgKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("Expected an argument name")
                        .into();
        }

        if tokens.len() < 3 || tokens[1].to_string() != ":" {
            return Message::new(MsgKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("Expected syntax <argument name> : <argument type>")
                        .into();
        }

        let arg_type = TypeKind::from_tokens(&tokens[2..])?.rc();

        Ok(Argument { arg_name, arg_type })
    }
}
