use crate::{
    error::{ Error, ErrorKind },
    token::Token,
    parse::{ parse_block, Statement, TypeKind }
};

pub struct FuncArg<'s, 't> {
    arg_name: &'t Token<'s>,
    arg_type: TypeKind<'s, 't>
}


impl<'s, 't> FuncArg<'s, 't> {
    pub fn from_tokens(tokens: &'t [Token<'s>]) -> Result<Self, Vec<Error>> {
        /* Parses a function argument from a list of tokens */

        if tokens.len() < 3 {
            return Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("Unrecognised syntax in function argument")
                        .into();
        }

        let mut errors = Vec::new();

        let arg_name = &tokens[0];

        match arg_name {
            Token::Identifier {..} => {},
            _ => errors.push(
                Error::new(ErrorKind::SyntaxError)
                    .set_position(arg_name.position())
                    .set_message("Expected argument name")
            )
        }

        if tokens[1].to_string() != ":" {
            errors.push(
                Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("Unrecognised syntax in function argument")
            );
        }

        match TypeKind::from_tokens(&tokens[2..]) {
            Ok(arg_type) => Ok(FuncArg { arg_name, arg_type }),

            Err(ref mut es) => {
                errors.append(es);

                Err(errors)
            }
        }
    }
}


pub struct GenericType<'s, 't> {
    name: &'t Token<'s>,
    derives: Vec<&'t Token<'s>>
}


impl<'s, 't> GenericType<'s, 't> {
    pub fn from_tokens(tokens: &'t [Token<'s>]) -> Result<Self, Vec<Error>> {
        /* Parses a generic parameter from a list of types */

        if tokens.len() % 2 == 0 {
            return Error::new(ErrorKind::SyntaxError)
                    .set_position(tokens[0].position())
                    .set_message("Invalid syntax in generic type definition")
                    .into();
        }

        let mut errors = Vec::new();
        let name = &tokens[0];

        if let Token::Identifier {..} = name {} else {
            errors.push(
                Error::new(ErrorKind::SyntaxError)
                    .set_position(name.position())
                    .set_message("Expected generic type name")
            );
        }

        let derives = if tokens.len() > 1 {
            let mut ds = Vec::new();

            if tokens[1].to_string() != ":" && tokens.len() == 2 {
                errors.push(
                    Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[1].position())
                        .set_message("Expected list of generic type constraints")
                );
            }

            let mut i = 2;

            while i + 1 < tokens.len() {
                match tokens[i] {
                    Token::Identifier {..} => ds.push(&tokens[i]),
                    _ => errors.push(
                        Error::new(ErrorKind::SyntaxError)
                            .set_position(tokens[i].position())
                            .set_message("Invalid syntax in generic type definition")
                    )
                }

                if tokens[i + 1].to_string() != "+" {
                    errors.push(
                        Error::new(ErrorKind::SyntaxError)
                            .set_position(tokens[i + 1].position())
                            .set_message("Expected '+'")
                    );
                }

                i += 2;
            }

            ds
        } else {
            Vec::new()
        };

        if errors.is_empty() {
            Ok(GenericType { name, derives })
        } else {
            Err(errors)
        }
    }
}


pub struct Function<'s, 't> {
    name: &'t Token<'s>,
    args: Vec<FuncArg<'s, 't>>,
    generics: Vec<GenericType<'s, 't>>,
    body: Vec<Statement<'s, 't>>,
    public: bool
}

impl<'s, 't> Function<'s, 't> {
    fn parse_generics(generics_block: &'t Token<'s>) -> Result<Vec<GenericType<'s, 't>>, Vec<Error>> {
        /* Parses a list of generics */

        let mut generics = Vec::new();
        let mut errors = Vec::new();

        let inner_values = match generics_block {
            Token::Block { contents, .. } => contents,
            _ => unreachable!()
        };

        let comma_separated = Token::split_tokens(&inner_values[..], |t| t.to_string() == ",");

        for (start, end) in comma_separated {
            match GenericType::from_tokens(&inner_values[start..end]) {
                Ok(gt) => generics.push(gt),
                Err(ref mut es) => errors.append(es)
            }
        }

        if errors.is_empty() {
            Ok(generics)
        } else {
            Err(errors)
        }
    }


    fn parse_arguments(args_block: &'t Token<'s>) -> Result<Vec<FuncArg<'s, 't>>, Vec<Error>> {
        /* Parses a list of arguments */

        let inner_values = if args_block.delim_equals("(") {
            match args_block {
                Token::Block { contents, .. } => contents,
                _ => unreachable!()
            }
        } else {
            return Error::new(ErrorKind::SyntaxError)
                        .set_position(args_block.position())
                        .set_message("Expected a block of arguments")
                        .into();
        };

        let mut arguments = Vec::new();
        let mut errors = Vec::new();

        let comma_separated = Token::split_tokens(&inner_values[..], |t| t.to_string() == ",");

        for (start, end) in comma_separated {
            match FuncArg::from_tokens(&inner_values[start..end]) {
                Ok(arg) => arguments.push(arg),
                Err(ref mut es) => errors.append(es)
            }
        }

        if errors.is_empty() {
            Ok(arguments)
        } else {
            Err(errors)
        }
    }


    fn parse_function_body(body_block: &'t Token<'s>) -> Result<Vec<Statement<'s, 't>>, Vec<Error>> {
        /* Parses a function body */

        if !body_block.delim_equals("{") {
            return Error::new(ErrorKind::SyntaxError)
                        .set_position(body_block.position())
                        .set_message("Expected function body")
                        .into()
        }

        let block_contents = match body_block {
            Token::Block { contents, .. } => contents,
            _ => unreachable!()
        };

        parse_block(&block_contents[..])
    }


    pub fn from_tokens(tokens: &'t [Token<'s>]) -> Result<Self, Vec<Error>> {
        /* Parses a function from a list of tokens */

        if tokens.len() < 4 {
            return Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("Unrecognised syntax in function definition")
                        .into();
        }

        let mut index = 0;
        let mut errors = Vec::new();

        let public = if tokens[index].to_string() == "pub" {
            index += 1;
            true
        } else {
            false
        };

        if tokens[index].to_string() == "fn" {
            index += 1;
        } else {
            return Error::new(ErrorKind::SyntaxError)
                    .set_position(tokens[index].position())
                    .set_message("Unrecognised syntax")
                    .into();
        }

        let name = &tokens[index];

        // ensure the function name is an identifier
        match &tokens[index] {
            Token::Identifier {..} => index += 1,

            _ => errors.push(
                Error::new(ErrorKind::SyntaxError)
                    .set_position(tokens[index].position())
                    .set_message("Expected function name")
            )
        };

        let generics = if tokens[index].delim_equals("[") {
            index += 1;

            Self::parse_generics(&tokens[index - 1])
                 .map_err(|ref mut es| errors.append(es))
        } else {
            Ok(Vec::new())
        };

        let arguments = Self::parse_arguments(&tokens[index])
                            .map_err(|ref mut es| errors.append(es));

        index += 1;

        let function_body = Self::parse_function_body(&tokens[index])
                                .map_err(|ref mut es| errors.append(es));

        if errors.is_empty() {
            Ok(Function {
                name,
                args: arguments.unwrap(),
                generics: generics.unwrap(),
                body: function_body.unwrap(),
                public
            })
        } else {
            Err(errors)
        }
    }
}
