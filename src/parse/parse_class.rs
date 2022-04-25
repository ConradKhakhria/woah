use crate::{
    error::{ Error, ErrorKind },
    line::Line,
    parse::{
        Function,
        TypeKind
    },
    token::Token,
};

#[derive(Debug)]
pub struct Class<'s, 't> {
    pub name: &'t Token<'s>,
    pub public_fields: Vec<Field<'s, 't>>,
    pub private_fields: Vec<Field<'s, 't>>,
    pub public_methods: Vec<Function<'s, 't>>,
    pub private_methods: Vec<Function<'s, 't>>
}


impl<'s, 't> Class<'s, 't> {
    pub fn new(line: &Line<'s, 't>) -> Result<Class<'s, 't>, Vec<Error>> {
        /* Parses a class from a nested Line */

        let tokens = line.line_tokens;
        let mut errors = vec![];

        if tokens.len() != 2 || tokens[0].to_string() != "obj" {
            return Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0].position())
                        .set_message("invalid class definition syntax")
                        .into();
        }

        let name = &tokens[0];

        if let Token::Identifier {..} = name {} else {
            errors.push(Error::new(ErrorKind::SyntaxError)
                            .set_position(name.position())
                            .set_message("Expected class name")
                            .into());
        }

        let mut public_fields = vec![];
        let mut private_fields = vec![];
        let mut public_methods = vec![];
        let mut private_methods = vec![];

        for line in line.line_derivs.iter() {
            if vec!["def", "pub"].contains(&&line.line_tokens[0].to_string()[..]) {
                match Function::from_line(line) {
                    Ok(f) => {
                        if f.public {
                            public_methods.push(f);
                        } else {
                            private_methods.push(f);
                        }
                    },

                    Err(ref mut es) => errors.append(es)
                }
            } else {
                match Field::from_tokens(&line.line_tokens) {
                    Ok((field, public)) => {
                        if public {
                            public_fields.push(field);
                        } else {
                            private_fields.push(field);
                        }
                    },

                    Err(ref mut es) => errors.append(es)
                }
            }
        }

        if errors.is_empty() {
            Ok(Class {
                name,
                public_fields,
                private_fields,
                public_methods,
                private_methods
            })
        } else {
            Err(errors)
        }
    }
}


#[derive(Debug)]
pub struct Field<'s, 't> {
    pub field_name: &'t Token<'s>,
    pub field_type: TypeKind<'s, 't>
}

impl<'s, 't> Field<'s, 't> {
    fn from_tokens(tokens: &'t [Token<'s>]) -> Result<(Field<'s, 't>, bool), Vec<Error>> {
        /* Parses an argument */

        let offset = if tokens[0].to_string() == "pub" { 1 } else { 0 };

        let field_name = &tokens[0 + offset];

        if let Token::Identifier {..} = field_name { } else {
            return Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0 + offset].position())
                        .set_message("Expected an argument name")
                        .into();
        }

        if tokens.len() < 3 + offset || tokens[1 + offset].to_string() != ":" {
            return Error::new(ErrorKind::SyntaxError)
                        .set_position(tokens[0 + offset].position())
                        .set_message("Expected syntax <argument name> : <argument type>")
                        .into();
        }

        let field_type = TypeKind::from_tokens(&tokens[2..])?;

        Ok((Field { field_name, field_type }, offset == 1))
    }
}

