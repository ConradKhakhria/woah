use crate::{
    error::{ Error, ErrorKind },
    line::Line,
    parse::{
        Function,
        TypeKind
    },
    token::Token,
};
use std::{
    collections::HashMap,
    rc::Rc,
    string::ToString
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
            if line.line_tokens.len() < 3 {
                errors.push(Error::new(ErrorKind::SyntaxError)
                                .set_position(line.line_tokens[0].position())
                                .set_message("Unrecognised syntax in class defintion")
                                .into());
            } else if line.line_tokens[0].to_string() == "def" || line.line_tokens[1].to_string() == "def" {
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


    pub fn attribute_type<T: ToString>(&self, attribute_name: &T) -> Option<Rc<TypeKind<'s, 't>>> {
        /* Gets the type of a public or private attribute */

        if let Some(tp) = self.field_type(attribute_name) {
            Some(tp)
        } else if let Some(tp) = self.method_type(attribute_name) {
            Some(tp)
        } else {
            None
        }
    }


    pub fn field_type<T: ToString>(&self, field_name: &T) -> Option<Rc<TypeKind<'s, 't>>> {
        /* Returns the type of a public or private field */

        for field_collection in vec![ &self.public_fields, &self.private_fields ].iter() {
            for field in field_collection.iter() {
                if field.field_name.to_string() == field_name.to_string() {
                    return Some(Rc::clone(&field.field_type))
                }
            }
        }

        None
    }


    pub fn method_type<T: ToString>(&self, method_name: &T) -> Option<Rc<TypeKind<'s, 't>>> {
        /* Returns the type of a method */

        for method_collection in vec![ &self.public_methods, &self.private_methods ].iter() {
            for method in method_collection.iter() {
                if method.name.to_string() == method_name.to_string() {
                    let mut args = vec![];

                    for arg in method.args.iter() {
                        args.push(Rc::clone(&arg.arg_type));
                    }

                    return Some(Rc::new(
                        TypeKind::Function {
                            args,
                            return_type: match &method.return_type {
                                Some(tp) => Some(Rc::clone(tp)),
                                None => None
                            }
                        }
                    ))
                }
            }
        }

        None
    }
}


#[derive(Debug)]
pub struct Field<'s, 't> {
    pub field_name: &'t Token<'s>,
    pub field_type: Rc<TypeKind<'s, 't>>
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

        let field_type = Rc::new(TypeKind::from_tokens(&tokens[2 + offset..])?);

        Ok((Field { field_name, field_type }, offset == 1))
    }
}

pub fn collect_classes<'s, 't>(lines: &Vec<Line<'s, 't>>) -> Result<HashMap<String, Class<'s, 't>>, Vec<Error>> {
    /* Creates a map of all the classes in a program, for use in later compilation */

    let mut classes = HashMap::new();
    let mut errors = vec![];

    // for now, this will always land on a new class when imports are integrated,
    // this will have to deal with them too (but modules are just like classes anyway) 
    for line in lines {
        match Class::new(line) {
            Ok(c) => {
                classes.insert(c.name.to_string(), c);
            }

            Err(ref mut es) => {
                errors.append(es)
            }
        }
    }

    if errors.is_empty() {
        Ok(classes)
    } else {
        Err(errors)
    }
}
