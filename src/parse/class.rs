use crate::{
    error::{ Error, ErrorKind },
    line::Line,
    parse::{
        Function,
        TypeKind
    },
    token::Token,
};
use derive_getters::Getters;
use std::{
    collections::HashMap,
    rc::Rc,
    string::ToString
};

#[derive(Debug, Getters)]
pub struct Class<'s, 't> {
    name: &'t Token<'s>,
    attributes: HashMap<String, Attribute<'s, 't>>,
}


impl<'s, 't> Class<'s, 't> {
    /* Initialisation */

    fn get_class_name(line: &Line<'s, 't>) -> Result<&'t Token<'s>, Vec<Error>> {
        /* Determintes and checks the name of a class */

        match &line.line_tokens[1] {
            t @ Token::Identifier { string, .. } => {
                if ('A'..'Z').contains(&string.chars().next().unwrap()) {
                    Ok(t)
                } else {
                    Error::new(ErrorKind::SyntaxError)
                        .set_position(t.position())
                        .set_message("Class names must begin with an upper-case character")
                        .into()
                }
            },

            t => Error::new(ErrorKind::SyntaxError)
                        .set_position(t.position())
                        .set_message("Expected class name")
                        .into()
        }
    }


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

        let name = Self::get_class_name(line)?;
        let mut attributes = HashMap::new();

        for line in line.line_derivs.iter() {
            match Attribute::from_line(line) {
                Ok(attr) => {
                    let attr_name = attr.attr_name.clone();

                    if let Some(_) = attributes.insert(attr.attr_name.clone(), attr) {
                        errors.push(Error::new(ErrorKind::NameError)
                                        .set_position(line.line_tokens[0].position())
                                        .set_message(format!("Cannot have multiple attributes called '{}'", attr_name)));
                    }
                }

                Err(ref mut es) => errors.append(es)
            }
        }

        if errors.is_empty() {
            Ok(Class {
                name,
                attributes
            })
        } else {
            Err(errors)
        }
    }
}


impl<'s, 't> Into<TypeKind<'s, 't>> for Class<'s, 't> {
    fn into(self) -> TypeKind<'s, 't> {
        TypeKind::HigherOrder {
            name: self.name,
            args: vec![]
        }
    }
}


impl<'s, 't> Into<TypeKind<'s, 't>> for &Class<'s, 't> {
    fn into(self) -> TypeKind<'s, 't> {
        TypeKind::HigherOrder {
            name: self.name,
            args: vec![]
        }
    }
}


/* Attributes
 *
 * All of a classes attributes are stored in an Attribute struct,
 * which records the visibility and attribute type.
 * 
 * Attributes must have unique names, disallowing identically-named
 * methods and fields
 */


#[derive(Debug, Getters)]
pub struct Attribute<'s, 't> {
    public: bool,
    attr_name: String,
    attr_type: Rc<TypeKind<'s, 't>>
}


impl<'s, 't> Attribute<'s, 't> {
    fn from_line(line: &Line<'s, 't>) -> Result<Self, Vec<Error>> {
        /* Attempts ot parse an attribute from a line */

        let tokens = line.line_tokens;
        let mut index = 0;
        let mut errors = vec![];

        if tokens[0].to_string() == "pub" {
            index += 1;
        }

        let attr_name = tokens[index].to_string();

        if let Token::Identifier {..} = tokens[index] {} else {
            errors.push(Error::new(ErrorKind::SyntaxError)
                            .set_position(tokens[index].position())
                            .set_message("Expected attribute identifier"));
        }

        if tokens[index + 1].to_string() != ":" {
            errors.push(Error::new(ErrorKind::SyntaxError)
                            .set_position(tokens[index + 1].position())
                            .set_message("Expected syntax <attribute name> : <attribute type>"));
        }

        let attr_type = match TypeKind::from_tokens(&tokens[index + 2..]) {
            Ok(tp) => tp.rc(),
            Err(ref mut es) => {
                errors.append(es);
                TypeKind::Bool.rc() // placeholder
            }
        };

        if errors.is_empty() {
            Ok(Attribute {
                public: index == 1,
                attr_name,
                attr_type
            })
        } else {
            Err(errors)
        }
    }
}
