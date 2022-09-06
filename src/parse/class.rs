use crate::error::*;
use crate::line::Line;
use crate::parse::Function;
use crate::parse::parse_type_kind;
use crate::parse::TypeKind;
use crate::token::Token;
use derive_getters::Getters;
use std::collections::HashMap;
use std::rc::Rc;
use std::string::ToString;


#[derive(Debug, Getters)]
pub struct Class {
    name: String,
    attributes: HashMap<String, Attribute>,
}


impl Class {

    /* Initialisation */

    fn get_class_name(line: &Line) -> Result<String, Vec<Error>> {
        /* Determintes and checks the name of a class */

        match &line.line_tokens[1] {
            Token::Identifier { string, position } => {
                if ('A'..'Z').contains(&string.chars().next().unwrap()) {
                    Ok(string.to_string())
                } else {
                    Error::new(ErrorKind::SyntaxError)
                        .set_position(*position)
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


    pub fn new(line: &Line) -> Result<Class, Vec<Error>> {
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


impl Into<TypeKind> for Class {
    fn into(self) -> TypeKind {
        TypeKind::HigherOrder {
            name: self.name.to_string(),
            args: vec![]
        }
    }
}


impl Into<TypeKind> for &Class {
    fn into(self) -> TypeKind {
        TypeKind::HigherOrder {
            name: self.name.to_string(),
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
pub struct Attribute {
    public: bool,
    attr_name: String,
    attr_type: Rc<TypeKind>
}


impl Attribute {
    fn from_line(line: &Line) -> Result<Self, Vec<Error>> {
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

        let attr_type = match parse_type_kind(&tokens[index + 2..]) {
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
