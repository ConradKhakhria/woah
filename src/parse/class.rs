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
            match Attribute::from_line(line, name.to_string()) {
                Ok(attr) => {
                    let attr_name = attr.name.clone();

                    if let Some(_) = attributes.insert(attr.name.clone(), attr) {
                        errors.push(Error::new(ErrorKind::NameError)
                                        .set_position(line.line_tokens[0].position())
                                        .set_message(format!("Cannot have multiple defintions of '{}'", attr_name)));
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

    /* Attribute getters */

    pub fn class_attribute_type(&self, attr_name: &Token<'s>, curr: &String) -> Option<Rc<TypeKind<'s, 't>>> {
        /* Returns a class attribute (class method) */

        match self.attributes.get(&attr_name.to_string()) {
            Some(attr) => {
                match &attr.attribute_type {
                    AttrType::ClassMethod(m) | AttrType::Constructor(m) => {
                        if !attr.public && self.name.to_string() != *curr {
                            None
                        } else {
                            Some(Rc::new(m.into()))
                        }
                    },
                    _ => None
                }
            }

            None => None
        }
    }


    pub fn object_attribute_type(&self, attr_name: &Token<'s>, curr: &String) -> Option<Rc<TypeKind<'s, 't>>> {
        /* Returns an object attribute (field or object method) */

        let attr = match self.attributes.get(&attr_name.to_string()) {
            Some(attr) => attr,
            None => return None
        };

        if !attr.public && self.name.to_string() != *curr {
            return None;
        }

        match &attr.attribute_type {
            AttrType::ObjectMethod(f) => Some(Rc::new(f.into())),
            AttrType::Field { attr_type, ..} => Some(Rc::clone(attr_type)),
            _ => None
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


#[derive(Debug)]
pub enum AttrType<'s, 't> {
    Field {
        attr_name: &'t Token<'s>,
        attr_type: Rc<TypeKind<'s, 't>>
    },

    ClassMethod(Function<'s, 't>),

    Constructor(Function<'s, 't>),

    ObjectMethod(Function<'s, 't>)
}


#[derive(Debug, Getters)]
pub struct Attribute<'s, 't> {
    public: bool,
    name: String,
    attribute_type: AttrType<'s, 't>
}



impl<'s, 't> Attribute<'s, 't> {
    fn from_line(line: &Line<'s, 't>, class_name: String) -> Result<Self, Vec<Error>> {
        let tokens = line.line_tokens;
        let mut index = 0;
        let name;

        if tokens.len() < 3 {
            return Error::new(ErrorKind::SyntaxError)
                            .set_position(tokens[0].position())
                            .set_message("Unrecognised syntax in class defintion")
                            .into();
        }

        let public = if tokens[0].to_string() == "pub" {
            index += 1;
            true
        } else {
            false
        };

        let attribute_type = if tokens[index].to_string() == "def" {
            let function = Function::from_line(line)?;

            name = function.name.to_string();

            if name == "new" {
                if !function.object_method {
                    return Error::new(ErrorKind::SyntaxError)
                                .set_position(tokens[index + 1].position())
                                .set_message("Constructor must have a 'self' parameter")
                                .into();
                }

                match &function.return_type {
                    Some(tp) => {
                        match &**tp {
                            TypeKind::HigherOrder { name, .. } => {
                                if name.to_string() != class_name {
                                    return Error::new(ErrorKind::TypeError)
                                                .set_position(tokens[index + 1].position())
                                                .set_message("constructor must return the type of the class itself")
                                                .into()
                                }
                            },

                            _ => return Error::new(ErrorKind::TypeError)
                                            .set_position(tokens[index + 1].position())
                                            .set_message("constructor must return the type of the class itself")
                                            .into()
                        }
                    }

                    None => return Error::new(ErrorKind::TypeError)
                                        .set_position(tokens[index + 1].position())
                                        .set_message("constructor must return the type of the class itself")
                                        .into()
                }

                if let None = &function.return_type {
                    return Error::new(ErrorKind::SyntaxError)
                                .set_position(tokens[index + 1].position())
                                .set_message("Constructor must have no return type (as self return is implicit)")
                                .into()
                }

                AttrType::Constructor(function)
            } else if function.object_method {
                AttrType::ObjectMethod(function)
            } else {
                AttrType::ClassMethod(function)
            }
        } else {
            let attr_name = &tokens[index];

            name = attr_name.to_string();

            if let Token::Identifier {..} = attr_name {} else {
                return Error::new(ErrorKind::SyntaxError)
                            .set_position(attr_name.position())
                            .set_message("Expected an attribute name")
                            .into();
            }

            if tokens[index + 1].to_string() != ":" {
                return Error::new(ErrorKind::SyntaxError)
                            .set_position(attr_name.position())
                            .set_message("Expected syntax (pub)? <name> : <type>")
                            .into();
            }

            let attr_type = Rc::new(TypeKind::from_tokens(&tokens[index + 2..])?);

            AttrType::Field {
                attr_name,
                attr_type 
            }
        };

        Ok(Attribute {
            public,
            name,
            attribute_type
        })
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
                let class_name = c.name.to_string();

                if let Some(_) = classes.insert(c.name.to_string(), c) {
                    errors.push(Error::new(ErrorKind::NameError)
                                    .set_position(line.line_tokens[0].position())
                                    .set_message(format!("Class '{}' defined twice", class_name)));
                }
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
