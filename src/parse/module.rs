use crate::{
    error::{ Error, ErrorKind },
    line::{ create_lines, Line },
    parse::{
        Class,
        Function,
        TypeKind
    },
    token::Token
};
use derive_getters::Getters;
use std::{
    ffi::OsString,
    path::Path
};


enum AssociatedType<'s, 't> {
    Class {
        class: Class<'s, 't>,
        class_type: TypeKind
    },

    None
}


#[derive(Getters)]
pub struct Module<'s, 't> {
    file_scope: Vec<OsString>,
    associated_type: AssociatedType<'s, 't>,
    module_methods: Vec<Function<'s, 't>>,
    instance_methods: Vec<Function<'s, 't>>
}


impl<'s, 't> Module<'s, 't> {
    /* Instantiation */


    fn parse_import(line: &Line<'s, 't>) -> Result<(), Vec<Error>> {
        /* Parses a module import */

        Error::new(ErrorKind::UnimplementedError)
            .set_position(line.line_tokens[0].position())
            .set_message("Imports are not implemented")
            .into()
    }


    fn parse_mod_block(module: &mut Module<'s, 't>, line: &Line<'s, 't>) -> Vec<Error> {
        /* Parses a mod block into module */

        unimplemented!();
    }


    pub fn from_tokens(filepath: &Path, tokens: &'t [Token<'s>]) -> Result<Module<'s, 't>, Vec<Error>> {
        /* Reads a module from a file */

        let lines = create_lines(&tokens);

        let mut module = Module {
            file_scope: filepath.into_iter().map(|s| s.to_os_string()).collect(),
            associated_type: AssociatedType::None,
            module_methods: vec![],
            instance_methods: vec![]
        };

        let mut defined_associated_type = false;
        let mut defined_mod_block = false;

        let mut errors = vec![];

        for line in lines.iter() {
            match &line.line_tokens[0].to_string()[..] {
                "use" => {
                    match Self::parse_import(line) {
                        Ok(()) => {},
                        Err(ref mut es) => errors.append(es)
                    }
                }

                "obj" => {
                    let mut parse_class_res = Class::new(line);

                    if defined_associated_type {
                        errors.push(Error::new(ErrorKind::ModuleError)
                                        .set_position(line.line_tokens[0].position())
                                        .set_message("Module already has an associated type"));
                    } else if let Ok(c) = parse_class_res {
                        defined_associated_type = true;
                        module.associated_type = AssociatedType::Class { class_type: (&c).into(), class: c, };
                    } else if let Err(ref mut es) = parse_class_res {
                        errors.append(es);
                    }
                }

                "enum" => {
                    let mut parse_enum_res: Result<(), Vec<Error>> = Error::new(ErrorKind::UnimplementedError)
                                                                    .set_position(line.line_tokens[0].position())
                                                                    .set_message("Enums are not yet implemented")
                                                                    .into();

                    if defined_associated_type {
                        errors.push(Error::new(ErrorKind::ModuleError)
                                        .set_position(line.line_tokens[0].position())
                                        .set_message("Module already has an associated type"));
                    } else if let Ok(c) = parse_enum_res {
                        defined_associated_type = true;
                        ();
                    } else if let Err(ref mut es) = parse_enum_res {
                        errors.append(es);
                    }
                }

                "mod" => {
                    if defined_mod_block {
                        errors.push(Error::new(ErrorKind::ModuleError)
                                        .set_position(line.line_tokens[0].position())
                                        .set_message("The module has already been defined"));
                    } else {
                        errors.append(&mut Self::parse_mod_block(&mut module, line));
                        defined_mod_block = true;
                    }
                }

                s => {
                    errors.push(Error::new(ErrorKind::SyntaxError)
                                    .set_position(line.line_tokens[0].position())
                                    .set_message(format!("Unrecognised syntax - a top level line cannot begin '{}'", s)));
                }
            }
        }

        if errors.is_empty() {
            Ok(module)
        } else {
            Err(errors)
        }
    }
}
