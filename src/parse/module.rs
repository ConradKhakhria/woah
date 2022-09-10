use crate::error::*;
use crate::line::create_lines;
use crate::line::Line;
use crate::parse::Function;
use crate::token::tokenise;
use derive_getters::Getters;
use std::collections::HashMap;
use std::path::Path;


#[derive(Debug)]
pub enum ModuleType {
    None
}


#[derive(Debug, Getters)]
pub struct Module {
    instance_methods: HashMap<String, Function>,
    module_methods: HashMap<String, Function>,
    module_name: String,
    module_path: Vec<String>,
    _module_type: ModuleType,
    positions: [(usize, usize); 2],
    raw_lines: Vec<String>
}


impl Module {

    /* Instantiation */

    pub fn from_filepath(filepath: &Path) -> Result<Module, Vec<Error>> {
        /* Reads a module from a file whose name is supplied */

        let filename = filepath.to_str()
                            .expect("Liszp - fatal: could not convert filepath to string");
        let source = std::fs::read_to_string(filepath)
                            .expect(format!("Liszp - fatal: could not open file '{}'", filename).as_str());

        Self::from_source(&source, filepath)
    }


    pub fn from_source(source: &String, filepath: &Path) -> Result<Module, Vec<Error>> {
        /* Reads a module from a source string */

        let filename = filepath.to_str().unwrap().to_string();
        let mut errors = vec![];

        // get lines
        let tokens = tokenise(source, &filename, (1, 1))?;
        let lines = create_lines(tokens.as_slice());

        let mut module = Module {
            instance_methods: HashMap::new(),
            module_methods: HashMap::new(),
            module_name: String::new(),
            module_path: vec![],
            _module_type: ModuleType::None,
            positions: [
                lines.first().unwrap().first_position(),
                lines.last().unwrap().last_position()
            ],
            raw_lines: source.split("\n").map(|s| s.to_string()).collect()
        };

        module.create_module_path(filepath, &filename)?;

        // parse lines
        for line in lines.iter() {
            let es = match line.line_tokens[0].to_string().as_str() {
                "enum" => module.parse_enum(line),
                "mod"  => module.parse_module_block(line),
                "obj"  => module.parse_class(line),
                "use"  => module.parse_import(line),
                _      => Error::new(ErrorKind::SyntaxError)
                            .set_position(line.first_position())
                            .set_message("unrecognised syntax in module")
                            .into()
            };

            for e in es {
                errors.push(e.set_filename(&filename).set_line(&module.raw_lines));
            }
        }

        if errors.is_empty() {
            Ok(module)
        } else {
            Err(errors)
        }
    }

    /* Parse components of the file */

    fn parse_class(&mut self, line: &Line) -> Vec<Error> {
        /* Parses a class definition */

        Error::new(ErrorKind::UnimplementedError)
            .set_position(line.first_position())
            .set_message("classes are not implemented")
            .into()
    }


    fn parse_enum(&mut self, line: &Line) -> Vec<Error> {
        /* Parses an enum definition */

        Error::new(ErrorKind::UnimplementedError)
            .set_position(line.first_position())
            .set_message("enums are not implemented")
            .into()
    }


    fn parse_import(&mut self, line: &Line) -> Vec<Error> {
        /* Parses a module import */

        Error::new(ErrorKind::UnimplementedError)
            .set_position(line.first_position())
            .set_message("Imports are not implemented")
            .into()
    }


    fn parse_module_block(&mut self, line: &Line) -> Vec<Error> {
        /* Parses a mod block into module */

        // temporarily, the opening line will consist of only 2 tokens
        if line.line_tokens.len() != 2 {
            return Error::new(ErrorKind::SyntaxError)
                        .set_position(line.first_position())
                        .set_message("invalid syntax in module")
                        .into();
        }

        if line.line_tokens[1].to_string().to_lowercase() != self.module_path.last().unwrap().to_lowercase() {
            return Error::new(ErrorKind::SyntaxError)
                        .set_position(line.first_position())
                        .set_message("the module name must be the same as the filename")
                        .into();
        }

        let mut errors = vec![];

        for line in line.line_derivs.iter() {
            match Function::parse_function(line) {
                Ok(f) => {
                    let method_category = if f.variable_instance_method.is_some() {
                        &mut self.instance_methods
                    } else {
                        &mut self.module_methods
                    };

                    if method_category.contains_key(&f.name) {
                        errors.push(
                            Error::new(ErrorKind::NameError)
                                .set_position(f.first_position())
                                .set_message(format!(
                                    "method '{}' for module '{}' defined twice",
                                    f.name,
                                    self.module_name()
                                ))
                        );
                    } else {
                        method_category.insert(f.name.clone(), f);
                    }
                }

                Err(ref mut es) => errors.append(es)
            }
        }

        errors
    }

    /* Misc */

    fn create_module_path(&mut self, filepath: &Path, filename: &String) -> Result<(), Vec<Error>> {
        /* Creates a module path and adds it to self */

        let extension = filepath.extension().unwrap().to_str().unwrap();
        let extension_length = extension.len();

        for path_component in filepath.iter() {
            match path_component.to_str() {
                Some(s) => {
                    if s.ends_with(extension) {
                        let string = s[..s.len() - extension_length - 1].to_string();

                        self.module_path.push(string.clone());
                        self.module_name = string;
                    } else {
                        self.module_path.push(s.to_string());
                    }
                }

                None => {
                    return Error::new(ErrorKind::ModuleError)
                                .set_filename(&filename)
                                .set_message("all files and directories must have valid UTF-8 names")
                                .into();
                }
            }
        }

        Ok(())
    }


    pub fn first_position(&self) -> (usize, usize) {
        /* Gets the first position of a module */

        self.positions[0].clone()
    }

    #[allow(dead_code)]
    pub fn last_position(&self) -> (usize, usize) {
        /* Gets the last position of a module */

        self.positions[1].clone()
    }
}
