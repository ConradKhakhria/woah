use crate::error::*;
use crate::line::create_lines;
use crate::line::Line;
use crate::parse::Function;
use crate::token::tokenise;
use derive_getters::Getters;
use std::path::Path;


enum ModuleType {
    None
}


#[derive(Getters)]
pub struct Module {
    instance_methods: Vec<Function>,
    module_methods: Vec<Function>,
    module_name: String,
    module_path: Vec<String>,
    module_type: ModuleType
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
        let raw_lines: Vec<String> = source.split("\n").map(|s| s.to_string()).collect();
        let tokens = tokenise(source, &filename, (1, 1))?;
        let lines = create_lines(tokens.as_slice());

        let mut module = Module {
            instance_methods: vec![],
            module_methods: vec![],
            module_name: String::new(),
            module_path: vec![],
            module_type: ModuleType::None
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
                errors.push(e.set_filename(&filename).set_line(&raw_lines));
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
                    if f.variable_instance_method.is_some() {
                        self.instance_methods.push(f);
                    } else {
                        self.module_methods.push(f);
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
}
