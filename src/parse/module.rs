use crate::{
    error::{ Error, ErrorKind },
    line::create_lines,
    parse::{
        Function,
        Import
    },
    token::tokenise
};
use std::{
    collections::HashMap,
    path::Path
};

#[derive(Debug)]
pub struct Module {
    name: String,
    functions: HashMap<String, Function>,
    imports: Vec<Import>
}


impl Module {
    pub fn from_filepath(path: &Path) -> Result<Self, Vec<Error>> {
        /* Reads and parses a module from a file */

        let filename = match path.to_str() {
            Some(s) => s,
            None => return Error::new(ErrorKind::SyntaxError)
                                .set_message(format!("Filename '{:?}' is not valid unicode", path.as_os_str()))
                                .into()
        };

        let source = std::fs::read_to_string(path).unwrap();
        let source_lines: Vec<&str> = source.lines().collect();
        let tokens = match tokenise(&source, filename) {
            Ok(ts) => ts,
            Err(es) => {
                let with_line = es.iter().map(|e| e.clone().set_line(&source_lines)).collect();

                return Err(with_line)
            }
        };


        let lines = create_lines(&tokens);

        /* Collect functions and imports */

        let mut functions = HashMap::new();
        let mut imports = vec![];
        let mut errors = vec![];

        for line in lines.iter() {
            match &line.line_tokens[0].to_string()[..] {
                "use" => match Import::from_line(line) {
                    Ok(i) => imports.push(i),
                    Err(ref mut es) => errors.append(es)
                }

                "def" => match Function::from_line(line) {
                    Ok(f) => {
                        functions.insert(f.name.clone(), f);
                    },
                    Err(ref mut es) => errors.append(es)
                }

                _ => {
                    errors.push(Error::new(ErrorKind::SyntaxError)
                                    .set_position(line.line_tokens[0].position())
                                    .set_message("unrecognised syntax"));
                }
            }
        }

        if errors.is_empty() {
            Ok(Module {
                name: path.file_name().unwrap().to_str().unwrap().into(), // I love Rust
                functions,
                imports
            })
        } else {
            Err(errors.iter().map(|e| e.clone().set_line(&source_lines)).collect())
        }
    }
}
