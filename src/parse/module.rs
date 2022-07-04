use crate::{
    analysis::{ BUILT_IN_FUNCTIONS, type_of_builtin_function },
    message::{ Message, MsgKind },
    line::create_lines,
    parse::{
        Argument,
        Function,
        Import,
        TypeKind
    },
    token::tokenise
};
use std::{
    collections::HashMap,
    path::Path,
    rc::Rc
};

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub functions: HashMap<String, Function>,
    pub imports: Vec<Import>,
    pub source_lines: Vec<String>
}


impl Module {
    pub fn from_filepath(path: &Path) -> Result<Self, Vec<Message>> {
        /* Reads and parses a module from a file */

        let filename = match path.to_str() {
            Some(s) => s,
            None => return Message::new(MsgKind::SyntaxError)
                                .set_message(format!("Filename '{:?}' is not valid unicode", path.as_os_str()))
                                .into()
        };

        /* Get module name */

        let module_name = {
            let filename_len = filename.len();

            if filename.ends_with(".woah") {
                filename[..filename_len - 5].to_string()
            } else {
                return Message::new(MsgKind::ModuleError)
                            .set_message(format!("File {} doesn't end with '.woah'", filename))
                            .into();
            }
        };

        /* Get tokens and lines */

        let source = std::fs::read_to_string(path).unwrap();
        let source_lines: Vec<String> = source.lines().map(|s| s.to_string()).collect();
        let tokens = match tokenise(&source, filename) {
            Ok(ts) => ts,
            Err(es) => {
                let with_line = es.iter().map(|e| e.clone().set_line(&source_lines)).collect();

                return Err(with_line)
            }
        };


        let lines = create_lines(&tokens);

        /* Collect functions and imports */

        let mut functions = Self::create_builtin_functions();
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
                    errors.push(Message::new(MsgKind::SyntaxError)
                                    .set_position(line.line_tokens[0].position())
                                    .set_message("unrecognised syntax"));
                }
            }
        }

        if errors.is_empty() {
            Ok(Module {
                name: module_name,
                functions,
                imports,
                source_lines
            })
        } else {
            Err(errors.iter().map(|e| e.clone().set_line(&source_lines)).collect())
        }
    }


    fn create_builtin_functions() -> HashMap<String, Function> {
        /* Generates Function structs for all built-in functions */

        let mut functions = HashMap::new();

        for function_name in BUILT_IN_FUNCTIONS.iter() {
            let function_type = type_of_builtin_function(function_name).unwrap();

            let name = function_name.to_string();

            let (arg_types, return_type) = match &*function_type {
                TypeKind::Function { args, return_type } => (args, return_type),
                _ => unreachable!()
            };
    
            let mut args = vec![];
    
            for (i, arg_type) in arg_types.iter().enumerate() {
                args.push(Argument {
                    arg_name: format!("_arg{}", i + 1),
                    arg_type: Rc::clone(arg_type)
                });
            }

            let function = Function {
                name: name.clone(),
                args,
                return_type: Rc::clone(return_type),
                body: vec![]
            };

            functions.insert(name, function);
        }

        functions
    }


    pub fn rc(self) -> Rc<Self> {
        /* Wraps self in an Rc */

        Rc::new(self)
    }
}


impl Into<TypeKind> for Module {
    fn into(self) -> TypeKind {
        TypeKind::HigherOrder {
            name: self.name.clone(),
            args: vec![]
        }
    }
}


impl Into<Rc<TypeKind>> for Module {
    fn into(self) -> Rc<TypeKind> {
        let tk = TypeKind::HigherOrder {
            name: self.name.clone(),
            args: vec![]
        };

        tk.rc()
    }
}


impl Into<TypeKind> for &Module {
    fn into(self) -> TypeKind {
        TypeKind::HigherOrder {
            name: self.name.clone(),
            args: vec![]
        }
    }
}


impl Into<Rc<TypeKind>> for &Module {
    fn into(self) -> Rc<TypeKind> {
        let tk = TypeKind::HigherOrder {
            name: self.name.clone(),
            args: vec![]
        };

        tk.rc()
    }
}
