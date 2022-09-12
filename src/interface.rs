use crate::error::*;
use crate::parse::Module;
use std::collections::HashMap;
use std::path::PathBuf;


pub struct Interface {
    modules: HashMap<String, Module>
}


impl Interface {
    pub fn new() -> Self {
        Interface { modules: HashMap::new() }
    }


    /* User interface */

    pub fn build(&mut self) -> Result<(), Vec<Error>> {
        /* Builds a project (location and flags from std::env::args()) */

        let root_dir_name = std::env::args().nth(2).unwrap_or(".".into());
        let src_path = Self::get_source_path(&root_dir_name)?;
        
        self.collect_modules(&src_path)?;


        Ok(())
    }


    /* Module and filesystem */

    fn get_source_path(root_dir_name: &String) -> Result<PathBuf, Vec<Error>> {
        /* Gets the path of the source directory */

        let mut root_path = PathBuf::from(root_dir_name);
        root_path.push("src");

        match std::fs::metadata(&root_path) {
            Ok(_) => Ok(root_path),

            Err(e) => {
                if let std::io::ErrorKind::NotFound = e.kind() {
                    Error::new(ErrorKind::FileSystemError)
                        .set_message("the path provided is not a valid project directory")
                        .into()   
                } else {
                    Ok(root_path)
                }
            }
        }
    }


    fn collect_modules(&mut self, module_cursor: &PathBuf) -> Result<(), Vec<Error>> {
        /* Collects all modules in a project */

        let mut errors = vec![];

        if module_cursor.is_dir() {
            for entry in std::fs::read_dir(module_cursor).unwrap() {
                match entry {
                    Ok(dir_entry) => {
                        let path = dir_entry.path();

                        if let Err(ref mut es) = self.collect_modules(&path) {
                            errors.append(es);
                        }
                    }

                    Err(e) => errors.push(
                        Error::new(ErrorKind::ModuleError)
                            .set_message(e.to_string())
                    )
                }
            }
        } else {
            let module = Module::from_filepath(module_cursor)?;

            if self.modules.contains_key(module.module_name()) {
                errors.push(
                    Error::new(ErrorKind::NameError)
                        .set_position(module.first_position())
                        .set_line(module.raw_lines())
                        .set_message(format!("two modules in this path named '{}'", module.module_name()))
                );
            } else {
                self.modules.insert(module.module_name().clone(), module);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}
