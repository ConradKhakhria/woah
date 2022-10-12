use crate::analysis::analyse_program;
use crate::error::*;
use crate::parse::Module;
use std::collections::HashMap;
use std::env;
use std::path::PathBuf;


pub struct Interface {
    modules: HashMap<String, Module>
}


impl Interface {
    pub fn new() -> Self {
        Interface { modules: HashMap::new() }
    }


    /* Interface */

    pub fn build(&mut self) -> Result<(), Vec<Error>> {
        /* Builds a project (location and flags from std::env::args()) */

        // get modules
        let root_dir_name = env::args().nth(2).unwrap_or(".".into());
        let src_path = Self::get_source_path(&root_dir_name)?;
        
        self.collect_modules(&src_path)?;

        // statically analyse modules
        let static_analysis_results = analyse_program(&mut self.modules)?;

        Ok(())
    }


    pub fn create_project(&mut self) -> Result<(), Vec<Error>> {
        /* Creates a new project with a name specified in the args */

        let project_name = Self::get_project_name(env::args().nth(2))?;
        let cwd = match env::current_dir() {
            Ok(path) => path,
            Err(e) => return Error::new(ErrorKind::FileSystemError)
                                        .set_message(e.to_string())
                                        .into()
        };

        // directory paths
        let root_path = cwd.join(project_name);
        let src_path = root_path.join("src");
        let target_path = root_path.join("target");
        let generated_c_path = target_path.join("generated_c");

        let mut builder = std::fs::DirBuilder::new();
        builder.recursive(true);

        for path in [&generated_c_path, &src_path] {
            if builder.create(path).is_err() {
                return Error::new(ErrorKind::FileSystemError)
                            .set_message(format!("unable to create dir {}", path.display()))
                            .into();
            }
        }

        Ok(())
    }


    /* Module and filesystem */

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


    fn get_project_name(possible_name: Option<String>) -> Result<String, Vec<Error>> {
        /* Tests whether possible_name is a valid project name */

        match possible_name {
            Some(name) => {
                if !name.chars().all(char::is_alphabetic) {
                    return Error::new(ErrorKind::NameError)
                                .set_message("your project's name can only consist of letters")
                                .into();
                }

                Ok(name)
            }

            None => Error::new(ErrorKind::InterfaceError)
                        .set_message("your new project needs a name")
                        .into() 
        }
    }


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
}
