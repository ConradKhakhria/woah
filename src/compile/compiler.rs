use crate::error::*;
use crate::parse::Module;
use std::collections::HashMap;
use std::path::Path;


pub struct Compiler {
    modules: HashMap<String, Module>
}


impl Compiler {
    pub fn new() -> Self {
        Compiler { modules: HashMap::new() }
    }


    /* User interface */


    pub fn build(&mut self) -> Result<(), Vec<Error>> {
        /* Builds a project (location and flags from std::env::args()) */

        let root_dir_name = std::env::args().nth(2).unwrap_or(".".into());

        self.collect_modules(Path::new(&root_dir_name))?;


        Ok(())
    }


    /* Module and filesystem */


    fn collect_modules(&mut self, module_cursor: &Path) -> Result<(), Vec<Error>> {
        /* Collects all modules in a project */

        let mut errors = vec![];

        if module_cursor.is_dir() {
            for entry in std::fs::read_dir(module_cursor).unwrap() {
                match entry {
                    Ok(dir_entry) => {
                        let path = dir_entry.path();

                        if let Err(ref mut es) = self.collect_modules(path.as_path()) {
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

            self.modules.insert(module.module_name().clone(), module);
        }

        Ok(())
    }


    /* Compile */

}
