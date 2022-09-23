use crate::analysis::TypeChecker;
use crate::error::*;
use crate::parse::Module;
use std::collections::HashMap;

pub fn analyse_program(modules: &HashMap<String, Module>) -> Result<(), Vec<Error>> {
   /* Statically analyses a program
    *
    * analysis results contain
    * - escape analysis on values
    * - function last-statement returns
    * - errors
    */

    let mut type_checker = TypeChecker::new(modules);
    let mut errors = vec![];

    for module in modules.values() {
        for function_collection in [module.instance_methods(), module.module_methods()] {
            for function in function_collection.values() {
                if let Err(es) = type_checker.check_function_types(function) {
                    for error in es {
                        errors.push(
                            error.set_line(module.raw_lines())
                                 .set_filename(module.filename())
                        );
                    }
                }
            }
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}
