use crate::analysis::check_function_types;
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

    let mut errors = vec![];

    for module in modules.values() {
        for function_collection in [module.instance_methods(), module.module_methods()] {
            for function in function_collection.values() {
                match check_function_types(module, function) {
                    Ok(_) => {},
                    Err(ref mut es) => errors.append(es)
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
