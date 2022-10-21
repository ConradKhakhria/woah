use crate::analysis::check_function_types;
use crate::error::*;
use crate::parse::Module;
use std::collections::HashMap;


pub struct StaticAnalysisResults {

}


pub fn analyse_program<'m>(modules: &'m mut HashMap<String, Module>) -> Result<StaticAnalysisResults, Vec<Error>> {
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
                match check_function_types(function, modules) {
                    Ok(()) => {},

                    Err(es) => {
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
    }

    if errors.is_empty() {
        Ok(StaticAnalysisResults {  })
    } else {
        Err(errors)
    }
}
