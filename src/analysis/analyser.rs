use crate::analysis::check_function_types;
use crate::analysis::EscapeResult;
use crate::error::*;
use crate::parse::Module;
use std::collections::HashMap;


pub struct StaticAnalysisResults<'m> {
    escape_analysis_results: HashMap<&'m String, EscapeResult<'m>>
}


pub fn analyse_program<'m>(modules: &'m mut HashMap<String, Module>) -> Result<StaticAnalysisResults<'m>, Vec<Error>> {
   /* Statically analyses a program
    *
    * analysis results contain
    * - escape analysis on values
    * - function last-statement returns
    * - errors
    */

    let mut escape_analysis_results = HashMap::new();
    let mut errors = vec![];

    for module in modules.values() {
        for function_collection in [module.instance_methods(), module.module_methods()] {
            for function in function_collection.values() {
                match check_function_types(function, modules) {
                    Ok(()) => {
                        escape_analysis_results.insert(&function.name, EscapeResult::analyse_function(function));
                    },

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
        Ok(StaticAnalysisResults { escape_analysis_results })
    } else {
        Err(errors)
    }
}
