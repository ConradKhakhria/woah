use crate::analysis::check_types;
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

    if let Err(ref mut es) = check_types(modules) {
        errors.append(es);
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}
