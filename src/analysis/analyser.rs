use crate::error::*;
use crate::parse::Function;
use crate::parse::Module;
use crate::parse::TypeKind;
use std::collections::HashMap;
use std::rc::Rc;


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

    }   

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}
