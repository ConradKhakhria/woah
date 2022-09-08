use crate::error::*;
use crate::parse::Module;
use std::collections::HashMap;

pub struct AnalysisResults<'a> {
    modules: &'a HashMap<String, Module>,

}


impl<'a> AnalysisResults<'a> {
    fn analyse(modules: &'a HashMap<String, Module>) -> Result<Self, Vec<Error>> {





        Err(vec![])
    }
}


pub fn analyse_program<'a>(modules: &'a HashMap<String, Module>) -> Result<AnalysisResults<'a>, Vec<Error>> {
   /* Statically analyses a program from its modules
    *
    * this static analysis contains type-checking and escape analysis
    *
    * returns: the results of analysis or a list of errors
    */

    let mut results = AnalysisResults { modules };


    Err(vec![])
}
