use crate::error::*;
use crate::parse::Module;
use std::path::Path;


pub struct Compiler {
    modules: Vec<Module>
}


impl Compiler {
    pub fn new() -> Self {
        Compiler { modules: vec![] }
    }


    /* User interface */


    pub fn build(&mut self) -> Result<(), Vec<Error>> {
        /* Builds a project (location and flags from std::env::args()) */





        Ok(())
    }

}
