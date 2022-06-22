use crate::{
    message::{ Message },
    parse::{
        Module
    },
};
use std::collections::HashMap;

pub struct Compiler {
    _files: HashMap<String, Vec<String>>,
    errors: Vec<Message>,
    warnings: Vec<Message>
}


impl Compiler {
    /* Init */

    pub fn new() -> Self {
        Compiler {
            _files: HashMap::new(),
            errors: vec![],
            warnings: vec![]
        }
    }

    
    /* Utils */


    pub fn print_warnings(&self) -> &Self {
        /* Prints all warnings */

        for w in self.warnings.iter() {
            eprintln!("{}", w);
        }

        self
    }


    pub fn print_errors(&self) -> &Self {
        /* Prints all errors */

        for e in self.errors.iter() {
            eprintln!("{}", e);
        }

        self
    }


    /* Compilation */


    pub fn compile_single_file(&self, _module: &Module) -> Result<(), ()> {
        /* Compiles a single file with no regard to imports */

        Ok(())
    }
}
