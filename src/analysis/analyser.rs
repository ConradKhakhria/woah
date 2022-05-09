use crate::{
    error::Error,
    parse::{
        Function,
        Import,
        Module,
        TypeKind
    }
};
use std::{
    collections::HashMap,
    rc::Rc
};


struct Scope<'m> {
    val_names: Vec<&'m String>,
    val_types: Vec<Rc<TypeKind>>,
    val_constant: Vec<bool>
}

pub struct Analyser<'m> {
    modules: HashMap<&'m String, &'m Module>,
    current_module: Option<&'m Module>,
    current_function: Option<&'m Function>,
    current_scope: Vec<Scope<'m>>
}


impl<'m> Analyser<'m> {
    pub fn new() -> Self {
        /* Creates a new static analyser */

        Analyser {
            modules: HashMap::new(),
            current_module: None,
            current_function: None,
            current_scope: vec![]
        }
    }


    pub fn analyse_module(&mut self, module: &'m mut Module) -> Vec<Error> {
        /* Analyses all functions in a module, recursing on imports */

        if self.modules.contains_key(&module.name()) {
            return vec![];
        }

        let mut errors = vec![];

        for import in module.imports().iter_mut() {
            errors.append(&mut self.analyse_module(&mut import.module));
        }





        self.modules.insert(&module.name(), &module);
    }
}


