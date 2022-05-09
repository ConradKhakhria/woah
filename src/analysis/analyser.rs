use crate::{
    error::{ Error, ErrorKind },
    parse::{
        Function,
        Module,
        Statement,
        StatementType,
        TypeKind
    }
};
use std::{
    collections::HashMap,
    rc::Rc,
    string::ToString
};


pub struct Scope<'m> {
    frames: Vec<Vec<&'m String>>,
    values: HashMap<&'m String, (Rc<TypeKind>, bool)>
}


impl<'m> Scope<'m> {
    pub fn new() -> Self {
        Scope {
            frames: vec![],
            values: HashMap::new()
        }
    }


    pub fn add_value(&mut self, name: &'m String, v_type: Rc<TypeKind>, constant: bool) -> Option<Error> {
        /* Adds a value to the scope */

        if self.frames.is_empty() {
            self.frames.push(vec![]);
        }

        if self.values.contains_key(name) {
            Error::new(ErrorKind::NameError)
                .set_message(format!("Cannot redefine value '{}'", name))
                .into()
        } else {
            self.frames.last_mut().unwrap().push(name);
            self.values.insert(name, (Rc::clone(&v_type), constant));

            None
        }
    }


    pub fn get_value_type<T: ToString>(&self, ident: &T) -> Option<Rc<TypeKind>> {
        /* Attempts to get a value from the scope */

        if let Some((tp, _)) =self.values.get(&ident.to_string()) {
            Some(Rc::clone(tp))
        } else {
            None
        }
    }


    pub fn is_value_constant<T: ToString>(&self, ident: &T) -> Option<bool> {
        /* Attempts to get a value from the scope */

        if let Some((_, c)) =self.values.get(&ident.to_string()) {
            Some(*c)
        } else {
            None
        }
    }
}


pub struct Analyser<'m> {
    pub modules: HashMap<&'m String, &'m Module>,
    pub current_module: Option<&'m Module>,
    pub current_function: Option<&'m Function>,
    pub current_scope: Scope<'m>,
    pub current_position: (usize, usize)
}


impl<'m> Analyser<'m> {
    pub fn new() -> Self {
        /* Creates a new static analyser */

        Analyser {
            modules: HashMap::new(),
            current_module: None,
            current_function: None,
            current_scope: Scope::new(),
            current_position: (1, 1)
        }
    }


    /* Static analysis */


    pub fn analyse_statement(&mut self, statement: &Statement) -> Vec<Error> {
        /* Statically analyses a statement in a function */

        let mut error = vec![];
    
        match &statement.stmt_type {
            _ => unimplemented!()
        }

        error
    }


    pub fn analyse_function(&mut self, function: &'m Function) -> Vec<Error> {
        /* Statically analyses a function */

        self.current_function = Some(function);
        self.current_scope = Scope::new();

        for arg in function.args.iter() {
            self.current_scope.add_value(&arg.arg_name, Rc::clone(&arg.arg_type), true);
        }

        let mut errors = vec![];

        for statement in function.body.iter() {
            errors.append(&mut self.analyse_statement(statement));
        }

        errors
    }


    pub fn analyse_module(&mut self, module: &'m Module) -> Vec<Error> {
        /* Analyses all functions in a module, recursing on imports */

        if self.modules.contains_key(&module.name) {
            return vec![];
        }

        let mut errors = vec![];

        for import in module.imports.iter() {
            errors.append(&mut self.analyse_module(&import.module));
        }

        for function in module.functions.values() {
            errors.append(&mut self.analyse_function(function));
        }

        self.modules.insert(&module.name, &module);

        errors
    }
}
