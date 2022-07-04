use crate::{
    interpret::value::Value,
};
use std::{
    cell::RefCell,
    rc::Rc,
    string::ToString
};


struct StackElement<'m> {
    name: String,
    value: Option<Rc<RefCell<Value<'m>>>>
}


impl<'m> StackElement<'m> {
    fn is_called<'a, S: Into<&'a str>>(&self, name: S) -> bool {
        /* Returns whether a stack element has a given name */

        name.into() == self.name
    }
}


#[derive(Debug)]
pub (super) struct StackFrame<'m> {
    names: Vec<Vec<String>>,
    values: Vec<Vec<Option<Rc<RefCell<Value<'m>>>>>>
}


impl<'m> StackFrame<'m> {
    pub fn new() -> Self {
        StackFrame {
            names:  Vec::new(),
            values: Vec::new()
        }
    }


    pub fn get_value<T: ToString>(&self, value_name: &T) -> Option<Rc<RefCell<Value<'m>>>> {
        /* Attempts to retrieve a value from the stack frame */

        let value_name = value_name.to_string();

        for scope_index in 0..self.names.len() {
            for (i, name) in self.names[scope_index].iter().enumerate() {
                if *name == value_name {
                    if let Some(v) = &self.values[scope_index][i] {
                        return Some(Rc::clone(v));
                    }
                }
            }
        }

        None
    }


    pub fn add_scope(&mut self) {
        /* Adds a new scope to the stack frame */

        self.names.push(vec![]);
        self.values.push(vec![]);
    }


    pub fn pop_scope(&mut self) {
        /* Removes the top scope from the stack frame */

        self.names.pop();
        self.values.pop();
    }


    pub fn add_value<T: ToString>(&mut self, value_name: &T, value: &Option<Rc<RefCell<Value<'m>>>>) {
        /* Adds a value to the stack frame */

        self.names.last_mut().unwrap().push(value_name.to_string());
        self.values.last_mut().unwrap().push(match value {
            Some(v) => Some(Rc::clone(v)),
            None => None
        });
    }


    pub fn remove_value<T: ToString>(&mut self, value_name: &T) {
        /* Attempts to remove a value from the stack frame */
    
        let value_name = value_name.to_string();

        for scope_index in (0..self.values.len()).rev() {
            for i in (0..self.values[scope_index].len()).rev() {
                if self.names[scope_index][i] == value_name {
                    self.names[scope_index].swap_remove(i);
                    self.values[scope_index].swap_remove(i);
                }
            }
        }
    }
}
