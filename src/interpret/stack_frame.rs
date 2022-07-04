use crate::{
    interpret::value::Value,
};
use std::{
    cell::RefCell,
    rc::Rc,
    string::ToString
};

#[derive(Debug)]
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
    values: Vec<Vec<StackElement<'m>>>
}


impl<'m> StackFrame<'m> {
    pub fn new() -> Self {
        StackFrame {
            values: Vec::new()
        }
    }


    pub fn get_value(&self, value_name: &str) -> Option<Rc<RefCell<Value<'m>>>> {
        /* Attempts to retrieve a value from the stack frame */

        for scope in self.values.iter() {
            for element in scope.iter() {
                if element.is_called(value_name) {
                    match &element.value {
                        Some(v) => return Some(Rc::clone(v)),
                        None => panic!("'{}' has not been assigned a value yet", value_name)
                    }
                }
            }
        }

        None
    }


    pub fn add_scope(&mut self) {
        /* Adds a new scope to the stack frame */

        self.values.push(vec![]);
    }


    pub fn pop_scope(&mut self) {
        /* Removes the top scope from the stack frame */

        self.values.pop();
    }


    pub fn add_value<S: ToString>(&mut self, value_name: S, value: &Option<Rc<RefCell<Value<'m>>>>) {
        /* Adds a value to the stack frame */

        self.values.last_mut().unwrap().push(
            StackElement {
                name: value_name.to_string(),
                value: match value {
                    Some(v) => Some(Rc::clone(v)),
                    None => None
                }
            }
        );
    }


    pub fn remove_value(&mut self, value_name: &str) {
        /* Attempts to remove a value from the stack frame */

        for scope_index in 0..self.values.len() {
            for element_index in 0..self.values[scope_index].len() {
                if self.values[scope_index][element_index].is_called(value_name) {
                    self.values[scope_index].swap_remove(element_index);
                    return;
                }
            }
        }
    }
}
