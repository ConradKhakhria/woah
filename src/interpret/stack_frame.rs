use crate::{
    interpret::value::Value,
    parse::{
        Expr,
        ExprKind,
        Function
    }
};
use std::{
    cell::RefCell,
    rc::Rc,
    string::ToString
};


pub (super) struct StackFrame<'m> {
    names: Vec<String>,
    values: Vec<Option<Rc<RefCell<Value<'m>>>>>
}


impl<'m> StackFrame<'m> {
    pub fn new() -> Self {
        StackFrame {
            names: Vec::new(),
            values: Vec::new()
        }
    }


    pub fn get_value<T: ToString>(&self, value_name: &T) -> Option<Rc<RefCell<Value<'m>>>> {
        /* Attempts to retrieve a value from the stack frame */

        let value_name = value_name.to_string();

        for (i, name) in self.names.iter().enumerate() {
            if name == &value_name {
                return match &self.values[i] {
                    Some(v) => Some(Rc::clone(v)),
                    None => None
                }
            }
        }

        None
    }


    pub fn add_value<T: ToString>(&mut self, value_name: &T, value: &Option<Rc<RefCell<Value<'m>>>>) {
        /* Adds a value to the stack frame */

        self.names.push(value_name.to_string());
        self.values.push(match value {
            Some(v) => Some(Rc::clone(v)),
            None => None
        });
    }
}
