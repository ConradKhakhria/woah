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
    values: Vec<Rc<RefCell<Value<'m>>>>
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
                return Some(Rc::clone(&self.values[i]))
            }
        }

        None
    }


    pub fn add_value<T: ToString>(&mut self, value_name: &T, value: &Rc<RefCell<Value<'m>>>) {
        /* Adds a value to the stack frame */

        let value_name = value_name.to_string();

        self.names.push(value_name);
        self.values.push(Rc::clone(value));
    }
}
