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


pub (super) struct StackFrame {
    names: Vec<String>,
    values: Vec<Rc<RefCell<Value>>>
}


impl StackFrame {
    pub fn new() -> Self {
        StackFrame {
            names: Vec::new(),
            values: Vec::new()
        }
    }


    pub fn get_value<T: ToString>(&self, value_name: &T) -> Option<Rc<RefCell<Value>>> {
        /* Attempts to retrieve a value from the stack frame */

        let value_name = value_name.to_string();

        for (i, name) in self.names.iter().enumerate() {
            if name == &value_name {
                return Some(Rc::clone(&self.values[i]))
            }
        }

        None
    }
}
