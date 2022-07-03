use crate::parse::Function;
use std::{
    cell::RefCell,
    rc::Rc,
};


#[derive(Clone, Debug)]
pub (super) enum Value<'m> {
    Array(Vec<Rc<RefCell<Value<'m>>>>),

    Bool(bool),

    Float(f64),

    Function(&'m Function),

    Int(i64),

    String(String),
}


impl<'m> Value<'m> {
    pub fn rc_refcell(self) -> Rc<RefCell<Self>> {
        /* Wraps self in an Rc */

        Rc::new(RefCell::new(self))
    }
}
