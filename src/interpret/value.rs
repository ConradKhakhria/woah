use std::{
    cell::RefCell,
    rc::Rc,
};


#[derive(Clone)]
pub (super) enum Value {
    Array(Vec<Rc<RefCell<Value>>>),

    Bool(bool),

    Int(i64),

    Float(f64),

    String(String),
}


impl Value {
    pub fn rc_refcell(self) -> Rc<RefCell<Self>> {
        /* Wraps self in an Rc */

        Rc::new(RefCell::new(self))
    }
}
