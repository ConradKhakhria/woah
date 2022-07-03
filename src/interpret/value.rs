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

    NoValue,

    String(String),
}


impl std::fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Array(xs) => {
                let mut string = "[".to_string();

                for x in xs.iter() {
                    string = format!("{}{}, ", string, x.borrow());
                }

                write!(f, "{}]", &string[..string.len() - 2])
            },

            Value::Bool(x) => write!(f, "{}", x),

            Value::Float(x) => write!(f, "{}", x),

            Value::Function(func) => write!(f, "<function '{}'>", func.name),

            Value::Int(x) => write!(f, "{}", x),

            Value::NoValue => write!(f, "<no value>"),

            Value::String(x) => write!(f, "{}", x)
        }
    }
}


impl<'m> Value<'m> {
    pub fn rc_refcell(self) -> Rc<RefCell<Self>> {
        /* Wraps self in an Rc */

        Rc::new(RefCell::new(self))
    }
}
