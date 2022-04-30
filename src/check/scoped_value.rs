use crate::{
    parse::TypeKind
};
use std::rc::Rc;

pub struct ScopedValue<'s, 't> {
    pub var_name: String,
    pub constant: bool,
    pub var_type: Option<Rc<TypeKind<'s, 't>>>
}


impl<'s, 't> ScopedValue<'s, 't> {

}

