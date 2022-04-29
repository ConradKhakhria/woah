use crate::{
    parse::{
        Class,
        TypeKind
    }
};

use std::collections::HashMap;

pub struct TypeChecker<'s, 't> {
    pub classes: HashMap<String, Class<'s, 't>>,
    pub current_class: String,
    pub current_scope: Vec<(String, TypeKind<'s, 't>)>
}

impl<'s, 't> TypeChecker<'s, 't> {
    pub fn new(classes: HashMap<String, Class<'s, 't>>) -> Self {
        /* Creates a new type checker from a map of a program's classes */

        TypeChecker {
            classes,
            current_class: String::new(),
            current_scope: vec![]
        }
    }
}
