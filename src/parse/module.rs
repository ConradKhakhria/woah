use crate::{
    parse::{
        Function,
        Import
    }
};
use std::collections::HashMap;

pub struct Module<'s, 't> {
    functions: HashMap<&'s str, Function<'s, 't>>,
    imports: Vec<Import<'s, 't>>
}
