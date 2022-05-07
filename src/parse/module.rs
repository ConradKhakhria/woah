use crate::{
    error::Error,
    parse::{
        Function,
        Import
    }
};
use std::{
    collections::HashMap,
    path::Path
};

pub struct Module<'s, 't> {
    name: String,
    functions: HashMap<&'s str, Function<'s, 't>>,
    imports: Vec<Import<'s, 't>>
}


impl<'s, 't> Module<'s, 't> {
    pub fn from_filepath(path: &Path) -> Result<Self, Vec<Error>> {
        /* Reads and parses a module from a file */


        Err(vec![])
    }
}
