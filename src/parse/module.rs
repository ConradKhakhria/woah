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

pub struct Module {
    name: String,
    functions: HashMap<String, Function>,
    imports: Vec<Import>
}


impl Module {
    pub fn from_filepath(path: &Path) -> Result<Self, Vec<Error>> {
        /* Reads and parses a module from a file */


        Err(vec![])
    }
}
