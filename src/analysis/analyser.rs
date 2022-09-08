use crate::parse::Module;
use std::collections::HashMap;

pub struct Analyser<'a> {
    modules: &'a HashMap<String, Module>
}


impl<'a> Analyser<'a> {
    pub fn new(modules: &'a HashMap<String, Module>) -> Self {
        Analyser { modules }
    }
}
