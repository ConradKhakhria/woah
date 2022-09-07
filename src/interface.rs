use crate::error::*;
use crate::parse::Module;
use std::path::Path;


pub fn build(args: &Vec<String>) -> Result<(), Vec<Error>> {
    /* Builds a project */

    let module = Module::from_filepath(Path::new(&args[2]))?;

    Ok(())
}
