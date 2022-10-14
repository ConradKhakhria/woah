use crate::analysis::analyse_program;
use crate::compile::compile;
use crate::error::*;
use crate::parse::Module;
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::path::PathBuf;


/* Interface */

pub fn build() -> Result<(), Vec<Error>> {
    /* Builds a project (location and flags from std::env::args()) */

    // get modules
    let root_dir_name = env::args().nth(2).unwrap_or(".".into());
    let src_path = get_source_path(&root_dir_name)?;
    let root_path = src_path.parent().unwrap().to_path_buf();

    let mut modules = HashMap::new();
    collect_modules(&src_path, &mut modules)?;

    let static_analysis_results = analyse_program(&mut modules)?;

    for module in modules.values() {
        compile(&root_path, module, &static_analysis_results);
    }

    Ok(())
}


pub fn create_project() -> Result<(), Vec<Error>> {
    /* Creates a new project with a name specified in the args */

    let root_path = get_project_path(env::args().nth(2))?;
    let src_path = root_path.join("src");
    let target_path = root_path.join("target");
    let generated_c_path = target_path.join("generated_c");

    let mut builder = std::fs::DirBuilder::new();
    builder.recursive(true);

    for path in [&generated_c_path, &src_path] {
        if builder.create(path).is_err() {
            return Error::new(ErrorKind::FileSystemError)
                        .set_message(format!("unable to create dir {}", path.display()))
                        .into();
        }
    }

    // The success of this is guaranteed by the checks in get_project_path()
    let project_name = root_path.file_name().unwrap().to_str().unwrap();
    let mut files_to_create = [
        (src_path.join(project_name), "woah"),
        (root_path.join("README"), "txt"),
        (root_path.join(".gitignore"), "")
    ];

    for (file_to_create, file_extension) in files_to_create.iter_mut() {
        if !file_extension.is_empty() {
            file_to_create.set_extension(file_extension);
        }

        if let Err(e) = File::create(file_to_create) {
            return Error::new(ErrorKind::FileSystemError)
                        .set_message(e.to_string())
                        .into();
        }
    }

    Ok(())
}


/* Module and filesystem */

fn collect_modules(module_cursor: &PathBuf, modules: &mut HashMap<String, Module>) -> Result<(), Vec<Error>> {
    /* Collects all modules in a project */

    let mut errors = vec![];

    if module_cursor.is_dir() {
        for entry in std::fs::read_dir(module_cursor).unwrap() {
            match entry {
                Ok(dir_entry) => {
                    let path = dir_entry.path();

                    if let Err(ref mut es) = collect_modules(&path, modules) {
                        errors.append(es);
                    }
                }

                Err(e) => errors.push(
                    Error::new(ErrorKind::ModuleError)
                        .set_message(e.to_string())
                )
            }
        }
    } else {
        let module = Module::from_filepath(module_cursor)?;

        if modules.contains_key(module.module_name()) {
            errors.push(
                Error::new(ErrorKind::NameError)
                    .set_position(module.first_position())
                    .set_line(module.raw_lines())
                    .set_message(format!("two modules in this path named '{}'", module.module_name()))
            );
        } else {
            modules.insert(module.module_name().clone(), module);
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}


fn get_project_path(possible_path: Option<String>) -> Result<PathBuf, Vec<Error>> {
    /* Tests whether possible_path is a valid project path */

    let cwd = match env::current_dir() {
        Ok(path) => path,
        Err(e) => return Error::new(ErrorKind::FileSystemError)
                                    .set_message(e.to_string())
                                    .into()
    };

    match possible_path {
        Some(name) => {
            let path = cwd.join(name);

            match path.file_name() {
                Some(name) => {
                    match name.to_str() {
                        Some(name) => {
                            if !name.chars().all(char::is_alphabetic) {
                                Error::new(ErrorKind::NameError)
                                    .set_message("your project's name can only consist of letters")
                                    .into()
                            } else if name.chars().next().unwrap().is_lowercase() {
                                Error::new(ErrorKind::NameError)
                                    .set_message("your project's name must begin with an uppercase letter")
                                    .into()
                            } else {
                                Ok(path)
                            }
                        }

                        None => Error::new(ErrorKind::FileSystemError)
                                    .set_message("your project's name must be standard ASCII")
                                    .into()
                    }
                }

                None => Error::new(ErrorKind::FileSystemError)
                            .set_message("Your project path needs a name")
                            .into()
            }
        }

        None => Error::new(ErrorKind::InterfaceError)
                    .set_message("your new project needs a name")
                    .into() 
    }
}


fn get_source_path(root_dir_name: &String) -> Result<PathBuf, Vec<Error>> {
    /* Gets the path of the source directory */

    let mut root_path = PathBuf::from(root_dir_name);
    root_path.push("src");

    match std::fs::metadata(&root_path) {
        Ok(_) => Ok(root_path),

        Err(e) => {
            if let std::io::ErrorKind::NotFound = e.kind() {
                Error::new(ErrorKind::FileSystemError)
                    .set_message("the path provided is not a valid project directory")
                    .into()   
            } else {
                Ok(root_path)
            }
        }
    }
}
