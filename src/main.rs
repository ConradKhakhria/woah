use std::path::Path;

mod error;
mod line;
mod parse;
mod token;

fn main() {
    let filename = std::env::args()
                    .filter(|a| a.ends_with(".woah"))
                    .next()
                    .expect("No file supplied");

    let root_module = match parse::Module::from_filepath(Path::new(&filename)) {
        Ok(m) => m,
        Err(es) => {
            for e in es {
                eprintln!("{}", e);
            }

            return;
        }
    };

    println!("{:#?}", root_module);
}

