mod analysis;
mod error;
mod line;
mod parse;
mod token;

use std::path::Path;

fn main() {
    let filename = std::env::args()
                    .filter(|a| a.ends_with(".woah"))
                    .next()
                    .expect("No file supplied");

    let mut root_module = match parse::Module::from_filepath(Path::new(&filename)) {
        Ok(m) => m,
        Err(es) => {
            for e in es {
                eprintln!("{}", e);
            }

            return;
        }
    };

    let static_analyser = analysis::Analyser::new();
    let errors = static_analyser.analyse_module(&mut root_module);

    println!("{:#?}", errors);
}

