mod analysis;
mod message;
mod line;
mod parse;
mod token;

use std::path::Path;

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

    for function in root_module.functions.values() {
        println!("{}", function);
    }

    let mut static_analyser = analysis::Analyser::new();

    static_analyser.analyse_module(&root_module);

    for w in static_analyser.get_warnings().iter() {
        eprintln!("{}", w);
    }

    if let Err(es) = static_analyser.get_analysis_result() {
        for e in es.iter() {
            eprintln!("{}", e);
        }

        return;
    }

    println!("Done!");
}
