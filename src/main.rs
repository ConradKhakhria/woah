mod analysis;
mod compile;
mod interpret;
mod message;
mod line;
mod parse;
mod token;

use std::path::Path;

const INTERPRETED: bool = true; // this is not a permanent fixture

fn main() {
    let filename = std::env::args()
                    .filter(|a| a.ends_with(".woah"))
                    .next()
                    .expect("No file supplied");

    /* Parsing */

    let root_module = match parse::Module::from_filepath(Path::new(&filename)) {
        Ok(m) => m,
        Err(es) => {
            for e in es {
                eprintln!("{}", e);
            }

            return;
        }
    };

    /* Static analysis */

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


    /* Interpretation or compilation */

    if INTERPRETED {

        let mut program = interpret::ProgramState::new(&root_module);

        program.evaluate();

    } else {

        let compiler = compile::Compiler::new();
        let result = compiler.compile_single_file(&root_module);
    
        compiler
            .print_warnings()
            .print_errors();
    
        if result.is_ok() {
            println!("Done!");
        }

    }
}
