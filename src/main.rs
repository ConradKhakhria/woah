mod error;
mod lexer;
mod parse;
mod token;

fn main() {
    let filename = std::env::args()
                    .filter(|a| a.ends_with(".woah"))
                    .next()
                    .unwrap();

    if let Err(es) = hacky_testbed(filename.clone()) {
        let source_lines = std::fs::read_to_string(&filename)
                                    .unwrap()
                                    .lines()
                                    .map(|s| s.into())
                                    .collect();

        for err in es {
            eprintln!("{}", err.set_line(&source_lines));
        }
    }
}

fn hacky_testbed(filename: String) -> Result<(), Vec<crate::error::Error>> {
    /* This will be the testbed before the environment system is set up properly */

    let source = std::fs::read_to_string(&filename).unwrap();
    let tokens = lexer::tokenise(&source, &filename, (1, 1))?;

    let parsed_type = parse::TypeKind::from_tokens(&tokens[..])?;

    println!("{:#?}", parsed_type);

    Ok(())
}
