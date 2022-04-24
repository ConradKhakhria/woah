mod error;
mod line;
mod parse;
mod token;

fn main() {
    let filename = std::env::args()
                    .filter(|a| a.ends_with(".woah"))
                    .next()
                    .expect("No file supplied");

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
    let tokens = token::tokenise(&source, &filename, (1, 1))?;
    let lines = line::create_lines(&tokens);

    let mut functions = vec![];
    let mut errors = vec![];

    for line in lines.iter() {
        match parse::Function::from_line(line) {
            Ok(func) => functions.push(func),
            Err(ref mut es) => errors.append(es)
        }
    }

    if errors.len() > 0 {
        return Err(errors)
    }

    println!("{:#?}", functions);

    Ok(())
}
