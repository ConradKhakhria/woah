mod check;
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
    let classes = parse::collect_classes(&lines)?;
    let typechecker = check::TypeChecker::new(classes);

    /* Obtain an expression */

    let expr_string = "1 + 2 + 3";
    let expr_tokens = token::tokenise(&expr_string, &"<hi>", (1, 1))?;
    let expr = parse::parse_expression(&expr_tokens, (1, 1))?;

    println!("{:#?}", check::determine_expr_type(&expr, &typechecker));

    Ok(())
}
