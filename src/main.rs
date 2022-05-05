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
            eprintln!("{}", err.set_line(&source_lines).set_filename(&filename));
        }
    }
}

fn hacky_testbed(filename: String) -> Result<(), Vec<crate::error::Error>> {
    /* This will be the testbed before the environment system is set up properly */

    let source = std::fs::read_to_string(&filename).unwrap();
    let tokens = token::tokenise(&source, &filename, (1, 1))?;
    let lines = line::create_lines(&tokens);
    let classes = parse::collect_classes(&lines)?;
    let mut typechecker = check::TypeChecker::new(classes);

    /* Check expr type */

    let statements_source = concat!(

        "let x = 5\n",
        "let y = 6\n",
        "let pair = Pair.new(x, y)\n",

        "if pair.max() == 6\n",
        "   pair.x = 4\n"
    
    ).to_string();
    let tokens = token::tokenise(&statements_source, &"<hi>", (1, 1))?;
    let lines = line::create_lines(&tokens);
    let mut statement_block = parse::parse_statement_block(&lines)?;

    Err(typechecker.check_statement_block_type(&mut statement_block))
}
