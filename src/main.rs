mod error;
mod interface;
mod line;
mod parse;
mod token;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let res = match args[1].as_str() {
        "build" => interface::build(&args),
        c => panic!("Liszp: unknown command '{}'", c)
    };

    if let Err(es) = res {
        for e in es {
            eprintln!("{}", e);
        }
    }
}
