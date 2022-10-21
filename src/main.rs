mod analysis;
mod error;
mod interface;
mod line;
mod parse;
mod token;


fn main() {
    let command = std::env::args().nth(1).expect("Liszp: please supply a command");

    let res = match command.as_str() {
        "build" => interface::build(),
        "new"   => interface::create_project(),
        c => panic!("Liszp: unknown command '{}'", c)
    };

    if let Err(es) = res {
        for e in es {
            eprintln!("{}", e);
        }
    }
}
