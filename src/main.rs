mod analysis;
mod error;
mod interface;
mod line;
mod parse;
mod token;


fn main() {
    let mut interface = interface::Interface::new();
    let command = std::env::args().nth(1);

    if command.is_none() {
        panic!("Liszp: please supply a command");
    }

    let res = match command.unwrap().as_str() {
        "build" => interface.build(),
        c => panic!("Liszp: unknown command '{}'", c)
    };

    if let Err(es) = res {
        for e in es {
            eprintln!("{}", e);
        }
    }
}
