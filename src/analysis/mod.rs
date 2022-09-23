mod analyser;
mod type_checker;

pub use analyser::analyse_program as analyse_program;

pub (in crate::analysis) use type_checker::TypeChecker as TypeChecker;
