mod analyser;
mod escape_analysis;
mod type_checker;

pub use analyser::analyse_program as analyse_program;

pub use escape_analysis::EscapeResult as EscapeResult;

pub (in crate::analysis) use type_checker::check_function_types as check_function_types;
