mod analyser;
mod builtin;
mod type_checking;

pub use analyser::Analyser as Analyser;

pub use builtin::type_of_builtin_function as type_of_builtin_function;

pub use type_checking::get_expr_type as get_expr_type;
