mod analyser;
mod builtin;
mod type_checking;

pub use analyser::Analyser as Analyser;

pub use builtin::is_builtin as is_builtin;

pub use type_checking::get_expr_type as get_expr_type;
