use lazy_static::lazy_static;
use std::collections::HashSet;

pub fn is_builtin(function_name: &str) -> bool {
    lazy_static! {
        static ref BUILT_IN_FUNCTIONS: HashSet<&'static str> = HashSet::from([
            "println"
        ]);
    }

    BUILT_IN_FUNCTIONS.contains(function_name)
}
