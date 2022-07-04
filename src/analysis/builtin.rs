use crate::parse::TypeKind;
use std::rc::Rc;

pub fn type_of_builtin_function(function_name: &str) -> Option<Rc<TypeKind>> {
    /* Creates a TypeKind out of a built-in function */

    match function_name {
        "println" => Some(Rc::new(
            TypeKind::Function {
                args: vec![ TypeKind::Any.rc() ],
                return_type: None
            }
        )),

        _ => None
    }
}
