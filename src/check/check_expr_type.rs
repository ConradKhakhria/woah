use crate::{
    check::TypeChecker,
    error::{ Error, ErrorKind, collect_errors },
    parse::{ Class, Expr, ExprType, TypeKind }
};
use std::{
    collections::HashMap,
    rc::Rc
};

pub fn determine_expr_type<'s, 't>(expr: &Expr<'s, 't>, type_checker: &TypeChecker<'s, 't>) -> Result<Rc<TypeKind<'s, 't>>, Vec<Error>> {
    /* Attempts to determine the type of an expression */

    match &expr.expr_type {
        ExprType::ArrayIndexing { array, index } => {
            let array_type = determine_expr_type(&array, type_checker)?;
            let index_type = determine_expr_type(&index, type_checker)?;

            if let TypeKind::Int = &*index_type { } else {
                return Error::new(ErrorKind::TypeError)
                            .set_position(expr.last_token.position())
                            .set_message("Arrays can only ben indexed by integers")
                            .into()
            }

            match &*array_type {
                TypeKind::List(deriv) => Ok(Rc::clone(deriv)),
                _ => Error::new(ErrorKind::TypeError)
                        .set_position(expr.first_token.position())
                        .set_message("Cannot index a non-array type")
                        .into()
            }
        }


        ExprType::ArrayLiteral { elems } => {
            if elems.len() == 0 {
                return Ok(Rc::new(TypeKind::EmptyList));
            }

            let first_type = determine_expr_type(&elems[0], type_checker)?;

            for expr in elems[1..].iter() {
                if determine_expr_type(expr, type_checker)? != first_type {
                    return Error::new(ErrorKind::TypeError)
                                .set_position(expr.first_token.position())
                                .set_message(format!("Expected list of type {}", first_type))
                                .into();
                }
            }

            Ok(Rc::new(TypeKind::List(first_type)))
        }


        ExprType::AttrRes { parent, attr_name } => {
            let parent_class_type = determine_expr_type(parent, type_checker)?;
            let parent_class_name = match &*parent_class_type {
                TypeKind::HigherOrder { name, .. } => name,
                _ => unimplemented!()
            };

            let class = match type_checker.classes.get(&parent_class_name.to_string()) {
                Some(c) => c,
                None => return Error::new(ErrorKind::NameError)
                                    .set_position(expr.first_token.position())
                                    .set_message(format!("Unknown class '{}'", parent_class_name))
                                    .into()
            };

            match class.attribute_type(attr_name, &type_checker.current_class) {
                Some(tp) => Ok(tp),
                None => Error::new(ErrorKind::TypeError)
                            .set_position(expr.last_token.position())
                            .set_message(format!("class '{}' has no attribute '{}'", parent_class_name, attr_name))
                            .into()
            }
        }


        ExprType::Compound { operator, left, right } => {
            let mut errors = vec![];
                    
            let mut left_type = determine_expr_type(left, type_checker);
            let mut right_type = determine_expr_type(right, type_checker);

            if let Err(ref mut es) = left_type {
                errors.append(es);
            }
        
            if let Err(ref mut es) = right_type {
                errors.append(es);
            }

            let res = match (&*left_type.unwrap(), &*right_type.unwrap()) {
                (TypeKind::Int, TypeKind::Int)     => Rc::new(TypeKind::Int),
                (TypeKind::Float, TypeKind::Int)   => Rc::new(TypeKind::Float),
                (TypeKind::Int, TypeKind::Float)   => Rc::new(TypeKind::Float),
                (TypeKind::Float, TypeKind::Float) => Rc::new(TypeKind::Float),
                _ => return Error::new(ErrorKind::TypeError)
                            .set_position(left.first_token.position())
                            .set_message("Expected numeric type")
                            .into()
            };

            Ok(if vec!["==", "!=", "<", ">", "<=", ">="].contains(&&operator.to_string()[..]) {
                Rc::new(TypeKind::Bool)
            } else {
                res
            })
        }


        ExprType::FunctionCall { function, args } => {
            let function_type = determine_expr_type(function, type_checker)?;
            let mut errors = vec![];

            let (function_arg_types, function_return_type) = match &*function_type {
                TypeKind::Function { args, return_type } => (args, return_type),
                _ => return Error::new(ErrorKind::TypeError)
                                .set_position(expr.last_token.position())
                                .set_message(format!("Cannot call non-function value"))
                                .into()
            };

            for (i, arg) in args.iter().enumerate() {
                match determine_expr_type(arg, type_checker) {
                    Ok(tp) => {
                        if &*tp != &*function_arg_types[i] {
                            errors.push(Error::new(ErrorKind::TypeError)
                                                .set_position(arg.first_token.position())
                                                .set_message(format!("Expected type {}, received {}", function_arg_types[i], tp)));
                        }
                    },

                    Err(ref mut es) => errors.append(es)
                }
            }

            if errors.is_empty() {
                if let Some(tp) = function_return_type {
                    Ok(Rc::clone(tp))
                } else {
                    Error::new(ErrorKind::TypeError)
                        .set_position(expr.first_token.position())
                        .set_message("Function does not return a value")
                        .into()
                }
            } else {
                Err(errors)
            }
        }



        _ => unimplemented!()
    }
}
