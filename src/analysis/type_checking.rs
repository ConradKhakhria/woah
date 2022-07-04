use crate::{
    analysis::{ Analyser, type_of_builtin_function },
    message::{ Message, MsgKind },
    parse::{
        Expr,
        ExprKind,
        TypeKind
    }
};
use std::{
    rc::Rc
};

type TypeResult = Result<Rc<TypeKind>, Vec<Message>>;


fn get_array_indexing_type(analyser: &mut Analyser, array: &Expr, index: &Expr) -> TypeResult {
    /* Checks the type of an <array>[<index>] expression */

    let mut errors = vec![];

    let mut array_type = get_expr_type(analyser, array);
    let mut index_type = get_expr_type(analyser, index);

    if let Err(ref mut es) = array_type {
        errors.append(es);
    }
        
    if let Err(ref mut es) = index_type {
        errors.append(es);
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    if let TypeKind::Int = &*index_type.unwrap() {} else {
        errors.push(Message::new(MsgKind::TypeError)
                    .set_position(index.first_position)
                    .set_message("Arrays can only ben indexed by integers"));
    }

    let deriv = match &*array_type.unwrap() {
        TypeKind::List(deriv) => Rc::clone(deriv),
        _ => {
            errors.push(Message::new(MsgKind::TypeError)
                            .set_position(array.last_position)
                            .set_message("Cannot index a non-array type"));

            TypeKind::Int.rc() // placeholder
        }
    };

    if errors.is_empty() {
        Ok(deriv)
    } else {
        Err(errors)
    }
}


fn get_array_literal_type(analyser: &mut Analyser, elems: &Vec<Expr>) -> TypeResult {
    /* Checks the type of an array literal */
    
    if elems.len() == 0 {
        return Ok(TypeKind::EmptyList.rc());
    }

    let mut elem_types = vec![];
    let mut errors = vec![];

    for expr in elems.iter() {
        match get_expr_type(analyser, expr) {
            Ok(tp) => elem_types.push(tp),
            Err(ref mut es) => errors.append(es)
        }
    }

    if errors.len() > 0 {
        return Err(errors);
    }

    let first_type = &elem_types[0];

    for (i, tp) in elem_types[1..].iter().enumerate() {
        if tp != first_type {
            errors.push(Message::new(MsgKind::TypeError)
                                .set_position(elems[i].first_position)
                                .set_message(format!("Expected type {}, received {}", first_type, tp)));
        }
    }

    if errors.is_empty() {
        Ok(TypeKind::List(Rc::clone(first_type)).rc())
    } else {
        Err(errors)
    }
}


fn get_attr_res_type(analyser: &mut Analyser, parent: &Expr, attr_name: &String) -> TypeResult {
    /* Checks the type of an attribute resolution */

    match &*get_expr_type(analyser, parent)? {
        TypeKind::Module(mod_name) => {
            let module = analyser.modules.get(&mod_name).unwrap();

            // This will have to be changed in any future object-oriented
            // version of the language
            if let Some(func) = module.functions.get(attr_name) {
                let func_type: TypeKind = func.into();

                Ok(func_type.rc())
            } else {
                Message::new(MsgKind::NameError)
                    .set_position(parent.last_position)
                    .set_message(format!("Module '{}' has no attribute '{}'", mod_name, attr_name))
                    .into()
            }
        }

        _ => unimplemented!()
    }
}


fn get_compound_type(analyser: &mut Analyser, operator: &String, left: &Expr, right: &Expr) -> TypeResult {
    /* Checks the type of a compound expression */

    let mut errors = vec![];
                        
    let mut left_type = get_expr_type(analyser, left);
    let mut right_type = get_expr_type(analyser, right);

    if let Err(ref mut es) = left_type {
        errors.append(es);
    }
    
    if let Err(ref mut es) = right_type {
        errors.append(es);
    }

    if left_type.is_err() || right_type.is_err() {
        return Err(errors);
    }

    let res = match (&*left_type.unwrap(), &*right_type.unwrap()) {
        (TypeKind::Int, TypeKind::Int)     => TypeKind::Int.rc(),
        (TypeKind::Float, TypeKind::Int)   => TypeKind::Float.rc(),
        (TypeKind::Int, TypeKind::Float)   => TypeKind::Float.rc(),
        (TypeKind::Float, TypeKind::Float) => TypeKind::Float.rc(),
        _ => return Message::new(MsgKind::TypeError)
                    .set_position(left.first_position)
                    .set_message("Expected numeric type")
                    .into()
    };

    Ok(if vec!["==", "!=", "<", ">", "<=", ">="].contains(&&operator.to_string()[..]) {
        TypeKind::Bool.rc()
    } else {
        res
    })
}


fn get_funcall_type(analyser: &mut Analyser, function: &Expr, args: &Vec<Expr>) -> TypeResult {
    /* Returns the type of a function call expression */

    let function_type = get_expr_type(analyser, function)?;
    let mut errors = vec![];

    let (function_arg_types, function_return_type) = match &*function_type {
        TypeKind::Function { args, return_type } => (args, return_type),
        _ => return Message::new(MsgKind::TypeError)
                        .set_position(function.last_position)
                        .set_message(format!("Cannot call non-function value"))
                        .into()
    };

    for (i, arg) in args.iter().enumerate() {
        match get_expr_type(analyser, arg) {
            Ok(tp) => {
                if &*tp != &*function_arg_types[i] {
                    errors.push(Message::new(MsgKind::TypeError)
                                        .set_position(arg.first_position)
                                        .set_message(format!("Expected type {}, received {}", function_arg_types[i], tp)));
                }
            },

            Err(ref mut es) => errors.append(es)
        }
    }

    if errors.is_empty() {
        Ok(Rc::clone(function_return_type))
    } else {
        Err(errors)
    }
}


fn get_identifier_type(analyser: &mut Analyser, ident: &String) -> TypeResult {
    /* Checks the type of an identifier */

    if let Some(tp) = analyser.current_scope.get_value_type(ident) {
        Ok(tp)
    } else if let Some(m) = analyser.modules.get(ident) {
        Ok((*m).into())
    } else if let Some(f) = analyser.current_module.unwrap().functions.get(ident) {
        Ok(f.into())
    } else if let Some(t) = type_of_builtin_function(ident) {
        Ok(t)
    } else {
        Message::new(MsgKind::NameError)
            .set_position(analyser.current_position)
            .set_message(format!("No value or module named '{}' in scope", ident))
            .into()
    }
}


pub fn get_expr_type(analyser: &mut Analyser, expr: &Expr) -> TypeResult {
    /* Checks the type of an expression in a known context */

    analyser.current_position = expr.first_position;

    match &expr.expr_kind {
        ExprKind::ArrayIndexing { array, index } => {
            get_array_indexing_type(analyser, array, index)
        }

        ExprKind::ArrayLiteral { elems } => {
            get_array_literal_type(analyser, elems)
        }

        ExprKind::AttrRes { parent, attr_name } => {
            get_attr_res_type(analyser, parent, attr_name)
        }

        ExprKind::Compound { operator, left, right } => {
            get_compound_type(analyser, operator, left, right)
        }
    
        ExprKind::FunctionCall { function, args } => {
            get_funcall_type(analyser, function, args)
        }
    
        ExprKind::Identifier(ident) => get_identifier_type(analyser, ident),

        ExprKind::Float(_) => Ok(TypeKind::Float.rc()),

        ExprKind::Integer(_) => Ok(TypeKind::Int.rc()),

        ExprKind::String(_) => Ok(TypeKind::String.rc()),

        ExprKind::Unary { operand, .. } => get_expr_type(analyser, operand)
    }
}
