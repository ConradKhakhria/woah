use crate::{
    error::{
        collect_errors,
        Error,
        ErrorKind
    },
    parse::{
        Class,
        Expr,
        ExprType,
        TypeKind,
    },
    token::Token
};
use derive_getters::Getters;
use std::{
    collections::HashMap,
    rc::Rc
};

#[derive(Getters)]
pub struct TypeChecker<'s, 't> {
    classes: HashMap<String, Class<'s, 't>>,
    current_class: String,
    current_scope: Vec<(String, Rc<TypeKind<'s, 't>>)>
}

type TypeResult<'s, 't> = Result<Rc<TypeKind<'s, 't>>, Vec<Error>>;

impl<'s, 't> TypeChecker<'s, 't> {
    pub fn new(classes: HashMap<String, Class<'s, 't>>) -> Self {
        /* Creates a new type checker from a map of a program's classes */

        TypeChecker {
            classes,
            current_class: String::new(),
            current_scope: vec![]
        }
    }


    /* Expression Type checking */


    fn check_array_indexing_type(&self, array: &Expr<'s, 't>, index: &Expr<'s, 't>) -> TypeResult<'s, 't> {
        /* Checks the type of an <array>[<index>] expression */

        let mut errors = vec![];

        let mut array_type = self.check_expr_type(array);
        let mut index_type = self.check_expr_type(index);

        if let Err(ref mut es) = array_type {
            errors.append(es);
        }
    
        if let Err(ref mut es) = index_type {
            errors.append(es);
        }

        if let TypeKind::Int = &*index_type.unwrap() { } else {
            errors.push(Error::new(ErrorKind::TypeError)
                        .set_position(index.first_token.position())
                        .set_message("Arrays can only ben indexed by integers"));
        }

        let deriv = match &*array_type.unwrap() {
            TypeKind::List(deriv) => Rc::clone(deriv),
            _ => {
                errors.push(Error::new(ErrorKind::TypeError)
                                .set_position(array.last_token.position())
                                .set_message("Cannot index a non-array type"));

                Rc::new(TypeKind::Int) // placeholder
            }
        };

        if errors.is_empty() {
            Ok(deriv)
        } else {
            Err(errors)
        }
    }


    fn check_array_literal_type(&self, elems: &Vec<Expr<'s, 't>>) -> TypeResult<'s, 't> {
        /* Checks the type of an array literal */
    
        if elems.len() == 0 {
            return Ok(Rc::new(TypeKind::EmptyList));
        }

        let mut elem_types = vec![];
        let mut errors = vec![];

        for expr in elems.iter() {
            match self.check_expr_type(expr) {
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
                errors.push(Error::new(ErrorKind::TypeError)
                                    .set_position(elems[i].first_token.position())
                                    .set_message(format!("Expected type {}, received {}", first_type, tp)));
            }
        }

        if errors.is_empty() {
            Ok(Rc::new(TypeKind::List(Rc::clone(first_type))))
        } else {
            Err(errors)
        }
    }


    fn check_attr_res_type(&self, parent: &Expr<'s, 't>, attr_name: &Token<'s>) -> TypeResult<'s, 't> {
        /* Checks the type of an attribute resolution */

        let (class_name, object_method) = match &*self.check_expr_type(parent)? {
            TypeKind::Class(name) => (name.to_string(), false),

            TypeKind::HigherOrder { name, .. } => (name.to_string(), true),

            _ => unimplemented!()
        };

        let class = match self.classes.get(&class_name) {
            Some(c) => c,
            None => return Error::new(ErrorKind::NameError)
                                .set_position(attr_name.position())
                                .set_message(format!("Unknown class '{}'", class_name))
                                .into()
        };

        let (attr_result, attr_type_string) = if object_method {
            (class.object_attribute_type(attr_name, &self.current_class), "object")
        } else {
            (class.class_attribute_type(attr_name, &self.current_class), "class")
        };

        match attr_result {
            Some(tp) => Ok(tp),
            None => Error::new(ErrorKind::TypeError)
                        .set_position(attr_name.position())
                        .set_message(format!("class '{}' has no {} attribute '{}'", class_name, attr_type_string, attr_name))
                        .into()
        }
    }


    fn check_compound_type(&self, operator: &Token<'s>, left: &Expr<'s, 't>, right: &Expr<'s, 't>) -> TypeResult<'s, 't> {
        /* Checks the type of a compound expression */

        let mut errors = vec![];
                        
        let mut left_type = self.check_expr_type(left);
        let mut right_type = self.check_expr_type(right);

        if let Err(ref mut es) = left_type {
            errors.append(es);
        }
    
        if let Err(ref mut es) = right_type {
            errors.append(es);
        }

        if !errors.is_empty() {
            return Err(errors);
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


    fn check_funcall_type(&self, function: &Expr<'s, 't>, args: &Vec<Expr<'s, 't>>) -> TypeResult<'s, 't> {
        /* Returns the type of a function call expression */

        let function_type = self.check_expr_type(function)?;
        let mut errors = vec![];

        let (function_arg_types, function_return_type) = match &*function_type {
            TypeKind::Function { args, return_type } => (args, return_type),
            _ => return Error::new(ErrorKind::TypeError)
                            .set_position(function.last_token.position())
                            .set_message(format!("Cannot call non-function value"))
                            .into()
        };

        for (i, arg) in args.iter().enumerate() {
            match self.check_expr_type(arg) {
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
                    .set_position(function.first_token.position())
                    .set_message("Function does not return a value")
                    .into()
            }
        } else {
            Err(errors)
        }
    }


    fn check_identifier_type(&self, token: &Token<'s>) -> TypeResult<'s, 't> {
        /* Checks the type of an identifier */

        let ident_name = token.to_string();

        // checks for a variable with the same name
        for (val_name, val_type) in self.current_scope().iter() {
            if ident_name == *val_name {
                return Ok(Rc::clone(val_type));
            }
        }

        // checks for a class of the same name
        match self.classes().get(&ident_name) {
            Some(class) => Ok(Rc::new(TypeKind::Class(ident_name))),
            None => Error::new(ErrorKind::NameError)
                        .set_position(token.position())
                        .set_message(format!("No variable or class named '{}' in this scope", ident_name))
                        .into()
        }
    }


    pub fn check_expr_type(&self, expr: &Expr<'s, 't>) -> TypeResult<'s, 't> {
        /* Checks the type of an expression in a known context */

        match &expr.expr_type {
            ExprType::ArrayIndexing { array, index } => {
                self.check_array_indexing_type(&array, &index)
            }

            ExprType::ArrayLiteral { elems } => {
                self.check_array_literal_type(elems)
            }

            ExprType::AttrRes { parent, attr_name } => {
                self.check_attr_res_type(parent, attr_name)
            }

            ExprType::Compound { operator, left, right } => {
                self.check_compound_type(operator, left, right)
            }
    
            ExprType::FunctionCall { function, args } => {
                self.check_funcall_type(function, args)
            }
    
            ExprType::Identifier => {
                self.check_identifier_type(&expr.first_token)
            }

            ExprType::Float => Ok(Rc::new(TypeKind::Float)),

            ExprType::Integer => Ok(Rc::new(TypeKind::Int)),

            ExprType::String => Ok(Rc::new(TypeKind::String)),

            ExprType::Unary { operand, .. } => {
                self.check_expr_type(operand)
            }
        }
    }
}
