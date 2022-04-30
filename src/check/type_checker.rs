use crate::{
    check::ScopedValue,
    error::{
        Error,
        ErrorKind
    },
    parse::{
        Class,
        Expr,
        ExprKind,
        Statement,
        StatementType,
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
    current_scope: Vec<ScopedValue<'s, 't>>
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


    /* Scope functions */


    fn get_scope_entry<T: ToString>(&self, name: &T) -> Option<&ScopedValue<'s, 't>> {
        /* Gets the latest entry in the scope of a given name */

        let name_string = name.to_string();

        for scope_entry in self.current_scope.iter().rev() {
            if scope_entry.var_name == name_string {
                return Some(scope_entry);
            }
        }

        None
    }


    /* Expression Type checking */


    fn get_array_indexing_type(&self, array: &mut Expr<'s, 't>, index: &mut Expr<'s, 't>) -> TypeResult<'s, 't> {
        /* Checks the type of an <array>[<index>] expression */

        let mut errors = vec![];

        let mut array_type = self.get_expr_type(array);
        let mut index_type = self.get_expr_type(index);

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


    fn get_array_literal_type(&self, elems: &mut Vec<Expr<'s, 't>>) -> TypeResult<'s, 't> {
        /* Checks the type of an array literal */
    
        if elems.len() == 0 {
            return Ok(Rc::new(TypeKind::EmptyList));
        }

        let mut elem_types = vec![];
        let mut errors = vec![];

        for expr in elems.iter_mut() {
            match self.get_expr_type(expr) {
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


    fn get_attr_res_type(&self, parent: &mut Expr<'s, 't>, attr_name: &Token<'s>) -> TypeResult<'s, 't> {
        /* Checks the type of an attribute resolution */

        let (class_name, object_method) = match &*self.get_expr_type(parent)? {
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

        let error_message = format!(
            "class '{}' has no {}{} attribute '{}'",
            class_name,
            if class_name == self.current_class {
                ""
            } else {
                "public "
            },
            attr_type_string,
            attr_name
        );

        match attr_result {
            Some(tp) => Ok(tp),
            None => Error::new(ErrorKind::TypeError)
                        .set_position(attr_name.position())
                        .set_message(error_message)
                        .into()
        }
    }


    fn get_compound_type(&self, operator: &Token<'s>, left: &mut Expr<'s, 't>, right: &mut Expr<'s, 't>) -> TypeResult<'s, 't> {
        /* Checks the type of a compound expression */

        let mut errors = vec![];
                        
        let mut left_type = self.get_expr_type(left);
        let mut right_type = self.get_expr_type(right);

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


    fn get_funcall_type(&self, function: &mut Expr<'s, 't>, args: &mut Vec<Expr<'s, 't>>) -> TypeResult<'s, 't> {
        /* Returns the type of a function call expression */

        let function_type = self.get_expr_type(function)?;
        let mut errors = vec![];

        let (function_arg_types, function_return_type) = match &*function_type {
            TypeKind::Function { args, return_type } => (args, return_type),
            _ => return Error::new(ErrorKind::TypeError)
                            .set_position(function.last_token.position())
                            .set_message(format!("Cannot call non-function value"))
                            .into()
        };

        for (i, arg) in args.iter_mut().enumerate() {
            match self.get_expr_type(arg) {
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


    fn get_identifier_type(&self, token: &Token<'s>) -> TypeResult<'s, 't> {
        /* Checks the type of an identifier */

        let ident_name = token.to_string();

        // checks for a value of the same name
        if let Some(scoped_value) = self.get_scope_entry(&ident_name) {
            if scoped_value.var_name == ident_name {
                return match &scoped_value.var_type {
                    Some(tp) => Ok(Rc::clone(tp)),
                    None => Err(vec![]) // poisoned
                }
            }
        }

        // checks for a class of the same name
        match self.classes().get(&ident_name) {
            Some(_) => Ok(Rc::new(TypeKind::Class(ident_name))),
            None => Error::new(ErrorKind::NameError)
                        .set_position(token.position())
                        .set_message(format!("No variable or class named '{}' in this scope", ident_name))
                        .into()
        }
    }


    pub fn get_expr_type(&self, expr: &mut Expr<'s, 't>) -> TypeResult<'s, 't> {
        /* Checks the type of an expression in a known context */

        let expr_type = match &mut expr.expr_kind {
            ExprKind::ArrayIndexing { array, index } => {
                self.get_array_indexing_type(array, index)
            }

            ExprKind::ArrayLiteral { elems } => {
                self.get_array_literal_type(elems)
            }

            ExprKind::AttrRes { parent, attr_name } => {
                self.get_attr_res_type(parent, attr_name)
            }

            ExprKind::Compound { operator, left, right } => {
                self.get_compound_type(operator, left, right)
            }
    
            ExprKind::FunctionCall { function, args } => {
                self.get_funcall_type(function, args)
            }
    
            ExprKind::Identifier => {
                self.get_identifier_type(&expr.first_token)
            }

            ExprKind::Float => Ok(Rc::new(TypeKind::Float)),

            ExprKind::Integer => Ok(Rc::new(TypeKind::Int)),

            ExprKind::String => Ok(Rc::new(TypeKind::String)),

            ExprKind::Unary { operand, .. } => {
                self.get_expr_type(operand)
            }
        };

        match expr_type {
            Ok(tp) => {
                expr.expr_type = Some(Rc::clone(&tp));
                Ok(tp)
            }

            err => err
        }
    }


    /* Statement type checking */


    pub fn check_statement_block_type(&mut self, statements: &mut Vec<Statement<'s, 't>>) -> Vec<Error> {
        /* Checks the types of a list of statements */

        let stack_top_name = self.current_scope.last().map(|v| v.var_name.clone());
        let mut errors = vec![];

        for statement in statements.iter_mut() {
            errors.append(&mut self.check_statement_type(statement));
        }

        match stack_top_name {
            Some(name) => {
                while let Some(value) = self.current_scope.last() {
                    if value.var_name == name {
                        break;
                    } else {
                        self.current_scope.pop();
                    }
                }
            }

            None => self.current_scope = vec![]
        }

        errors
    }


    fn check_assign_type(&self, assigned_to: &mut Expr<'s, 't>, new_value: &mut Expr<'s, 't>) -> Vec<Error> {
        /* Checks the types of an assignment */

        let mut assigned_to_type_res = self.get_expr_type(assigned_to);
        let mut new_value_type_res = self.get_expr_type(new_value);
        let mut errors = vec![];

        if let Err(ref mut es) = assigned_to_type_res {
            errors.append(es);
        }

        if let Err(ref mut es) = new_value_type_res {
            errors.append(es);
        }

        let (assigned_to_type, new_value_type) = if errors.is_empty() {
            (assigned_to_type_res.unwrap(), new_value_type_res.unwrap())
        } else {
            return errors
        };
        
        if assigned_to_type != new_value_type {
            Error::new(ErrorKind::TypeError)
                .set_position(assigned_to.last_token.position())
                .set_message(format!("expected type {}, received {}", assigned_to_type, new_value_type))
                .into()
        } else {
            vec![]
        }
    }


    fn check_conditional_type(&mut self, condition: &mut Expr<'s, 't>, block: &mut Vec<Statement<'s, 't>>) -> Vec<Error> {
        /* Checks the types of a conditional expression */

        let mut errors = vec![];

        match self.get_expr_type(condition) {
            Ok(cond_type) => {
                if &*cond_type != &TypeKind::Bool {
                    errors.push(Error::new(ErrorKind::TypeError)
                                    .set_position(condition.first_token.position())
                                    .set_message(format!("Expected boolean expression, received {}", cond_type)))
                }
            },

            Err(ref mut es) => errors.append(es)
        }

        errors.append(&mut self.check_statement_block_type(block));

        errors
    }


    fn check_declaration_type(&mut self, declaration: &mut StatementType<'s, 't>) -> Vec<Error> {
        /* Checks the type of a declaration */

        let mut errors = vec![];

        let (value_name, value_type, value, constant) = match declaration {
            StatementType::Declare { value_name, value_type, value, constant } => {
                (value_name, value_type, value, constant)
            },

            _ => unreachable!()
        };

        if let Some(_) = self.get_scope_entry(value_name) {
            return Error::new(ErrorKind::TypeError)
                        .set_position(value_name.position())
                        .set_message(format!("value '{}' has already been defined", value_name))
                        .into();
        }

        let var_type = match value {
            Some(expr) => match self.get_expr_type(expr) {
                Ok(expr_type) => {
                    if let Some(given_type) = value_type {
                        if **given_type != *expr_type {
                            errors.push(Error::new(ErrorKind::TypeError)
                                            .set_position(expr.first_token.position())
                                            .set_message(format!("Expected type {}, received {}", given_type, expr_type)));
                        }
                    }
        
                    if errors.is_empty() {
                        Some(Rc::clone(&expr_type))
                    } else {
                        None
                    }
                },

                Err(ref mut es) => {
                    errors.append(es);

                    None
                }
            }

            None => match value_type {
                Some(tp) => Some(Rc::clone(tp)),
                None => None
            }
        };

        self.current_scope.push(
            ScopedValue {
                var_name: value_name.to_string(),
                constant: *constant,
                var_type
            }
        );

        errors
    }


    pub fn check_statement_type(&mut self, statement: &mut Statement<'s, 't>) -> Vec<Error> {
       /* Checks whether a statement obeys the type system
        *
        * returns: a list of any type errors that were found
        * -------
        */
    
        match &mut statement.stmt_type {
            StatementType::Assign { assigned_to, new_value } => {
                self.check_assign_type(assigned_to, new_value)
            }

            StatementType::Conditional { condition, block, .. } => {
                self.check_conditional_type(condition, block)
            }

            tp @ StatementType::Declare {..} => {
                self.check_declaration_type(tp)
            }

            _ => unimplemented!()
        }
    }
}
