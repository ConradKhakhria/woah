use crate::{
    error::{
        Error,
        ErrorKind
    },
    parse::{
        Class,
        Expr,
        ExprType,
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
    current_scope: Vec<(String, bool, Rc<TypeKind<'s, 't>>)>
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


    fn get_scope_entry<T: ToString>(&self, name: &T) -> Option<(&String, bool, Rc<TypeKind<'s, 't>>)> {
        /* Gets the latest entry in the scope */

        let name_string = name.to_string();

        for (val_name, constant, val_type) in self.current_scope.iter().rev() {
            if *val_name == name_string {
                return Some((val_name, *constant, Rc::clone(val_type)));
            }
        }

        None
    }


    /* Expression Type checking */


    fn get_array_indexing_type(&self, array: &Expr<'s, 't>, index: &Expr<'s, 't>) -> TypeResult<'s, 't> {
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


    fn get_array_literal_type(&self, elems: &Vec<Expr<'s, 't>>) -> TypeResult<'s, 't> {
        /* Checks the type of an array literal */
    
        if elems.len() == 0 {
            return Ok(Rc::new(TypeKind::EmptyList));
        }

        let mut elem_types = vec![];
        let mut errors = vec![];

        for expr in elems.iter() {
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


    fn get_attr_res_type(&self, parent: &Expr<'s, 't>, attr_name: &Token<'s>) -> TypeResult<'s, 't> {
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

        match attr_result {
            Some(tp) => Ok(tp),
            None => Error::new(ErrorKind::TypeError)
                        .set_position(attr_name.position())
                        .set_message(format!("class '{}' has no {} attribute '{}'", class_name, attr_type_string, attr_name))
                        .into()
        }
    }


    fn get_compound_type(&self, operator: &Token<'s>, left: &Expr<'s, 't>, right: &Expr<'s, 't>) -> TypeResult<'s, 't> {
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


    fn get_funcall_type(&self, function: &Expr<'s, 't>, args: &Vec<Expr<'s, 't>>) -> TypeResult<'s, 't> {
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

        for (i, arg) in args.iter().enumerate() {
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

        // checks for a variable with the same name
        for (val_name, _, val_type) in self.current_scope().iter() {
            if ident_name == *val_name {
                return Ok(Rc::clone(val_type));
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


    pub fn get_expr_type(&self, expr: &Expr<'s, 't>) -> TypeResult<'s, 't> {
        /* Checks the type of an expression in a known context */

        match &expr.expr_type {
            ExprType::ArrayIndexing { array, index } => {
                self.get_array_indexing_type(&array, &index)
            }

            ExprType::ArrayLiteral { elems } => {
                self.get_array_literal_type(elems)
            }

            ExprType::AttrRes { parent, attr_name } => {
                self.get_attr_res_type(parent, attr_name)
            }

            ExprType::Compound { operator, left, right } => {
                self.get_compound_type(operator, left, right)
            }
    
            ExprType::FunctionCall { function, args } => {
                self.get_funcall_type(function, args)
            }
    
            ExprType::Identifier => {
                self.get_identifier_type(&expr.first_token)
            }

            ExprType::Float => Ok(Rc::new(TypeKind::Float)),

            ExprType::Integer => Ok(Rc::new(TypeKind::Int)),

            ExprType::String => Ok(Rc::new(TypeKind::String)),

            ExprType::Unary { operand, .. } => {
                self.get_expr_type(operand)
            }
        }
    }


    /* Statement type checking */


    fn check_statement_block_type(&mut self, statements: &mut Vec<Statement<'s, 't>>) -> Vec<Error> {
        /* Checks the types of a list of statements */

        let stack_top_name = self.current_scope.last().map(|(s, _, _)| s.clone());
        let mut errors = vec![];

        for statement in statements.iter_mut() {
            errors.append(&mut self.check_statement_type(statement));
        }

        match stack_top_name {
            Some(name) => {
                while let Some((curr_top_name, _, _)) = self.current_scope.last() {
                    if *curr_top_name == name {
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

        match value {
            _ => unimplemented!()
        }





        vec![]
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
