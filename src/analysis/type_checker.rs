use crate::error::*;
use crate::parse::*;
use std::rc::Rc;


struct StackFrameElement<'a> {
    value_name: &'a String,
    value_type: Rc<TypeKind>,
    constant: bool,
}


struct TypeChecker<'a> {
    current_function: &'a Function,
    current_module: &'a Module,
    current_scope: Vec<Vec<StackFrameElement<'a>>>
}


impl<'a> TypeChecker<'a> {
    fn new(current_module: &'a Module, current_function: &'a Function) -> Self {
        /* Creates a new TypeChecker for a function */

        TypeChecker {
            current_function,
            current_module, 
            current_scope: vec![]
        }
    }


    /* Misc */

    fn get_from_scope(&self, name: &str) -> Option<&StackFrameElement<'a>> {
        /* Attempts to get a value from the scope */

        for scope in self.current_scope.iter() {
            for value in scope.iter() {
                if name == value.value_name {
                    return Some(value)
                }
            }
        }

        None
    }


    fn scope_contains(&self, name: &str) -> bool {
        /* Says whether the scope contains this value */

        self.get_from_scope(name).is_some()
    }


    /* Statement type-checking */

    fn get_statement_block_type(&mut self, statements: &'a [Statement]) -> Result<Rc<TypeKind>, Vec<Error>> {
        /* Gets the type of a statement */

        let mut final_statement_type = None;
        let mut errors = vec![];
        
        for statement in statements.iter() {
            let mut statement_type = match &statement.stmt_type {
                StatementType::Assign { assigned_to, new_value } => {
                    self.get_assignment_type(assigned_to, new_value)
                },

                StatementType::Declare { .. } => {
                    self.get_declaration_type(&statement)
                }

                _ => Error::new(ErrorKind::UnimplementedError)
                        .set_position(statement.first_position())
                        .set_message("these statements cannot be type-checked yet")
                        .into()
            };

            match statement_type {
                Ok(tp) => final_statement_type = Some(tp.clone()),
                Err(ref mut es) => errors.append(es)
            }
        }

        if errors.is_empty() {
            Err(errors)
        } else {
            Ok(final_statement_type.unwrap().clone())
        }
    }


    fn get_assignment_type(&mut self, assigned_to: &Expr, new_value: &Expr) -> Result<Rc<TypeKind>, Vec<Error>> {
        /* Gets the type of an assignment expression */

        let mut assigned_to_type = self.get_expression_type(assigned_to);
        let mut new_value_type = self.get_expression_type(new_value);
        let mut errors = vec![];

        if let Err(ref mut es) = assigned_to_type {
            errors.append(es);
        }

        if let Err(ref mut es) = new_value_type {
            errors.append(es);
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        let assigned_to_type = assigned_to_type.unwrap();
        let new_value_type = new_value_type.unwrap();

        if assigned_to_type != new_value_type {
            errors.push(
                Error::new(ErrorKind::TypeError)
                    .set_position(assigned_to.first_position.clone())
                    .set_message(format!(
                        "assigning value of type {} to a value of type {}",
                        new_value_type,
                        assigned_to_type
                    ))
            );
        }

        Ok(assigned_to_type.clone())
    }


    fn get_declaration_type(&mut self, statement: &'a Statement) -> Result<Rc<TypeKind>, Vec<Error>> {
        /* Gets the type of a declaration */

        // Too many arguments otherwise lol
        let value_name;
        let value_type;
        let value;
        let constant;

        match &statement.stmt_type {
            StatementType::Declare { value_name: vn, value_type: vt, 
                                     value: v, constant: c } => {
                value_name = vn;
                value_type = vt;
                value = v;
                constant = c;
            },

            _ => unreachable!()
        }

        let determined_value_type = self.get_expression_type(value)?;

        if let Some(value_type) = value_type {
            if *value_type == determined_value_type {
                self.current_scope.last_mut().unwrap().push(
                    StackFrameElement {
                        value_name,
                        value_type: TypeKind::ReportedError.rc(),
                        constant: *constant
                    }
                );

                return Error::new(ErrorKind::TypeError)
                            .set_position(statement.first_position())
                            .set_message(format!(
                                "value '{}' expected type {} but received {}",
                                value_name,
                                value_type,
                                determined_value_type
                            ))
                            .into();
            }
        }

        self.current_scope.last_mut().unwrap().push(
            StackFrameElement {
                value_name,
                value_type: determined_value_type.clone(),
                constant: *constant
            }
        );

        Ok(determined_value_type.clone())
    }


    /* Expression type-checking */

    fn get_expression_type(&mut self, expression: &Expr) -> Result<Rc<TypeKind>, Vec<Error>> {
        /* Gets the type of an expression */

        Err(vec![])
    }
}


pub fn check_function_types<'a>(current_module: &'a Module, current_function: &'a Function) -> Result<(), Vec<Error>> {
    /* Checks the types of a function */

    let mut type_checker = TypeChecker::new(current_module, current_function);
    


    Ok(())
}
