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
    current_scope: Vec<Vec<StackFrameElement<'a>>>,
    final_statement_stack: Vec<bool>
}


impl<'a> TypeChecker<'a> {
    fn new(current_module: &'a Module, current_function: &'a Function) -> Self {
        /* Creates a new TypeChecker for a function */

        TypeChecker {
            current_function,
            current_module, 
            current_scope: vec![vec![]],
            final_statement_stack: vec![]
        }
    }


    /* Misc */

    fn add_to_scope(&mut self, value_name: &'a String, value_type: Rc<TypeKind>, constant: bool) {
        /* Adds a new value to the scope */

        let new_stack_frame_element = StackFrameElement {
            value_name,
            value_type,
            constant
        };

        self.current_scope.last_mut().unwrap().push(new_stack_frame_element);
    }


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


    fn is_final_statement(&self) -> bool {
        /* Determines whether we are currently in the final statement */

        for res in self.final_statement_stack.iter() {
            if !*res {
                return false;
            }
        }

        true
    }


    fn scope_contains(&self, name: &str) -> bool {
        /* Says whether the scope contains this value */

        self.get_from_scope(name).is_some()
    }


    /* Statement type-checking */

    fn get_statement_block_type(&mut self, statements: &'a [Statement]) -> Result<Rc<TypeKind>, Vec<Error>> {
        /* Gets the type of a statement */

        if statements.len() == 0 {
            unreachable!();
        }

        let mut final_statement_type = None;
        let mut errors = vec![];
        
        for (index, statement) in statements.iter().enumerate() {
            self.final_statement_stack.push(index + 1 == statements.len());

            let mut statement_type = match &statement.stmt_type {
                StatementType::Assign { assigned_to, new_value } => {
                    self.get_assignment_type(assigned_to, new_value)
                }

                StatementType::ConditionalBlock { .. } => {
                    self.get_conditional_type(&statement, )
                }

                StatementType::Declare { .. } => {
                    self.get_declaration_type(&statement)
                }

                StatementType::IteratorForLoop { .. } => {
                    self.get_ifl_type(&statement)
                }

                StatementType::NumericRangeForLoop { .. } => {
                    self.get_nrfl_type(&statement)
                }

                StatementType::RawExpr { expr } => {
                    self.get_expression_type(expr)
                }

                StatementType::Return { value } => {
                    match value {
                        Some(expr) => self.get_expression_type(expr),
                        None => Ok(TypeKind::NoneType.rc())
                    }
                }

                StatementType::WhileLoop { condition, block } => {
                    self.get_while_loop_type(condition, block)
                }

                _ => Error::new(ErrorKind::UnimplementedError)
                        .set_position(statement.first_position())
                        .set_message("these statements cannot be type-checked yet")
                        .into()
            };

            self.final_statement_stack.pop();

            match statement_type {
                Ok(tp) => final_statement_type = Some(tp.clone()),
                Err(ref mut es) => errors.append(es)
            }
        }

        if errors.is_empty() {
            Ok(final_statement_type.unwrap_or(TypeKind::ReportedError.rc()).clone())
        } else {
            Err(errors)
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


    fn get_conditional_type(&mut self, statement: &'a Statement) -> Result<Rc<TypeKind>, Vec<Error>> {
       /* Gets the type of a conditional statement
        *
        * args:
        * - statement: the conditional block statement
        * - final_stmt: whether this statement is the last in the function
        *
        * note:
        *   if this statement is the final statement, this function ensures that.
        *   all branches have the same type
        */

        let conditional_blocks;
        let else_block;

        match &statement.stmt_type {
            StatementType::ConditionalBlock { conditional_blocks: c,
                                              else_block: e } => {
                conditional_blocks = c;
                else_block = e;
            },

            _ => unreachable!()
        };

        let mut cond_branch_types = vec![];
        let mut errors = vec![];

        // gets the types of all conditional blocks
        for (condition, block) in conditional_blocks {
            match self.get_expression_type(condition) {
                Ok(tp) => {
                    if let TypeKind::Bool = &*tp {} else {
                        errors.push(
                            Error::new(ErrorKind::TypeError)
                                .set_position(condition.first_position.clone())
                                .set_message(format!("expected boolean condition in conditioanl, got {}", tp))
                        );
                    }
                },

                Err(ref mut es) => errors.append(es)
            }

            match self.get_statement_block_type(block) {
                Ok(tp) => cond_branch_types.push(tp.clone()),
                Err(ref mut es) => errors.append(es)
            }
        }

        if let Some(block) = else_block {
            match self.get_statement_block_type(&block) {
                Ok(tp) => cond_branch_types.push(tp),
                Err(ref mut es) => errors.append(es)
            }
        }

        // We need to check that all branches have the same time in case of
        // this being the last statement
        if self.is_final_statement() {
            for i in 1..cond_branch_types.len() {
                if cond_branch_types[i] != cond_branch_types[0] {
                    let position = if i + 1 == cond_branch_types.len() {
                        else_block.as_ref().unwrap().last().unwrap().last_position()
                    } else {
                        conditional_blocks[i].1.last().unwrap().last_position()
                    };

                    errors.push(
                        Error::new(ErrorKind::TypeError)
                            .set_position(position)
                            .set_message("this branch doesn't have the same return type as the first branch")
                    );
                }
            }
        }

        if errors.is_empty() {
            if self.is_final_statement() {
                Ok(cond_branch_types[0].clone())
            } else {
                Ok(TypeKind::NoneType.rc())
            }
        } else {
            Err(errors)
        }
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


    fn get_ifl_type(&mut self, statement: &'a Statement) -> Result<Rc<TypeKind>, Vec<Error>> {
        /* Gets the type of an iterator for loop */

        let mut errors = vec![];
        let iterator_name;
        let range;
        let block;

        match &statement.stmt_type {
            StatementType::IteratorForLoop { iterator_name: i_n, range: r, block: b } => {
                iterator_name = i_n;
                range = r;
                block = b;
            },

            _ => unreachable!()
        }

        match self.get_expression_type(range) {
            Ok(tp) => {
                let iterator_type = match &*tp {
                    TypeKind::List(deriv) => deriv.clone(),
                    _ => {
                        errors.push(
                            Error::new(ErrorKind::TypeError)
                                .set_position(range.first_position.clone())
                                .set_message("cannot iterate over a non-list type")
                        );

                        TypeKind::ReportedError.rc()
                    }
                };

                self.add_to_scope(iterator_name, iterator_type, true);
            }

            Err(ref mut es) => errors.append(es)
        }

        if let Err(ref mut es) = self.get_statement_block_type(block) {
            errors.append(es);
        }

        if errors.is_empty() {
            Ok(TypeKind::NoneType.rc())
        } else {
            Err(errors)
        }
    }


    fn get_nrfl_type(&mut self, statement: &'a Statement) -> Result<Rc<TypeKind>, Vec<Error>> {
        /* Gets the type of a numeric-range for loop */

        let iterator_name;
        let start;
        let end;
        let step;
        let block;

        match &statement.stmt_type {
            StatementType::NumericRangeForLoop { iterator_name: i, start: v1,
                                                 end: v2, step: v3, block: b } => {
                iterator_name = i;
                start = v1;
                end = v2;
                step = v3;
                block = b;
            },

            _ => unreachable!()
        }

        let mut errors = vec![];
        let number_type_error = Error::new(ErrorKind::TypeError)
                                    .set_message("expected numeric value in for loop range");

        match self.get_expression_type(start) {
            Ok(tp) => {
                match &*tp {
                    TypeKind::Float|TypeKind::Int => {},

                    _ => {
                        errors.push(number_type_error.clone().set_position(start.first_position.clone()))
                    }
                }

                self.add_to_scope(iterator_name, tp.clone(), true);
            },

            Err(ref mut es) => errors.append(es)
        }

        for number in [ end, step ] {
            match self.get_expression_type(number) {
                Ok(tp) => {
                    match &*tp {
                        TypeKind::Float|TypeKind::Int => {},
    
                        _ => errors.push(number_type_error.clone().set_position(start.first_position.clone()))
                    }
                },
    
                Err(ref mut es) => errors.append(es)
            }
        }

        if let Err(ref mut es) = self.get_statement_block_type(block) {
            errors.append(es);
        }

        if errors.is_empty() {
            Ok(TypeKind::NoneType.rc())
        } else {
            Err(errors)
        }
    }


    fn get_while_loop_type(&mut self, condition: &Expr, block: &'a Vec<Statement>) -> Result<Rc<TypeKind>, Vec<Error>> {
        /* Gets the type of a while loop */

        let mut errors = vec![];

        match self.get_expression_type(condition) {
            Ok(tp) => {
                if let TypeKind::Bool = &*tp {} else {
                    errors.push(
                        Error::new(ErrorKind::TypeError)
                            .set_position(condition.first_position.clone())
                            .set_message("expected boolean condition in while loop")
                    );
                }
            }

            Err(ref mut es) => errors.append(es)
        }

        if let Err(ref mut es) = self.get_statement_block_type(block) {
            errors.append(es);
        }

        if errors.is_empty() {
            Ok(TypeKind::NoneType.rc())
        } else {
            Err(errors)
        }
    }

    /* Expression type-checking */

    fn get_expression_type(&mut self, _expression: &Expr) -> Result<Rc<TypeKind>, Vec<Error>> {
        /* Gets the type of an expression */

        Err(vec![])
    }
}


pub fn check_function_types<'a>(current_module: &'a Module, current_function: &'a Function) -> Result<(), Vec<Error>> {
    /* Checks the types of a function */

    let mut type_checker = TypeChecker::new(current_module, current_function);
    
    for arg in current_function.args.iter() {
        type_checker.add_to_scope(
            &arg.arg_name,
            arg.arg_type.clone(), 
            arg.arg_mutable
        );
    }

    let ret_type = type_checker.get_statement_block_type(&current_function.body)?;

    if ret_type != current_function.return_type.clone() {
        return Error::new(ErrorKind::TypeError)
                    .set_position(current_function.first_position())
                    .set_message(format!(
                        "function '{}' expects to return {} but in fact returns {}",
                        &current_function.name,
                        &current_function.return_type,
                        ret_type
                    ))
                    .into();
    }

    Ok(())
}
