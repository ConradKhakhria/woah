use crate::error::*;
use crate::parse::*;
use std::collections::HashMap;
use std::rc::Rc;


struct StackFrameElement<'a> {
    value_name: &'a String,
    value_type: Rc<TypeKind>,
    constant: bool,
}


struct TypeChecker<'a> {
    modules: &'a HashMap<String, Module>,
    current_function: &'a Function,
    current_module: &'a Module,
    current_scope: Vec<Vec<StackFrameElement<'a>>>,
    final_statement_stack: Vec<bool>
}


impl<'a> TypeChecker<'a> {
    fn check_function_types(&mut self) -> Result<(), Vec<Error>> {
        /* Checks the types of a function */

        for arg in self.current_function.args.iter() {
            self.add_to_scope(
                &arg.arg_name,
                arg.arg_type.clone(), 
                arg.arg_mutable
            );
        }
    
        let ret_type = self.get_statement_block_type(&self.current_function.body)?;
    
        if ret_type != self.current_function.return_type.clone() {
            return Error::new(ErrorKind::TypeError)
                        .set_position(self.current_function.first_position())
                        .set_message(format!(
                            "function '{}' expects to return {} but in fact returns {}",
                            &self.current_function.name,
                            &self.current_function.return_type,
                            ret_type
                        ))
                        .into();
        }
    
        Ok(()) 
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


    fn drop_scope(&mut self) {
        /* Attempts to remove a scope from the local namespace */

        self.current_scope.pop().unwrap(); // panics if None
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


    fn new_scope(&mut self) {
        /* Creates a new scope */

        self.current_scope.push(vec![]);
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

            self.new_scope();

            match self.get_statement_block_type(block) {
                Ok(tp) => cond_branch_types.push(tp.clone()),
                Err(ref mut es) => errors.append(es)
            }

            self.drop_scope();
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
            StatementType::IteratorForLoop { iterator_name: i_n, range: r,
                                             block: b } => {
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

                self.new_scope();
                self.add_to_scope(iterator_name, iterator_type, true);
            }

            Err(ref mut es) => errors.append(es)
        }

        if let Err(ref mut es) = self.get_statement_block_type(block) {
            errors.append(es);
        }

        self.drop_scope();

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

                self.new_scope();
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

        self.drop_scope();

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

        self.new_scope();

        if let Err(ref mut es) = self.get_statement_block_type(block) {
            errors.append(es);
        }

        self.drop_scope();

        if errors.is_empty() {
            Ok(TypeKind::NoneType.rc())
        } else {
            Err(errors)
        }
    }


    /* Expression type-checking */

    fn get_expression_type(&self, expression: &Expr) -> Result<Rc<TypeKind>, Vec<Error>> {
        /* Gets the type of an expression */

        match &expression.expr_kind {
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
                self.get_compound_type(operator, [left, right])
            }

            ExprKind::Float(_) => {
                Ok(TypeKind::Float.rc())
            }

            ExprKind::FunctionCall { function, args } => {
                self.get_funcall_type(function, args)
            }

            ExprKind::Identifier(name) => {
                self.get_ident_type(name, expression.first_position.clone())
            }

            _ => Error::new(ErrorKind::UnimplementedError)
                    .set_position(expression.first_position.clone())
                    .set_message("these expressions cannot be type-checked yet")
                    .into()
        }
    }


    fn get_array_indexing_type(&self, array: &Box<Expr>, index: &Box<Expr>) -> Result<Rc<TypeKind>, Vec<Error>> {
        /* Gets the type of an array indexing */

        let mut errors = vec![];

        let array_elem_type = match self.get_expression_type(&array) {
            Ok(tp) => {
                match &*tp {
                    TypeKind::List(deriv) => deriv.clone(),
                    _ => {
                        errors.push(
                            Error::new(ErrorKind::TypeError)
                                .set_position(array.first_position.clone())
                                .set_message("cannot index a non-array type")
                        );
                        TypeKind::ReportedError.rc()
                    }
                }
            }

            Err(ref mut es) => {
                errors.append(es);
                TypeKind::ReportedError.rc()
            }
        };

        match self.get_expression_type(&index) {
            Ok(tp) => {
                if let TypeKind::Int = &*tp {} else {
                    errors.push(
                        Error::new(ErrorKind::TypeError)
                            .set_position(index.first_position.clone())
                            .set_message("you can only index an array with an integer")
                    );
                }
            }

            Err(ref mut es) => errors.append(es)
        }

        if errors.is_empty() {
            Ok(array_elem_type)
        } else {
            Err(errors)
        }
    }


    fn get_array_literal_type(&self, elems: &Vec<Expr>) -> Result<Rc<TypeKind>, Vec<Error>> {
        /* Gets the type of an array literal */

        if elems.is_empty() {
            return Ok(TypeKind::EmptyList.rc());
        }

        let mut errors = vec![];

        let first_elem_type = self.get_expression_type(&elems[0])?;

        for elem in elems[1..].iter() {
            match self.get_expression_type(elem) {
                Ok(tp) => {
                    if first_elem_type != tp {
                        errors.push(
                            Error::new(ErrorKind::TypeError)
                                .set_position(elem.first_position.clone())
                                .set_message(format!("expected expression of type {}, found {}", first_elem_type, tp))
                        );
                    }
                }

                Err(ref mut es) => errors.append(es)
            }
        }

        if errors.is_empty() {
            Ok(first_elem_type)
        } else {
            Err(errors)
        }
    }


    fn get_attr_res_type(&self, parent: &Box<Expr>, attr_name: &String) -> Result<Rc<TypeKind>, Vec<Error>> {
        /* Gets the type of an attribute resolution */

        match &*self.get_expression_type(parent)? {
            // module-scoped functions etc
            TypeKind::ClassName(class_name) => {
                let class = self.modules.get(class_name).unwrap();

                match class.module_methods().get(attr_name) {
                    Some(func) => {
                        let type_kind: TypeKind = func.into();

                        Ok(type_kind.rc())
                    }

                    None => {
                        Error::new(ErrorKind::NameError)
                            .set_position(parent.last_position.clone())
                            .set_message(format!("module '{}' has no attribute '{}'", class_name, attr_name))
                            .into()
                    }
                }
            }

            // object methods and attributes
            TypeKind::HigherOrder { name, .. } => {
                Error::new(ErrorKind::UnimplementedError)
                    .set_position(parent.last_position.clone())
                    .set_message("objects have not been implemented yet")
                    .into()
            }

            t => {
                Error::new(ErrorKind::UnimplementedError)
                    .set_position(parent.last_position.clone())
                    .set_message(format!("attributes for type {} aren't implemented yet", t))
                    .into()
            }
        }
    }


    fn get_compound_type(&self, op: &String, operands: [&Box<Expr>; 2]) -> Result<Rc<TypeKind>, Vec<Error>> {
        /* Gets the type of a compound expression */
    
        let [left, right] = operands;
        let mut left_type = self.get_expression_type(left);
        let mut right_type = self.get_expression_type(right);
        
        let mut errors = vec![];

        for result in [&mut left_type, &mut right_type] {
            if let Err(ref mut es) = result {
                errors.append(es);
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        match op.as_str() {
            "+"|"-"|"*"|"/" => {
                match (&*left_type.unwrap(), &*right_type.unwrap()) {
                    (TypeKind::Float, TypeKind::Float) => Ok(TypeKind::Float.rc()),
                    (TypeKind::Int, TypeKind::Int) => Ok(TypeKind::Int.rc()),
                    (l, r) => {
                        Error::new(ErrorKind::TypeError)
                            .set_position(left.first_position.clone())
                            .set_message(format!(
                                "cannot perform '{}' between values of type {} and {}",
                                op,
                                l,
                                r
                            ))
                            .into()
                    }
                }
            }

            "%" => {
                match (&*left_type.unwrap(), &*right_type.unwrap()) {
                    (TypeKind::Int, TypeKind::Int) => Ok(TypeKind::Int.rc()),
                    _ => Error::new(ErrorKind::TypeError)
                            .set_position(left.first_position.clone())
                            .set_message("modulo ('%') expressions must take 2 integers")
                            .into()
                }
            }

            _ => { // comparison operators
                match (&*left_type.unwrap(), &*right_type.unwrap()) {
                    (TypeKind::Float, TypeKind::Float) => Ok(TypeKind::Float.rc()),
                    (TypeKind::Int, TypeKind::Int) => Ok(TypeKind::Int.rc()),
                    (l, r) => {
                        Error::new(ErrorKind::TypeError)
                            .set_position(left.first_position.clone())
                            .set_message(format!(
                                "cannot perform '{}' between values of type {} and {}",
                                op,
                                l,
                                r
                            ))
                            .into()
                    }
                }
            }
        }
    }


    fn get_funcall_type(&self, function: &Box<Expr>, args: &Vec<Expr>) -> Result<Rc<TypeKind>, Vec<Error>> {
        /* Gets the type of a function-call expression */

        let mut errors = vec![];
        let mut supplied_arg_types = vec![];

        for arg in args.iter() {
            match self.get_expression_type(arg) {
                Ok(tp) => supplied_arg_types.push(tp.clone()),

                Err(ref mut es) => {
                    errors.append(es);
                    supplied_arg_types.push(TypeKind::ReportedError.rc());
                }
            }
        }

        let return_type = match self.get_expression_type(function) {
            Ok(tp) => {
                match &*tp {
                    TypeKind::Function { args: defined_arg_types, return_type } => {
                        if defined_arg_types.len() != supplied_arg_types.len() {
                            errors.push(
                                Error::new(ErrorKind::TypeError)
                                    .set_position(function.first_position.clone())
                                    .set_message(format!(
                                        "function takes {} but received {}",
                                        defined_arg_types.len(),
                                        supplied_arg_types.len()
                                    ))
                            );
                        } else {
                            for i in 0..defined_arg_types.len() {
                                if defined_arg_types[i] != supplied_arg_types[i] {
                                    errors.push(
                                        Error::new(ErrorKind::TypeError)
                                            .set_position(args[i].first_position.clone())
                                            .set_message(format!(
                                                "expected value of type {}, received {}",
                                                &defined_arg_types[i],
                                                &supplied_arg_types[i]
                                            ))
                                    );
                                }
                            }
                        }

                        return_type.clone()
                    }

                    _ => {
                        errors.push(
                            Error::new(ErrorKind::TypeError)
                                .set_position(function.first_position.clone())
                                .set_message(format!("expected function, received {}", tp))
                        );

                        TypeKind::ReportedError.rc()
                    }
                }
            }

            Err(ref mut es) => {
                errors.append(es);
                TypeKind::ReportedError.rc()
            }
        };

        if errors.is_empty() {
            Ok(return_type)
        } else {
            Err(errors)
        }
    }


    fn get_ident_type(&self, ident: &String, pos: (usize, usize)) -> Result<Rc<TypeKind>, Vec<Error>> {
        /* Gets the type of an identifier */

        if let Some(elem) = self.get_from_scope(&ident) {
            Ok(elem.value_type.clone())
        } else if let Some(module) = self.modules.get(ident) {
            Ok(TypeKind::ClassName(module.module_name().clone()).rc())
        } else {
            Error::new(ErrorKind::NameError)
                .set_position(pos)
                .set_message(format!("cannot find identifier '{}' in this scope", ident))
                .into()
        }
    }
}

pub fn check_types(modules: &HashMap<String, Module>) -> Result<(), Vec<Error>> {
    /* Checks the types of all modules */

    let mut errors = vec![];

    for module in modules.values() {
        for function_collection in [module.instance_methods(), module.module_methods()] {
            for function in function_collection.values() {
                let mut type_checker = TypeChecker {
                    modules,
                    current_module: module,
                    current_function: function,
                    current_scope: vec![ vec![] ],
                    final_statement_stack: vec![]
                };

                if let Err(es) = type_checker.check_function_types() {
                    for error in es {
                        errors.push(
                            error
                                .set_line(module.raw_lines())
                                .set_filename(module.filename())
                        );
                    }
                }
            }
        }
    }

    Ok(())
}
