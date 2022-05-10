use crate::{
    analysis::get_expr_type,
    error::{ Error, ErrorKind },
    parse::{
        Expr,
        ExprKind,
        Function,
        Module,
        Statement,
        StatementType,
        TypeKind
    }
};
use std::{
    collections::HashMap,
    rc::Rc,
    string::ToString
};


pub struct Scope<'m> {
    frames: Vec<Vec<&'m String>>,
    values: HashMap<&'m String, (Rc<TypeKind>, bool)>
}


impl<'m> Scope<'m> {
    pub fn new() -> Self {
        Scope {
            frames: vec![],
            values: HashMap::new()
        }
    }


    pub fn add_value(&mut self, name: &'m String, v_type: Rc<TypeKind>, constant: bool) -> Option<Error> {
        /* Adds a value to the scope */

        if self.frames.is_empty() {
            self.frames.push(vec![]);
        }

        if self.values.contains_key(name) {
            Error::new(ErrorKind::NameError)
                .set_message(format!("Cannot redefine value '{}'", name))
                .into()
        } else {
            self.frames.last_mut().unwrap().push(name);
            self.values.insert(name, (Rc::clone(&v_type), constant));

            None
        }
    }


    pub fn get_value_type<T: ToString>(&self, ident: &T) -> Option<Rc<TypeKind>> {
        /* Attempts to get a value from the scope */

        if let Some((tp, _)) =self.values.get(&ident.to_string()) {
            Some(Rc::clone(tp))
        } else {
            None
        }
    }


    pub fn is_value_constant<T: ToString>(&self, ident: &T) -> Option<bool> {
        /* Attempts to get a value from the scope */

        if let Some((_, c)) =self.values.get(&ident.to_string()) {
            Some(*c)
        } else {
            None
        }
    }
}


pub struct Analyser<'m> {
    pub modules: HashMap<&'m String, &'m Module>,
    pub current_module: Option<&'m Module>,
    pub current_function: Option<&'m Function>,
    pub current_scope: Scope<'m>,
    pub current_position: (usize, usize)
}


impl<'m> Analyser<'m> {
    pub fn new() -> Self {
        /* Creates a new static analyser */

        Analyser {
            modules: HashMap::new(),
            current_module: None,
            current_function: None,
            current_scope: Scope::new(),
            current_position: (1, 1)
        }
    }


    /* Type analysis */


    fn allowable_value_type_error(&mut self, type_kind: &Rc<TypeKind>, pos: (usize, usize)) -> Option<Error> {
       /* Determines whether values can have our type
        *
        * returns: an error if not
        */

        match &**type_kind {
            TypeKind::EmptyList => {
                Error::new(ErrorKind::TypeError)
                    .set_position(pos)
                    .set_message("The type of this array cannot be determined")
                    .into()
            }

            TypeKind::Function {..} | TypeKind::Module(_) => {
                Error::new(ErrorKind::TypeError)
                    .set_position(pos)
                    .set_message(format!("Cannot declare variables of type '{}'", type_kind))
                    .into()
            }

            _ => None
        }
    }


    /* Expression analysis */


    fn assignable_expression_error(&mut self, expression: &Expr) -> Option<Error> {
       /* Determines whether an expression can be assigned to
        *
        * returns: if not assignable, an error saying so and where
        */

        let error = Error::new(ErrorKind::TypeError).set_position(expression.first_position);

        match &expression.expr_kind {
            ExprKind::ArrayIndexing { array, .. } => {
                self.assignable_expression_error(array)
            }

            ExprKind::ArrayLiteral { .. } => {
                error.set_message("Cannot assign to array literal").into()
            }

            ExprKind::AttrRes { parent, .. } => {
                self.assignable_expression_error(parent)
            }

            ExprKind::Compound { operator, .. } => {
                error.set_message(format!("Cannot assign to '{}' expression", operator)).into()
            }

            ExprKind::Float(_) => error.set_message("Cannot assign to float literal").into(),

            ExprKind::FunctionCall { .. } => error.set_message("Cannot assign to function call").into(),

            ExprKind::Identifier(ident) => {
                if self.current_scope.is_value_constant(ident).unwrap() {
                    error.set_message(format!("Cannot assign to constant value '{}'", ident)).into()
                } else {
                    None
                }
            }

            ExprKind::Integer(_) => error.set_message("Cannot assign to integer literal").into(),

            ExprKind::String(_) => error.set_message("Cannot assign to string literal").into(),

            ExprKind::Unary { operator, .. } => {
                error.set_message(format!("Cannot assign to '{}' statement", operator)).into()
            }
        }
    }


    /* Statement analysis */


    fn analyse_block(&mut self, block: &'m Vec<Statement>) -> Vec<Error> {
        /* Analyses a block of statements */

        let mut errors = vec![];

        for statement in block.iter() {
            errors.append(&mut self.analyse_statement(statement));
        }

        errors
    }


    fn analyse_assignment(&mut self, assigned_to: &Expr, new_value: &Expr) -> Vec<Error> {
        /* Statically analyses the types of an assignment statement */

        let mut assigned_to_type = get_expr_type(self, assigned_to);
        let mut new_value_type = get_expr_type(self, new_value);
        let mut errors = vec![];

        for result in vec![ &mut assigned_to_type, &mut new_value_type ] {
            if let Err(ref mut es) = result {
                errors.append(es);
            }
        }

        match (assigned_to_type, new_value_type) {
            (Ok(att), Ok(nvt)) => {
                if att != nvt {
                    errors.push(Error::new(ErrorKind::TypeError)
                                    .set_position(assigned_to.first_position)
                                    .set_message(format!("Expected type '{}', received '{}'", att, nvt))
                                    .into());
                }

                if let Some(e) = self.assignable_expression_error(assigned_to) {
                    errors.push(e);
                }

                if let Some(e) = self.allowable_value_type_error(&nvt, new_value.first_position) {
                    errors.push(e);
                }
            }

            _ => {}
        }

        errors
    }


    fn analyse_conditional(&mut self, condition: &Expr, block: &'m Vec<Statement>) -> Vec<Error> {
        /* Statically analyses a conditional statement */

        let mut errors = match get_expr_type(self, condition) {
            Ok(tp) => {
                if &*tp != &TypeKind::Bool {
                    Error::new(ErrorKind::TypeError)
                        .set_position(condition.first_position)
                        .set_message("Expected boolean expression in conditional statement")
                        .into()
                } else {
                    vec![]
                }
            }

            Err(es) => es
        };

        errors.append(&mut self.analyse_block(block));

        errors
    }


    fn analyse_declaration(&mut self, statement: &'m Statement) -> Vec<Error> {
        /* Analyses a declaration */

        let (value_name, value_type, value, constant) = match &statement.stmt_type {
            StatementType::Declare { value_name, value_type, value, constant } => {
                (value_name, value_type, value, constant)
            },

            _ => unreachable!()
        };

        match value {
            Some(expr) => {
                let actual_expr_type = match get_expr_type(self, &expr) {
                    Ok(tp) => tp,
                    Err(es) => return es
                };

                match value_type {
                    Some(tp) => {
                        let errors = if &*actual_expr_type == &**tp {
                            vec![]
                        } else {
                            Error::new(ErrorKind::TypeError)
                                .set_position(expr.first_position)
                                .set_message(format!("Expected type {}, received {}", tp, actual_expr_type))
                                .into()
                        };

                        self.current_scope.add_value(value_name, Rc::clone(tp), *constant);
                     
                        errors
                    }

                    None => {
                        match self.allowable_value_type_error(&actual_expr_type, expr.first_position) {
                            Some(e) => e.into(),
                            None => vec![]
                        }
                    }
                }
            }

            None => {
                Error::new(ErrorKind::UnimplementedError)
                    .set_position(statement.first_position)
                    .set_message("I have not implemented delayed value assignment yet")
                    .into()
            }
        }
    }


    fn analyse_for_loop(&mut self, iterator_name: &'m String, range: &Expr, block: &'m Vec<Statement>) -> Vec<Error> {
        /* Analyses a for loop statement */

        let iterator_type = match get_expr_type(self, range) {
            Ok(tp) => {
                match &*tp {
                    TypeKind::List(inner) => Rc::clone(inner),
                    t => return Error::new(ErrorKind::TypeError)
                                            .set_position(range.first_position)
                                            .set_message(format!("Cannot iterate over type {}", t))
                                            .into()
                }
            }

            Err(es) => return es
        };

        self.current_scope.add_value(iterator_name, iterator_type, true);

        self.analyse_block(block)
    }


    fn analyse_return(&mut self, value: &Option<Expr>, pos: (usize, usize)) -> Vec<Error> {
        /* Analyses a return statement */

        let current_function = self.current_function.unwrap();

        match (&current_function.return_type, value) {
            (Some(ret_type), Some(ret_value)) => {
                let value_type = match get_expr_type(self, ret_value) {
                    Ok(t) => t,
                    Err(es) => return es
                };

                if &**ret_type != &*value_type {
                    Error::new(ErrorKind::TypeError)
                        .set_position(pos)
                        .set_message(format!("Function '{}' returns type {}, received {}", current_function.name, ret_type, value_type))
                        .into()
                } else {
                    vec![]
                }
            }

            (None, None) => vec![],

            (Some(ret_type), None) => {
                Error::new(ErrorKind::TypeError)
                    .set_position(pos)
                    .set_message(format!("Function '{}' returns a value of type {}", current_function.name, ret_type))
                    .into()
            }

            (None, Some(_)) => {
                Error::new(ErrorKind::TypeError)
                    .set_position(pos)
                    .set_message(format!("Function '{}' does not return a value", current_function.name))
                    .into()
            }
        }
    }


    pub fn analyse_statement(&mut self, statement: &'m Statement) -> Vec<Error> {
        /* Statically analyses a statement in a function */

        match &statement.stmt_type {
            StatementType::Assign { assigned_to, new_value } => {
                self.analyse_assignment(assigned_to, new_value)
            }

            StatementType::Conditional { condition, block, .. } => {
                self.analyse_conditional(condition, block)
            }

            StatementType::Declare { .. } => {
                self.analyse_declaration(statement)
            }

            StatementType::Else { block } => {
                self.analyse_block(block)
            }

            StatementType::ForLoop { iterator_name, range, block } => {
                self.analyse_for_loop(iterator_name, range, block)
            }

            StatementType::RawExpr { expr } => {
                if let Err(es) = get_expr_type(self, expr) {
                    es
                } else {
                    vec![]
                }
            }

            StatementType::Return { value } => {
                self.analyse_return(value, statement.first_position)
            }

            _ => unimplemented!()
        }
    }


    /* Function analysis */


    pub fn analyse_function(&mut self, function: &'m Function) -> Vec<Error> {
        /* Statically analyses a function */

        self.current_function = Some(function);
        self.current_scope = Scope::new();

        for arg in function.args.iter() {
            self.current_scope.add_value(&arg.arg_name, Rc::clone(&arg.arg_type), true);
        }

        let mut errors = vec![];

        for statement in function.body.iter() {
            errors.append(&mut self.analyse_statement(statement));
        }

        errors
    }


    /* Module analysis */


    pub fn analyse_module(&mut self, module: &'m Module) -> Vec<Error> {
        /* Analyses all functions in a module, recursing on imports */

        if self.modules.contains_key(&module.name) {
            return vec![];
        }

        let mut errors = vec![];

        for import in module.imports.iter() {
            errors.append(&mut self.analyse_module(&import.module));
        }

        for function in module.functions.values() {
            errors.append(&mut self.analyse_function(function));
        }

        self.modules.insert(&module.name, &module);

        errors.into_iter()
              .map(|e| e.set_line(&module.source_lines))
              .collect()
    }
}
