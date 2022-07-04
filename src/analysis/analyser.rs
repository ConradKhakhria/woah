use crate::{
    analysis::get_expr_type,
    message::{ Message, MsgKind },
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


#[derive(Debug)]
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


    pub fn add_value(&mut self, name: &'m String, v_type: Rc<TypeKind>, constant: bool) -> Option<Message> {
        /* Adds a value to the scope */

        if self.frames.is_empty() {
            self.frames.push(vec![]);
        }

        if self.values.contains_key(name) {
            Message::new(MsgKind::NameError)
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
    pub current_position: (usize, usize),
    pub errors: Vec<Message>,
    pub warnings: Vec<Message>
}


impl<'m> Analyser<'m> {
/* Utils */

    pub fn new() -> Self {
        /* Creates a new static analyser */

        Analyser {
            modules: HashMap::new(),
            current_module: None,
            current_function: None,
            current_scope: Scope::new(),
            current_position: (1, 1),
            errors: vec![],
            warnings: vec![]
        }
    }


    pub fn get_analysis_result(&self) -> Result<(), &Vec<Message>> {
        /* Returns either nothing or a list of errors */

        if self.errors.len() == 0 {
            Ok(())
        } else {
            Err(&self.errors)
        }
    }


    pub fn get_warnings(&self) -> &Vec<Message> {
        /* Returns all warnings generated during static analysis */

        &self.warnings
    }


/* Type analysis */


    fn allowable_value_type_error(&mut self, type_kind: &Rc<TypeKind>, pos: (usize, usize)) -> Option<Message> {
       /* Determines whether values can have our type
        *
        * returns: an error if not
        */

        match &**type_kind {
            TypeKind::EmptyList => {
                Message::new(MsgKind::TypeError)
                    .set_position(pos)
                    .set_message("The type of this array cannot be determined")
                    .into()
            }

            TypeKind::Function {..} | TypeKind::Module(_) => {
                Message::new(MsgKind::TypeError)
                    .set_position(pos)
                    .set_message(format!("Cannot declare variables of type '{}'", type_kind))
                    .into()
            }

            _ => None
        }
    }


    /* Expression analysis */


    fn assignable_expression_error(&mut self, expression: &Expr) -> Option<Message> {
       /* Determines whether an expression can be assigned to
        *
        * returns: if not assignable, an error saying so and where
        */

        let error = Message::new(MsgKind::TypeError).set_position(expression.first_position);

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


    fn analyse_block(&mut self, block: &'m Vec<Statement>) {
        /* Analyses a block of statements */

        for statement in block.iter() {
            self.analyse_statement(statement);
        }
    }


    fn analyse_assignment(&mut self, assigned_to: &Expr, new_value: &Expr) {
        /* Statically analyses the types of an assignment statement */

        let mut assigned_to_type = get_expr_type(self, assigned_to);
        let mut new_value_type = get_expr_type(self, new_value);

        for result in vec![ &mut assigned_to_type, &mut new_value_type ] {
            if let Err(ref mut es) = result {
                self.errors.append(es);
            }
        }

        match (assigned_to_type, new_value_type) {
            (Ok(att), Ok(nvt)) => {
                if att != nvt {
                    self.errors.push(Message::new(MsgKind::TypeError)
                                        .set_position(assigned_to.first_position)
                                        .set_message(format!("Expected type '{}', received '{}'", att, nvt))
                                        .into());
                }

                if let Some(e) = self.assignable_expression_error(assigned_to) {
                    self.errors.push(e);
                }

                if let Some(e) = self.allowable_value_type_error(&nvt, new_value.first_position) {
                    self.errors.push(e);
                }
            }

            _ => {}
        }
    }


    fn analyse_conditional(&mut self, cases: &'m Vec<(Expr, Vec<Statement>)>, default: &'m Option<Vec<Statement>>) {
        /* Statically analyses a conditional statement */

        for (cond, block) in cases.iter() {
            match get_expr_type(self, cond) {
                Ok(tp) => {
                    if &*tp != &TypeKind::Bool {
                        self.errors.push(Message::new(MsgKind::TypeError)
                                            .set_position(cond.first_position)
                                            .set_message("Expected boolean expression in conditional statement"));
                    }
                }
    
                Err(ref mut es) => self.errors.append(es)
            }

            self.analyse_block(block);
        }

        if let Some(block) = default {
            self.analyse_block(block);
        }
    }


    fn analyse_declaration(&mut self, statement: &'m Statement) {
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
                    Err(ref mut es) => {
                        self.errors.append(es);
                        return;
                    }
                };

                match value_type {
                    Some(tp) => {
                        if &*actual_expr_type != &**tp {
                            self.errors.push(Message::new(MsgKind::TypeError)
                                                .set_position(expr.first_position)
                                                .set_message(format!("Expected type {}, received {}", tp, actual_expr_type)));
                        }

                        self.current_scope.add_value(value_name, Rc::clone(tp), *constant);
                    }

                    None => {
                        match self.allowable_value_type_error(&actual_expr_type, expr.first_position) {
                            Some(e) => self.errors.push(e),
                            None => {}
                        };

                        self.current_scope.add_value(value_name, Rc::clone(&actual_expr_type), *constant);
                    }
                }
            }

            None => {
                self.errors.push(Message::new(MsgKind::UnimplementedError)
                                    .set_position(statement.first_position)
                                    .set_message("I have not implemented delayed value assignment yet"));
            }
        }
    }


    fn analyse_iterator_for_loop(&mut self, iterator_name: &'m String, iterator: &Expr, block: &'m Vec<Statement>) {
        /* Analyses an iterator-based for loop statement */

        let iterator_type = match get_expr_type(self, iterator) {
            Ok(tp) => {
                match &*tp {
                    TypeKind::List(inner) => Rc::clone(inner),
                    t => {
                        self.errors.push(Message::new(MsgKind::TypeError)
                                            .set_position(iterator.first_position)
                                            .set_message(format!("Cannot iterate over type {}", t)));
                        return;
                    }
                }
            }

            Err(ref mut es) => {
                self.errors.append(es);
                return;
            }
        };

        self.current_scope.add_value(iterator_name, iterator_type, true);
        self.analyse_block(block)
    }


    fn analyse_range_for_loop(&mut self, iterator_name: &'m String, rs: &[&Expr; 3], block: &'m Vec<Statement>) {
        /* Analyses a range-based for loop statement */

        let mut errors = vec![];

        let mut start_type = get_expr_type(self, rs[0]);
        let mut end_type = get_expr_type(self, rs[1]);
        let mut step_type = get_expr_type(self, rs[2]);

        for res in vec![ &mut start_type, &mut end_type, &mut step_type ] {
            if let Err(ref mut es) = res {
                errors.append(es);
            }
        }

        if !errors.is_empty() {
            self.errors.append(&mut errors);
            return;
        }

        let start_type = start_type.unwrap();

        let iterator_type = match &*start_type {
            TypeKind::Int | TypeKind::Float => Rc::clone(&start_type),
            t => {
                self.errors.push(Message::new(MsgKind::TypeError)
                                    .set_position(rs[0].first_position)
                                    .set_message(format!("Cannot iterate over type {}", t)));
                return;
            }
        };

        match &*end_type.unwrap() {
            TypeKind::Int | TypeKind::Float => {},
            t => {
                self.errors.push(Message::new(MsgKind::TypeError)
                                    .set_position(rs[1].first_position)
                                    .set_message(format!("Cannot compare value of type {} to this value of type {}", iterator_type, t))
                                    .into());
                return;
            }
        }

        match &*step_type.unwrap() {
            TypeKind::Int => {}

            TypeKind::Float => {
                if &*iterator_type == &TypeKind::Int {
                    self.errors.push(Message::new(MsgKind::TypeError)
                                .set_position(rs[2].first_position)
                                .set_message("Cannot increment an integer by a float type"));
                    return;
                }
            }

            t => {
                self.errors.push(Message::new(MsgKind::TypeError)
                            .set_position(rs[1].first_position)
                            .set_message(format!("Cannot compare value of type {} to this value of type {}", iterator_type, t)));
                return;
            }
        }

        self.current_scope.add_value(iterator_name, iterator_type, true);

        self.analyse_block(block)
    }


    fn analyse_return(&mut self, value: &Option<Expr>, pos: (usize, usize)) {
        /* Analyses a return statement */

        let current_function = self.current_function.unwrap();

        match (&*current_function.return_type, value) {
            (TypeKind::NoReturnType, Some(_)) => {
                self.errors.push(Message::new(MsgKind::TypeError)
                                    .set_position(pos)
                                    .set_message(format!("Function '{}' does not return a value", current_function.name)))
            },

            (ret_type, Some(ret_value)) => {
                let value_type = match get_expr_type(self, ret_value) {
                    Ok(t) => t,
                    Err(ref mut es) => {
                        self.errors.append(es);
                        return;
                    }
                };

                if *ret_type != *value_type {
                    self.errors.push(Message::new(MsgKind::TypeError)
                                        .set_position(pos)
                                        .set_message(format!(
                                            "Function '{}' returns type {}, received {}",
                                            current_function.name,
                                            ret_type,
                                            value_type
                                        )));
                }
            }

            (ret_type, None) => {
                self.errors.push(Message::new(MsgKind::TypeError)
                                    .set_position(pos)
                                    .set_message(format!("Function '{}' returns a value of type {}", current_function.name, ret_type)))
            }

            _ => {}
        }
    }


    pub fn analyse_statement(&mut self, statement: &'m Statement) {
        /* Statically analyses a statement in a function */

        match &statement.stmt_type {
            StatementType::Assign { assigned_to, new_value } => {
                self.analyse_assignment(assigned_to, new_value);
            }

            StatementType::Conditional { cases, default } => {
                self.analyse_conditional(cases, default);
            }

            StatementType::Declare { .. } => {
                self.analyse_declaration(statement);
            }

            StatementType::IteratorForLoop { iterator_name, range, block } => {
                self.analyse_iterator_for_loop(iterator_name, range, block);
            }

            StatementType::RangeForLoop { iterator_name, start_value, end_value, step_value, block } => {
                self.analyse_range_for_loop(iterator_name, &[start_value, end_value, step_value], block);
            }

            StatementType::RawExpr { expr } => {
                if let Err(ref mut es) = get_expr_type(self, expr) {
                    self.errors.append(es);
                }
            }

            StatementType::Return { value } => {
                self.analyse_return(value, statement.first_position);
            }

            _ => unimplemented!()
        }
    }


    /* Function analysis */


    pub fn analyse_function(&mut self, function: &'m Function) {
        /* Statically analyses a function */

        self.current_function = Some(function);
        self.current_scope = Scope::new();

        for arg in function.args.iter() {
            self.current_scope.add_value(&arg.arg_name, Rc::clone(&arg.arg_type), true);
        }

        for statement in function.body.iter() {
            self.analyse_statement(statement);
        }

        if let TypeKind::NoReturnType = &*function.return_type {} else {
            if !self.contains_return(&function.body){
                self.errors.push(Message::new(MsgKind::TypeError)
                                    .set_position(function.body.last().unwrap().first_position)
                                    .set_message(format!(
                                        "Function '{}' does not return type {} in all cases",
                                        function.name,
                                        function.return_type.as_ref()
                                    )));
            }
        }
    }


    fn contains_return(&mut self, statements: &'m [Statement]) -> bool {
       /* Returns whether a block of statements contains a return
        *
        * A dead code warning is generated if any code is unreachable after a return
        */

        let last_index = statements.len() - 1;

        for (i, stmt) in statements.iter().enumerate() {
            match &stmt.stmt_type {
                StatementType::Return {..} => {
                    if i < last_index {
                        self.warnings.push(Message::new(MsgKind::DeadCodeWarning)
                                            .set_position(stmt.first_position)
                                            .set_message("Code following this return is unreachable"));
                    }

                    return true;
                },

                StatementType::Conditional { cases, default } => {
                    if cases.len() == 1 && default.is_none() {
                        continue;
                    }

                    let mut all_paths_return = true;

                    for (_, block) in cases.iter() {
                        if !self.contains_return(block) {
                            all_paths_return = false;
                        }
                    }

                    if let Some(block) = default {
                        if !self.contains_return(block) {
                            all_paths_return = false;
                        }
                    }

                    if all_paths_return {
                        return true;
                    }
                },

                _ => {}
            }
        }

        false
    }

    /* Module analysis */


    pub fn analyse_module(&mut self, module: &'m Module) {
        /* Analyses all functions in a module, recursing on imports */

        if self.modules.contains_key(&module.name) {
            return;
        }

        self.current_module = Some(module);

        for import in module.imports.iter() {
            self.analyse_module(&import.module);
        }

        for function in module.functions.values() {
            self.analyse_function(function);
        }

        self.modules.insert(&module.name, &module);

        for e in self.errors.iter_mut() {
            *e = e.clone().set_line(&module.source_lines);
        }

        for w in self.warnings.iter_mut() {
            *w = w.clone().set_line(&module.source_lines);
        }
    }
}
