use crate::{
    interpret::{
        stack_frame::StackFrame,
        value::Value
    },
    parse::{
        Expr,
        ExprKind,
        Function,
        Module,
        Statement,
        StatementType
    }
};
use std::{
    cell::RefCell,
    rc::Rc
};

pub struct ProgramState<'m> {
    root_module: &'m Module,
    stack: Vec<StackFrame<'m>>
}


impl<'m> ProgramState<'m> {
    pub fn new(root_module: &'m Module) -> Self {
        ProgramState {
            root_module,
            stack: Vec::new()
        }
    }


    fn get_stack_frame(&mut self) -> &mut StackFrame<'m> {
        /* Returns a reference to the current stack frame */

        self.stack.last_mut().unwrap()
    }


    /* Evaluation */

    pub fn evaluate(&mut self) {
       /* Executes a module
        *
        * There's no particular return value here, so all output
        * will be side effects
        */

        match self.root_module.functions.get("main") {
            Some(f) => {
                match self.evaluate_function_call(f, &vec![]) {
                    Some(r) => println!("{}", r.borrow()),
                    None => println!()
                }
            },
            None => panic!("No main function found in program")
        }
    }


    fn evaluate_function_call(&mut self, function: &'m Function, args: &Vec<Expr>) -> Option<Rc<RefCell<Value<'m>>>> {
        /* Evaluates a function with arguments and returns the result */

        if function.name == "println" {
            if args.len() == 1 {
                println!("{}", self.evaluate_expr(&args[0]).borrow());
            } else {
                panic!("Function 'println' takes 1 argument exactly");
            }

            return Some(Value::NoValue.rc_refcell());
        }

        let mut stack_frame = StackFrame::new();

        for (i, arg) in args.iter().enumerate() {
            let arg = self.evaluate_expr(arg);
            stack_frame.add_value(&function.args[i].arg_name, &Some(arg));
        }

        self.stack.push(stack_frame);
    
        let return_value = self.evaluate_block(&function.body);

        self.stack.pop();

        return_value
    }


    /* Statement evaluation */

    fn evaluate_block<I: IntoIterator<Item = &'m Statement>>(&mut self, block: I) -> Option<Rc<RefCell<Value<'m>>>> {
       /* Evaluates a block of statements
        *
        * Returns
        * -------
        * If a return takes place somewhere within the block
        *     Some(a return value, or a NoValue)
        * Otherwise
        *     None
        */

        self.get_stack_frame().add_scope();

        for stmt in block.into_iter() {
            println!("{:?}", stmt);

            match &stmt.stmt_type {
                StatementType::Assign { assigned_to, new_value } => {
                    self.evaluate_assignment(assigned_to, new_value);
                }

                StatementType::Declare { value_name, value, constant, .. } => {
                    self.evaluate_declaration(value_name, value, *constant);
                }
    
                StatementType::Conditional { cases, default } => {
                    if let Some(return_value) = self.evaluate_conditional(cases, default) {
                        self.get_stack_frame().pop_scope();

                        return Some(return_value);
                    }
                }

                StatementType::IteratorForLoop { iterator_name, range, block } => {
                    self.evaluate_ifl(iterator_name, range, block);
                }

                StatementType::RangeForLoop { iterator_name, start_value, end_value, step_value, block } => {
                    self.evaluate_rfl(iterator_name, [start_value, end_value, step_value], block);
                }
    
                StatementType::RawExpr { expr } => {
                    if let ExprKind::FunctionCall { function, args } = &expr.expr_kind {
                        let function = match *self.evaluate_expr(function).borrow() {
                            Value::Function(f) => f,
                            _ => panic!("Expected function")
                        };

                        let _discard = self.evaluate_function_call(function, args);
                    } else {
                        self.evaluate_expr(expr); // This is of course purely for the sake of side effects
                    }
                }
    
                StatementType::Return { value } => {
                    let return_value = match value {
                        Some(e) => Some(self.evaluate_expr(e)),
                        None => Some(Value::NoValue.rc_refcell())
                    };

                    self.get_stack_frame().pop_scope();

                    return return_value;
                }

                StatementType::WhileLoop { condition, block } => {
                    if let Some(return_value) = self.evaluate_while_loop(condition, block) {
                        self.get_stack_frame().pop_scope();

                        return Some(return_value);
                    }
                }
    
                _ => unimplemented!()
            }
        }

        self.get_stack_frame().pop_scope();
        None
    }


    fn evaluate_assignment(&mut self, assigned_to: &Expr, new_value: &Expr) {
        /* Performs value assignment */

        let assigned_to = self.evaluate_expr(assigned_to);
        let new_value = self.evaluate_expr(new_value);

        *assigned_to.borrow_mut() = (*new_value).borrow().clone();
    }


    fn evaluate_conditional(&mut self, cases: &'m Vec<(Expr, Vec<Statement>)>, default: &'m Option<Vec<Statement>>) -> Option<Rc<RefCell<Value<'m>>>> {
        /* Evaluates a conditional block */

        for (condition, block) in cases.iter() {
            let condition = match *self.evaluate_expr(condition).borrow() {
                Value::Bool(b) => b,
                _ => unreachable!()
            };

            if condition {
                return self.evaluate_block(block)
            }
        }

        match default {
            Some(block) => self.evaluate_block(block),
            None => None
        }
    }


    fn evaluate_declaration(&mut self, value_name: &String, expr: &Option<Expr>, constant: bool) {
        /* Adds a value to the stack frame */

        let value = match expr {
            Some(e) => {
                let value = self.evaluate_expr(e);

                Some(if !constant {
                    value.borrow().clone().rc_refcell()
                } else {
                    value
                })
            },
            None => None
        };

        self.get_stack_frame().add_value(value_name, &value);
    }


    fn evaluate_ifl(&mut self, iterator: &String, range: &Expr, block: &'m Vec<Statement>) -> Option<Rc<RefCell<Value<'m>>>> {
        /* Evaluates an iterator for loop */

        if let Value::Array(ref xs) = *self.evaluate_expr(range).borrow() {
            for x in xs.iter() {
                self.get_stack_frame().add_value(iterator, &Some(Rc::clone(x)));

                let ret_val = self.evaluate_block(block);

                self.get_stack_frame().remove_value(iterator.as_str());

                if let Some(rv) = ret_val {
                    return Some(rv);
                }
            }
        } else {
            unreachable!()
        }

        None
    }


    fn evaluate_rfl(&mut self, iterator: &String, dims: [&Expr; 3], block: &'m Vec<Statement>) -> Option<Rc<RefCell<Value<'m>>>> {
        /* Evaluates a range for loop */

        let start = self.evaluate_expr(&dims[0]);
        let end = self.evaluate_expr(&dims[1]);
        let step = self.evaluate_expr(&dims[2]);

        self.get_stack_frame().add_value(iterator, &Some(Rc::clone(&start)));

        'forLoop: loop {
            match (&*start.borrow(), &*end.borrow()) {
                (Value::Int(iterator), Value::Int(limit)) => {
                    if iterator >= limit {
                        break 'forLoop;
                    }
                }

                (Value::Int(iterator), Value::Float(limit)) => {
                    if *iterator as f64 >= *limit {
                        break 'forLoop;
                    }
                }

                (Value::Float(iterator), Value::Int(limit)) => {
                    if *iterator >= *limit as f64 {
                        break 'forLoop;
                    }
                }

                (Value::Float(iterator), Value::Float(limit)) => {
                    if iterator >= limit {
                        break 'forLoop;
                    }
                }

                _ => unreachable!()
            }

            if let Some(ret_val) = self.evaluate_block(block) {
                self.get_stack_frame().remove_value(iterator);

                return Some(ret_val);
            }

            let new_iterator_value =match (&*start.borrow(), &*step.borrow()) {
                (Value::Int(iterator), Value::Int(increment)) => Value::Int(iterator + increment),

                (Value::Float(iterator), Value::Int(limit)) => Value::Float(*iterator + *limit as f64),

                (Value::Float(iterator), Value::Float(limit)) => Value::Float(iterator + limit),

                _ => unreachable!()
            };

            *start.borrow_mut() = new_iterator_value;
        }

        self.get_stack_frame().remove_value(iterator);

        None
    }


    fn evaluate_while_loop(&mut self, condition: &Expr, block: &'m Vec<Statement>) -> Option<Rc<RefCell<Value<'m>>>> {
        /* Evaluates a while loop */

        loop {
            let condition = match *self.evaluate_expr(condition).borrow() {
                Value::Bool(b) => b,
                _ => unreachable!()
            };

            if condition {
                match self.evaluate_block(block) {
                    Some(return_value) => return Some(return_value),
                    None => {}
                }
            } else {
                return None;
            }
        }
    }


    /* Expression evaluation */

    fn evaluate_expr(&mut self, expr: &Expr) -> Rc<RefCell<Value<'m>>> {
        /* Evaluates an expression, panicking if an error takes place */

        match &expr.expr_kind {
            ExprKind::ArrayIndexing { array, index } => {
                self.eval_array_indexing(array, index)
            }
            
            ExprKind::ArrayLiteral { elems } => {
                self.eval_array_literal(elems)
            }

            ExprKind::AttrRes { .. } => {
                unimplemented!()
            }

            ExprKind::Compound { operator, left, right } => {
                self.eval_compound(operator, left, right)
            }

            ExprKind::Float(f) => {
                Value::Float(f.parse().unwrap()).rc_refcell()
            }

            ExprKind::FunctionCall { function, args } => {
                match *self.evaluate_expr(function).borrow() {
                    Value::Function(f) => {
                        self.evaluate_function_call(f, args)    
                            .expect("Function does not return a value")
                    },
                    _ => panic!("Expected function")
                }
            }

            ExprKind::Identifier(ident) => {
                self.eval_identifier(ident)
            }

            ExprKind::Integer(i) => {
                Value::Int(i.parse().unwrap()).rc_refcell()
            }

            ExprKind::String(s) => {
                Value::String(s.clone()).rc_refcell()
            }

            ExprKind::Unary { operator, operand } => {
                self.eval_unary(operator, operand)
            }
        }
    }


    fn eval_array_indexing(&mut self, array: &Box<Expr>, index: &Box<Expr>) -> Rc<RefCell<Value<'m>>> {
        /* Evaluates an array indexing expression */

        let index = match *self.evaluate_expr(index).borrow() {
            Value::Int(i) => i,
            _ => unreachable!()
        };

        match &*self.evaluate_expr(array).borrow() {
            Value::Array(xs) => Rc::clone(&xs[index as usize]),
            _ => unreachable!()
        }
    }


    fn eval_array_literal(&mut self, elems: &Vec<Expr>) -> Rc<RefCell<Value<'m>>> {
        /* Evaluates an array literal */

        let mut array = Vec::new();

        for elem in elems.iter() {
            array.push(self.evaluate_expr(elem))
        }

        Value::Array(array).rc_refcell()
    }


    fn eval_compound(&mut self, operator: &String, left: &Expr, right: &Expr) -> Rc<RefCell<Value<'m>>> {
        /* Evaluates a compound expression */

        let left = self.evaluate_expr(left);
        let right = self.evaluate_expr(right);

        let result = match (&*left.borrow(), &*right.borrow()) {
            (Value::Int(x), Value::Int(y)) => Self::eval_integer_compound(operator, *x, *y),

            (Value::Int(x), Value::Float(y)) => Self::eval_float_compound(operator, *x as f64, *y),

            (Value::Float(x), Value::Int(y)) => Self::eval_float_compound(operator, *x, *y as f64),

            (Value::Float(x), Value::Float(y)) => Self::eval_float_compound(operator, *x, *y),

            _ => unreachable!()
        };

        result
    }


    fn eval_integer_compound(op: &String, x: i64, y: i64) -> Rc<RefCell<Value<'m>>> {
        /* Evaluates a compound expression of 2 integers */

        let result = match op.as_str() {
            "+" => Value::Int(x + y),
            "-" => Value::Int(x - y),
            "*" => Value::Int(x * y),
            "/" => Value::Int(x / y),
            "%" => Value::Int(x % y),

            "==" => Value::Bool(x == y),
            "!=" => Value::Bool(x != y),
            "<"  => Value::Bool(x < y),
            ">"  => Value::Bool(x > y),
            "<=" => Value::Bool(x <= y),
            ">=" => Value::Bool(x >= y),

            _ => unreachable!()
        };

        Rc::new(RefCell::new(result))
    }


    fn eval_float_compound(op: &String, x: f64, y: f64) -> Rc<RefCell<Value<'m>>> {
        /* Evaluates a compound expression of 2 floats */

        let result = match op.as_str() {
            "+" => Value::Float(x + y),
            "-" => Value::Float(x - y),
            "*" => Value::Float(x * y),
            "/" => Value::Float(x / y),
            "%" => Value::Float(x % y),

            "==" => Value::Bool(x == y),
            "!=" => Value::Bool(x != y),
            "<"  => Value::Bool(x < y),
            ">"  => Value::Bool(x > y),
            "<=" => Value::Bool(x <= y),
            ">=" => Value::Bool(x >= y),

            _ => unreachable!()
        };

        Rc::new(RefCell::new(result))
    }


    fn eval_identifier(&mut self, ident: &String) -> Rc<RefCell<Value<'m>>> {
        /* Resolves an identifier */

        if let Some(func) = self.root_module.functions.get(ident) {
            Value::Function(func).rc_refcell()
        } else if let Some(value) = self.get_stack_frame().get_value(ident.as_str()) {
            value
        } else {
            panic!("Unknown identifier '{}'", ident)
        }
    }


    fn eval_unary(&mut self, operator: &String, operand: &Expr) -> Rc<RefCell<Value<'m>>> {
        /* Evaluates a unary expression */

        let operand = self.evaluate_expr(operand);

        match operator.as_str() {
            "-" => {
                match *operand.borrow() {
                    Value::Float(f) => Value::Float(-f).rc_refcell(),
                    Value::Int(i) => Value::Int(-i).rc_refcell(),
                    _ => panic!("Expected numeric operand for '{}' unary expression", operator)
                }
            },

            _ => panic!("Unknown unary operator")
        }
    }
}
