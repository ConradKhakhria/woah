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


    pub fn evaluate(&mut self) {
       /* Executes a module
        *
        * There's no particular return value here, so all output
        * will be side effects
        */

        match self.root_module.functions.get("main") {
            Some(f) => {
                self.evaluate_function_call(f, &vec![]);
            },
            None => panic!("No main function found in program")
        }
    }


    fn evaluate_function_call(&mut self, function: &'m Function, args: &Vec<Expr>) -> Option<Rc<RefCell<Value<'m>>>> {
        /* Evaluates a function with arguments and returns the result */

        let mut stack_frame = StackFrame::new();

        for (i, arg) in args.iter().enumerate() {
            let arg = self.evaluate_expr(arg);
            stack_frame.add_value(&function.args[i].arg_name, &arg);
        }

        self.stack.push(stack_frame);
    
        for stmt in function.body.iter() {
            if let Some(return_value) = self.evaluate_statement(stmt) {
                return Some(return_value);
            }
        }

        self.stack.pop();

        None
    }


    /* Statement evaluation */

    fn evaluate_statement(&mut self, stmt: &'m Statement) -> Option<Rc<RefCell<Value<'m>>>> {
        /* Evaluates a statement, returning if it's a return statement */

        match &stmt.stmt_type {
            // For this simplified implementation, we will only allow
            // assignments to atomic values, arrays, and array elements
            StatementType::Assign { assigned_to, new_value } => {
                unimplemented!()
            }

            _ => unimplemented!()
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

            ExprKind::AttrRes { parent, attr_name } => {
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
        } else if let Some(value) = self.stack.last().unwrap().get_value(ident) {
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
