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
    stack: Vec<StackFrame>
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
                self.evaluate_function(f, vec![]);
            },
            None => panic!("No main function found in program")
        }
    }


    fn evaluate_function(&mut self, function: &'m Function, args: Vec<&'m Value>) -> Option<Value> {
        /* Evaluates a function with arguments and returns the result */

        self.stack.push(StackFrame::new());
    
        for stmt in function.body.iter() {
            if let Some(return_value) = self.evaluate_statement(stmt) {
                return Some(return_value);
            }
        }

        self.stack.pop();

        None
    }


    /* Statement evaluation */

    fn evaluate_statement(&mut self, stmt: &'m Statement) -> Option<Value> {
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

    fn evaluate_expr(&mut self, expr: &Expr) -> Rc<RefCell<Value>> {
        /* Evaluates an expression, panicking if an error takes place */

        match &expr.expr_kind {
            ExprKind::ArrayIndexing { array, index } => {
                self.eval_array_indexing(array, index)
            }

            _ => unimplemented!()
        }
    }


    fn eval_array_indexing(&mut self, array: &Box<Expr>, index: &Box<Expr>) -> Rc<RefCell<Value>> {
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


    fn eval_array_literal(&mut self, elems: &Vec<Expr>) -> Rc<RefCell<Value>> {
        /* Evaluates an array literal */

        let mut array = Vec::new();

        for elem in elems.iter() {
            array.push(self.evaluate_expr(elem))
        }

        Value::Array(array).rc_refcell()
    }


    fn eval_compound(&mut self, op: &String, left: &Expr, right: &Expr) -> Rc<RefCell<Value>> {
        /* Evaluates a compound expression */

        let left = self.evaluate_expr(left);
        let right = self.evaluate_expr(right);

        let result = match (&*left.borrow(), &*right.borrow()) {
            (Value::Int(x), Value::Int(y)) => Self::eval_integer_compound(op, *x, *y),

            (Value::Int(x), Value::Float(y)) => Self::eval_float_compound(op, *x as f64, *y),

            (Value::Float(x), Value::Int(y)) => Self::eval_float_compound(op, *x, *y as f64),

            (Value::Float(x), Value::Float(y)) => Self::eval_float_compound(op, *x, *y),

            _ => unreachable!()
        };

        result
    }


    fn eval_integer_compound(op: &String, x: i64, y: i64) -> Rc<RefCell<Value>> {
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


    fn eval_float_compound(op: &String, x: f64, y: f64) -> Rc<RefCell<Value>> {
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
}
