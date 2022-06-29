use crate::{
    interpret::{
        ProgramState,
        stack_frame::StackFrame
    },
    parse::{
        Expr,
        ExprKind,
    }
};
use std::{
    cell::RefCell,
    rc::Rc,
};


#[derive(Clone)]
pub (super) enum Value {
    Array(Vec<Rc<RefCell<Value>>>),

    Bool(bool),

    Int(i64),

    Float(f64),

    String(String),
}


impl Value {
    pub fn eval_expr(expr: &Expr, stack_frame: &StackFrame) -> Rc<RefCell<Self>> {
        /* Evaluates an expression */

        match &expr.expr_kind {
            ExprKind::ArrayIndexing { array, index } => {
                Self::eval_array_indexing(array, index, stack_frame)
            }

            ExprKind::ArrayLiteral { elems } => {
                Self::eval_array_literal(elems, stack_frame)
            }

            ExprKind::AttrRes { .. } => {
                unimplemented!()
            }

            ExprKind::Compound { operator, left, right } => {
                Self::eval_compound(operator, left, right, stack_frame)
            }

            ExprKind::Float(f) => Value::Float(f.parse().unwrap()).rc_refcell(),

            ExprKind::FunctionCall { function, args } => {
                Self::eval_function(function, args, stack_frame)
            }

            _ => unimplemented!()
        }
    }


    fn eval_array_indexing(array: &Box<Expr>, index: &Box<Expr>, stack_frame: &StackFrame) -> Rc<RefCell<Self>> {
        /* Evaluates an array indexing expression */

        let index = match *Self::eval_expr(index, stack_frame).borrow() {
            Value::Int(i) => i,
            _ => unreachable!()
        };

        match &*Self::eval_expr(array, stack_frame).borrow() {
            Value::Array(xs) => Rc::clone(&xs[index as usize]),
            _ => unreachable!()
        }
    }


    fn eval_array_literal(elems: &Vec<Expr>, stack_frame: &StackFrame) -> Rc<RefCell<Self>> {
        /* Evaluates an array literal */

        let mut array = Vec::new();

        for elem in elems.iter() {
            array.push(Self::eval_expr(elem, stack_frame))
        }

        Value::Array(array).rc_refcell()
    }


    fn eval_compound(op: &String, left: &Expr, right: &Expr, stack_frame: &StackFrame) -> Rc<RefCell<Self>> {
        /* Evaluates a compound expression */

        let left = Self::eval_expr(left, stack_frame);
        let right = Self::eval_expr(right, stack_frame);

        let result = match (&*left.borrow(), &*right.borrow()) {
            (Value::Int(x), Value::Int(y)) => Self::eval_integer_compound(op, *x, *y),

            (Value::Int(x), Value::Float(y)) => Self::eval_float_compound(op, *x as f64, *y),

            (Value::Float(x), Value::Int(y)) => Self::eval_float_compound(op, *x, *y as f64),

            (Value::Float(x), Value::Float(y)) => Self::eval_float_compound(op, *x, *y),

            _ => unreachable!()
        };

        result
    }


    fn eval_integer_compound(op: &String, x: i64, y: i64) -> Rc<RefCell<Self>> {
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


    fn eval_float_compound(op: &String, x: f64, y: f64) -> Rc<RefCell<Self>> {
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


    fn eval_function(function: &Box<Expr>, args: &Vec<Expr>, stack_frame: &StackFrame) -> Rc<RefCell<Self>> {
        /* Evaluates a function call */

        unimplemented!();
    }

    /* Misc */


    pub fn rc_refcell(self) -> Rc<RefCell<Self>> {
        /* Wraps self in an Rc */

        Rc::new(RefCell::new(self))
    }
}
