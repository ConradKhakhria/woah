use crate::{
    interpret::ProgramState,
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
    pub fn eval_expr(expr: &Expr, program_state: &ProgramState) -> Rc<RefCell<Self>> {
        /* Evaluates an expression */

        match &expr.expr_kind {
            ExprKind::ArrayIndexing { array, index } => {
                Self::eval_array_indexing(array, index, program_state)
            }

            ExprKind::ArrayLiteral { elems } => {
                Self::eval_array_literal(elems, program_state)
            }

            ExprKind::AttrRes { .. } => {
                unimplemented!()
            }

            ExprKind::Compound { operator, left, right } => {
                Self::eval_compound(operator, left, right, program_state)
            }

            ExprKind::Float(f) => Value::Float(f.parse().unwrap()).rc_refcell(),

            ExprKind::FunctionCall { function, args } => {
                Self::eval_function(function, args, program_state)
            }

            _ => unimplemented!()
        }
    }


    fn eval_array_indexing(array: &Box<Expr>, index: &Box<Expr>, program_state: &ProgramState) -> Rc<RefCell<Self>> {
        /* Evaluates an array indexing expression */

        let index = match *Self::eval_expr(index, program_state).borrow() {
            Value::Int(i) => i,
            _ => unreachable!()
        };

        match &*Self::eval_expr(array, program_state).borrow() {
            Value::Array(xs) => Rc::clone(&xs[index as usize]),
            _ => unreachable!()
        }
    }


    fn eval_array_literal(elems: &Vec<Expr>, program_state: &ProgramState) -> Rc<RefCell<Self>> {
        /* Evaluates an array literal */

        let mut array = Vec::new();

        for elem in elems.iter() {
            array.push(Self::eval_expr(elem, program_state))
        }

        Value::Array(array).rc_refcell()
    }


    fn eval_compound(op: &String, left: &Expr, right: &Expr, prgram_state: &ProgramState) -> Rc<RefCell<Self>> {
        /* Evaluates a compound expression */

        let left = Self::eval_expr(left, prgram_state);
        let right = Self::eval_expr(right, prgram_state);

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


    fn eval_function(function: &Box<Expr>, args: &Vec<Expr>, prgram_state: &ProgramState) -> Rc<RefCell<Self>> {
        /* Evaluates a function call */

        unimplemented!();
    }

    /* Misc */


    pub fn rc_refcell(self) -> Rc<RefCell<Self>> {
        /* Wraps self in an Rc */

        Rc::new(RefCell::new(self))
    }
}
