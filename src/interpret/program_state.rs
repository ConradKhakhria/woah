use crate::{
    interpret::{
        stack_frame::StackFrame,
        value::Value
    },
    parse::{
        Function,
        Module,
        Statement,
        StatementType
    }
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

        let main_function = match self.root_module.functions.get("main") {
            Some(f) => self.evaluate_function(f, vec![]),
            None => panic!("No main function found in program")
        };
    }


    fn evaluate_function(&mut self, function: &'m Function, args: Vec<&'m Value>) -> Option<Value> {
        /* Evaluates a function with arguments and returns the result */

        let mut stack_frame = StackFrame::new();
    
        for stmt in function.body.iter() {
            if let Some(return_value) = self.evaluate_statement(stmt, &mut stack_frame) {
                return Some(return_value);
            }
        }

        None
    }


    fn evaluate_statement(&mut self, stmt: &'m Statement, stack_frame: &mut StackFrame) -> Option<Value> {
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
}
