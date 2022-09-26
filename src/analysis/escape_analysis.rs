use crate::parse::Function;
use crate::parse::Statement;
use crate::parse::StatementType;
use std::collections::HashMap;


pub struct EscapeResult<'f> {
    function: &'f Function,
    value_escapes: HashMap<&'f String, bool>
}


impl<'f> EscapeResult<'f> {

    /* Function analysis and interface */

    pub fn analyse_function(function: &'f Function) -> Self {
        /* Performs escape analysis on a function */

        let mut escape_result = EscapeResult {
            function,
            value_escapes: HashMap::new()
        };

        for arg in function.args.iter() {
            escape_result.value_escapes.insert(&arg.arg_name, false);
        }

        escape_result.analyse_statement_block(&function.body);

        escape_result
    }

    
    /* Statement analysis */

    fn analyse_statement_block(&mut self, block: &[Statement]) {
        /* Performs escape analyss on a block of statements */

        for stmt in block {
            match &stmt.stmt_type {

                _ => {}
            }
        }
    }
}
