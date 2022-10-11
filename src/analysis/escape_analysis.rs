use crate::parse::Expr;
use crate::parse::ExprKind;
use crate::parse::Function;
use crate::parse::Statement;
use crate::parse::StatementType;
use std::collections::HashSet;


pub struct EscapeResult<'f> {
    escaping_values: HashSet<&'f String>
}


impl<'f> EscapeResult<'f> {

    /* Function analysis and interface */

    pub fn analyse_function(function: &'f Function) -> Self {
        /* Performs escape analysis on a function */

        let mut escape_result = EscapeResult { escaping_values: HashSet::new() };

        escape_result.analyse_statement_block(&function.body);

        escape_result
    }


    /* Statement analysis */

    fn analyse_statement_block(&mut self, block: &'f [Statement]) {
        /* Performs escape analysis on a block of statements */

        for stmt in block {
            match &stmt.stmt_type {
                StatementType::Assign { assigned_to, new_value } => {
                    self.analyse_expression(assigned_to);
                    self.analyse_expression(new_value);
                }

                StatementType::ConditionalBlock { conditional_blocks,
                                                  else_block } => {
                    for (cond, block) in conditional_blocks {
                        self.analyse_expression(cond);
                        self.analyse_statement_block(block);
                    }
            
                    if let Some(block) = else_block {
                        self.analyse_statement_block(block);
                    }
                }

                StatementType::Declare { value_name, value, .. } => {
                    self.analyse_expression(value);
                }

                StatementType::IteratorForLoop { iterator_name, range, block } => {
                    self.analyse_expression(range);
                    self.analyse_statement_block(block);
                }

                StatementType::NumericRangeForLoop { iterator_name, start,
                                                     end, step, block } => {
                    for num in [start, end, step] {
                        self.analyse_expression(num);
                    }

                    self.analyse_statement_block(block);
                }

                StatementType::RawExpr { expr } => {
                    self.analyse_expression(expr);
                }

                StatementType::Return { value } => {
                    if let Some(expr) = value {
                        self.analyse_expression(expr);

                        if let ExprKind::Identifier(ident) = &expr.expr_kind {
                            self.escaping_values.insert(ident);
                        }
                    }
                }

                StatementType::WhileLoop { condition, block } => {
                    self.analyse_expression(condition);
                    self.analyse_statement_block(block);
                }
            }
        }
    }


    /* Expression analysis */

    fn analyse_expression(&mut self, expression: &'f Expr) {
       /* Performs escape analysis on an expression
        *
        * This essentially means checking whether a scoped
        * value is used as a function argument
        */

        match &expression.expr_kind {
            ExprKind::ArrayIndexing { array, index } => {
                self.analyse_expression(array);
                self.analyse_expression(index);
            }

            ExprKind::ArrayLiteral { elems } => {
                for expr in elems.iter() {
                    self.analyse_expression(expr);
                }
            }

            ExprKind::AttrRes { parent, .. } => {
                self.analyse_expression(parent);
            }

            ExprKind::Compound { left, right, .. } => {
                self.analyse_expression(left);
                self.analyse_expression(right);
            }

            ExprKind::FunctionCall { function, args } => {
                self.analyse_expression(&function);

                for arg in args {
                    if let ExprKind::Identifier(i) = &arg.expr_kind {
                        self.escaping_values.insert(i);
                    } else {
                        self.analyse_expression(arg);
                    }
                }
            }

            // I'm not sure that the code below is correct

            //ExprKind::Identifier(ident) => {
            //    if !self.escaping_values.contains(ident) {
            //        unreachable!();
            //    }
            //}

            ExprKind::Unary { operand, .. } => {
                self.analyse_expression(operand);
            }

            _ => {}
        }
    }
}
