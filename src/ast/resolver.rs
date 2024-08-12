use std::collections::HashMap;

use crate::diagnostics::DiagnosticsRef;

use super::{expression::VariableExpression, AstExplorer};

struct VariablesTable {
    variables: HashMap<String, ()>
}

impl VariablesTable {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    fn add_variable(&mut self, name: String) {
        self.variables.insert(name, ());
    }

    fn check_variable(&self, name: &str) -> bool {
        self.variables.contains_key(name)
    }
}

pub struct AstResolver {
    diagnostics: DiagnosticsRef,
    variables_table: VariablesTable,

}

impl AstResolver {
    pub fn new(diagnostics: DiagnosticsRef) -> Self {
        Self {
            diagnostics,
            variables_table: VariablesTable::new(),
        }
    }

    // fn check_valid_l_value(&mut self, ast: &Ast, expression_id: &ExpressionId) {
    //     let expression = ast.get_expression(expression_id);

    //     // match expression {
    //     //     Expression::Variable(_) => self.diagnostics.borrow_mut().invalid_l_value( ),
    //     //     _ => (),
    //     // }
    // }

    fn declare_variable(&mut self, name: String) {
        self.variables_table.add_variable(name);
    }

    fn check_variable(&mut self, name: &str) -> bool {
        self.variables_table.check_variable(name)
    }
}

impl AstExplorer for AstResolver {
    fn explore_semicolon_terminated_expression(&mut self, ast: &super::Ast, expression_id: &super::ExpressionId) {
        self.explore_expression(ast, expression_id);
    }

    fn explore_int_expression(&mut self, value: i32) {
    }

    fn explore_bool_expression(&mut self, value: bool) {
    }

    fn explore_binary_operator_expression(&mut self, ast: &super::Ast, binary_operator: &super::expression::BinaryOperator) {
        self.explore_expression(ast, &binary_operator.left);
        self.explore_expression(ast, &binary_operator.right);
    }

    fn explore_unary_operator_expression(&mut self, ast: &super::Ast, unary_operator: &super::expression::UnaryOperator) {
        self.explore_expression(ast, &unary_operator.expression);
    }

    fn explore_variable_expression(&mut self, variable_expression: &VariableExpression) {
        let name = &variable_expression.token.get_value();
        if !self.check_variable(name) {
            self.diagnostics.borrow_mut().undeclared_variable(variable_expression.token.get_text_pos());
        }
    }

    fn explore_assignement_expression(&mut self, ast: &super::Ast, assignement_expr: super::expression::AssignementExpression) {
        self.explore_expression(ast, &assignement_expr.variable);
        // TODO: if self.check_valid_l_value(ast, expression_id)
        self.explore_expression(ast, &assignement_expr.expression);
    }

    fn explore_let_statement(&mut self, ast: &super::Ast, let_statement: &super::statement::LetStatement) {
        self.explore_expression(ast, &let_statement.initializer);
        self.declare_variable(let_statement.identifier_name.clone());
    }
}