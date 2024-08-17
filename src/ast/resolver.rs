use std::collections::HashMap;

use crate::{diagnostics::DiagnosticsRef, lexer::TextPosition, typing::ExpressionType};

use super::{expression::{BinaryOperator, Operator, UnaryOperator, VariableExpression}, AstExplorer, AstStorage, ExpressionId};

impl ExpressionType {
    pub fn is_assignable(self, other: ExpressionType) -> bool {
        match (self, other) {
            (ExpressionType::Int, ExpressionType::Int) => true,
            (ExpressionType::Bool, ExpressionType::Bool) => true,
            (_, ExpressionType::Unresolved) => true,
            _ => false
        }
    }
}

#[derive(Debug, Clone)]
struct Variable {
    var_type: ExpressionType,
}

struct VariablesTable {
    variables: HashMap<String, Variable>
}

impl VariablesTable {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    fn add_variable(&mut self, name: String, variable: Variable) {
        self.variables.insert(name, variable);
    }

    fn get_variable(&self, name: &str) -> Option<&Variable> {
        self.variables.get(name)
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

    fn declare_variable(&mut self, name: String, variable: Variable) {
        self.variables_table.add_variable(name, variable);
    }

    fn get_variable(&self, name: &str) -> Option<&Variable> {
        self.variables_table.get_variable(name)
    }

    fn report_unresolved_binary_expression(&mut self, left_type: ExpressionType, right_type: ExpressionType, binary_operator: &BinaryOperator) {
        self.diagnostics.borrow_mut().unresolved_binary_expression(left_type, right_type, binary_operator);
    }

    fn report_unresolved_unary_expression(&mut self, expr_type: ExpressionType, unary_operator: &UnaryOperator) {
        self.diagnostics.borrow_mut().unresolved_unary_expression(expr_type, unary_operator)
    }

    fn report_incompatible_assignement(&mut self, left_type: ExpressionType, right_type: ExpressionType, error_pos: TextPosition) {
        self.diagnostics.borrow_mut().incompatible_assignement_types(left_type, right_type, error_pos);
    }

    fn report_undeclared_variable(&mut self, text_pos: &TextPosition) {
        self.diagnostics.borrow_mut().undeclared_variable(text_pos);
    }

    fn resolve_unary_operator(&mut self, expr_type: ExpressionType, operator: Operator) -> ExpressionType{
        match (operator, expr_type) {
            (Operator::UnaryPlus, ExpressionType::Int) => ExpressionType::Int,
            (Operator::UnaryMinus, ExpressionType::Int) => ExpressionType::Int,
            (Operator::BitwiseNOT, ExpressionType::Int) => ExpressionType::Int,
            (Operator::Not, ExpressionType::Bool) => ExpressionType::Bool,
            _ => ExpressionType::Unresolved
        }
    }

    fn resolve_binary_operator_expr_type(&self, left_type: ExpressionType, right_type: ExpressionType, operator: Operator) -> ExpressionType {
        let res_type = match (operator, left_type, right_type) {
            (Operator::Plus, ExpressionType::Int, ExpressionType::Int) => ExpressionType::Int,
            (Operator::Minus, ExpressionType::Int, ExpressionType::Int) => ExpressionType::Int,
            (Operator::Multiply, ExpressionType::Int, ExpressionType::Int) => ExpressionType::Int,
            (Operator::Divide, ExpressionType::Int, ExpressionType::Int) => ExpressionType::Int,
            (Operator::Modulo, ExpressionType::Int, ExpressionType::Int) => ExpressionType::Int,
            (Operator::BitwiseAND, ExpressionType::Int, ExpressionType::Int) => ExpressionType::Int,
            (Operator::BitwiseOR, ExpressionType::Int, ExpressionType::Int) => ExpressionType::Int,
            (Operator::BitwiseXOR, ExpressionType::Int, ExpressionType::Int) => ExpressionType::Int,

            (Operator::And, ExpressionType::Bool, ExpressionType::Bool) => ExpressionType::Bool,
            (Operator::Or, ExpressionType::Bool, ExpressionType::Bool) => ExpressionType::Bool,
            _ => ExpressionType::Unresolved,
        };
        res_type
    }
}

impl AstExplorer for AstResolver {
    fn explore_semicolon_terminated_expression(&mut self, ast_storage: &mut AstStorage, expression_id: super::ExpressionId) {
        self.explore_expression(ast_storage, expression_id);
    }

    fn explore_int_expression(&mut self, _: i32) {
    }

    fn explore_bool_expression(&mut self, _: bool) {
    }

    fn explore_binary_operator_expression(&mut self, ast_storage: &mut AstStorage, binary_operator: &super::expression::BinaryOperator, expression_id: ExpressionId) {
        self.explore_expression(ast_storage, binary_operator.left);
        let left_type = ast_storage.get_expression(binary_operator.left).expr_type;
        self.explore_expression(ast_storage, binary_operator.right);
        let right_type = ast_storage.get_expression(binary_operator.right).expr_type;

        let expr_type = self.resolve_binary_operator_expr_type(left_type, right_type, binary_operator.operator);
        if expr_type == ExpressionType::Unresolved {
            self.report_unresolved_binary_expression(left_type, right_type, binary_operator);
        }

        ast_storage.get_expression_mut(expression_id).expr_type = expr_type;
    }

    fn explore_unary_operator_expression(&mut self, ast_storage: &mut AstStorage, unary_operator: &super::expression::UnaryOperator, expression_id: ExpressionId) {
        self.explore_expression(ast_storage, unary_operator.expression);
        let expr_type = ast_storage.get_expression(unary_operator.expression).expr_type;
        let res_expr_type = self.resolve_unary_operator(expr_type, unary_operator.operator);
        if res_expr_type == ExpressionType::Unresolved {
            self.report_unresolved_unary_expression(expr_type, unary_operator);
        }

        ast_storage.get_expression_mut(expression_id).expr_type = res_expr_type;
    }

    fn explore_variable_expression(&mut self, ast_storage: &mut AstStorage, variable_expression: &VariableExpression, expression_id: ExpressionId) {
        let name = variable_expression.token.get_value();
        if let Some(variable) = self.get_variable(name) {
            ast_storage.get_expression_mut(expression_id).expr_type = variable.var_type;
        }
        else {
            self.report_undeclared_variable(variable_expression.token.get_text_pos());
        }
    }

    fn explore_assignement_expression(&mut self, ast_storage: &mut AstStorage, assignement_expr: &super::expression::AssignementExpression, expression_id: ExpressionId) {
        self.explore_expression(ast_storage, assignement_expr.variable);
        let var_type = ast_storage.get_expression(assignement_expr.variable).expr_type;
        // TODO: if self.check_valid_l_value(ast, expression_id)
        self.explore_expression(ast_storage, assignement_expr.expression);
        let expr_type = ast_storage.get_expression(assignement_expr.expression).expr_type;

        let res_type = if !expr_type.is_assignable(var_type) {
            self.report_incompatible_assignement(var_type, expr_type, assignement_expr.text_pos.clone());
            ExpressionType::Unresolved
        }
        else {
            var_type
        };

        ast_storage.get_expression_mut(expression_id).expr_type = res_type;

    }

    fn explore_let_statement(&mut self, ast_storage: &mut AstStorage, let_statement: &super::statement::LetStatement) {
        self.explore_expression(ast_storage, let_statement.initializer);
        let variable = Variable {
            var_type: ast_storage.get_expression(let_statement.initializer).expr_type
        };
        self.declare_variable(let_statement.identifier_name.clone(), variable);
    }
}