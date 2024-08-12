use std::collections::HashMap;

use crate::{diagnostics::DiagnosticsRef, lexer::TextPosition};

use super::{expression::{BinaryOperator, Operator, UnaryOperator, VariableExpression}, AstExplorer};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    Void,
    Unresolved,
}

impl Type {
    pub fn is_assignable(self, other: Type) -> bool {
        match (self, other) {
            (Type::Int, Type::Int) => true,
            (Type::Bool, Type::Bool) => true,
            (_, Type::Unresolved) => true,
            _ => false
        }
    }
}

#[derive(Debug, Clone)]
struct Variable {
    var_type: Type,
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
    type_value: Type,

}

impl AstResolver {
    pub fn new(diagnostics: DiagnosticsRef) -> Self {
        Self {
            diagnostics,
            variables_table: VariablesTable::new(),
            type_value: Type::Unresolved,
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

    fn report_unresolved_binary_expression(&mut self, left_type: Type, right_type: Type, binary_operator: &BinaryOperator) {
        self.diagnostics.borrow_mut().unresolved_binary_expression(left_type, right_type, binary_operator);
    }

    fn report_unresolved_unary_expression(&mut self, expr_type: Type, unary_operator: &UnaryOperator) {
        self.diagnostics.borrow_mut().unresolved_unary_expression(expr_type, unary_operator)
    }

    fn report_incompatible_assignement(&mut self, left_type: Type, right_type: Type, error_pos: TextPosition) {
        self.diagnostics.borrow_mut().incompatible_assignement_types(left_type, right_type, error_pos);
    }

    fn report_undeclared_variable(&mut self, text_pos: &TextPosition) {
        self.diagnostics.borrow_mut().undeclared_variable(text_pos);
    }

    fn resolve_unary_operator(&mut self, expr_type: Type, operator: Operator) -> Type{
        match (operator, expr_type) {
            (Operator::UnaryPlus, Type::Int) => Type::Int,
            (Operator::UnaryMinus, Type::Int) => Type::Int,
            (Operator::BitwiseNOT, Type::Int) => Type::Int,
            (Operator::Not, Type::Bool) => Type::Bool,
            _ => Type::Unresolved
        }
    }

    fn resolve_binary_operator_expr_type(&self, left_type: Type, right_type: Type, operator: Operator) -> Type {
        let res_type = match (operator, left_type, right_type) {
            (Operator::Plus, Type::Int, Type::Int) => Type::Int,
            (Operator::Minus, Type::Int, Type::Int) => Type::Int,
            (Operator::Multiply, Type::Int, Type::Int) => Type::Int,
            (Operator::Divide, Type::Int, Type::Int) => Type::Int,
            (Operator::Modulo, Type::Int, Type::Int) => Type::Int,
            (Operator::BitwiseAND, Type::Int, Type::Int) => Type::Int,
            (Operator::BitwiseOR, Type::Int, Type::Int) => Type::Int,
            (Operator::BitwiseXOR, Type::Int, Type::Int) => Type::Int,

            (Operator::And, Type::Bool, Type::Bool) => Type::Bool,
            (Operator::Or, Type::Bool, Type::Bool) => Type::Bool,
            _ => Type::Unresolved,
        };

        res_type
    }
}

impl AstExplorer for AstResolver {
    fn explore_semicolon_terminated_expression(&mut self, ast: &super::Ast, expression_id: &super::ExpressionId) {
        self.explore_expression(ast, expression_id);
        self.type_value = Type::Void;
    }

    fn explore_int_expression(&mut self, _: i32) {
        self.type_value = Type::Int;
    }

    fn explore_bool_expression(&mut self, _: bool) {
        self.type_value = Type::Bool;
    }

    fn explore_binary_operator_expression(&mut self, ast: &super::Ast, binary_operator: &super::expression::BinaryOperator) {
        self.explore_expression(ast, &binary_operator.left);
        let left_type = self.type_value;
        self.explore_expression(ast, &binary_operator.right);
        let right_type = self.type_value;

        self.type_value = self.resolve_binary_operator_expr_type(left_type, right_type, binary_operator.operator);
        if self.type_value == Type::Unresolved {
            self.report_unresolved_binary_expression(left_type, right_type, binary_operator);
        }

    }

    fn explore_unary_operator_expression(&mut self, ast: &super::Ast, unary_operator: &super::expression::UnaryOperator) {
        self.explore_expression(ast, &unary_operator.expression);
        let expr_type = self.type_value;
        self.type_value = self.resolve_unary_operator(expr_type, unary_operator.operator);
        if self.type_value == Type::Unresolved {
            self.report_unresolved_unary_expression(expr_type, unary_operator);
        }
    }

    fn explore_variable_expression(&mut self, variable_expression: &VariableExpression) {
        let name = &variable_expression.token.get_value();
        if let Some(variable) = self.get_variable(name) {
            self.type_value = variable.var_type;
        }
        else {
            self.report_undeclared_variable(variable_expression.token.get_text_pos());
        }
    }

    fn explore_assignement_expression(&mut self, ast: &super::Ast, assignement_expr: super::expression::AssignementExpression) {
        self.explore_expression(ast, &assignement_expr.variable);
        let left_type = self.type_value;
        // TODO: if self.check_valid_l_value(ast, expression_id)
        self.explore_expression(ast, &assignement_expr.expression);
        let right_type = self.type_value;

        if !right_type.is_assignable(left_type) {
            self.report_incompatible_assignement(left_type, right_type, assignement_expr.text_pos);
            self.type_value = Type::Unresolved;
        }
        else {
            self.type_value = left_type;
        }

    }

    fn explore_let_statement(&mut self, ast: &super::Ast, let_statement: &super::statement::LetStatement) {
        self.explore_expression(ast, &let_statement.initializer);
        let variable = Variable {
            var_type: self.type_value
        };
        self.declare_variable(let_statement.identifier_name.clone(), variable);
    }
}