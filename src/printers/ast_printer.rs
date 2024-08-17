use std::fmt::Display;

use colored::{Color, Colorize};

use crate::{ast::{expression::{Operator, VariableExpression}, AstExplorer, AstStorage, ExpressionId}, typing::ExpressionType};


const INT_COLOR: Color = Color::Cyan;
const VARIABLE_COLOR: Color = Color::BrightBlue;
const KEYWORD_COLOR: Color = Color::Blue;
const OPERATOR_COLOR: Color = Color::White;
const PARENTHESE_COLOR: Color = Color::Magenta;
const TYPE_COLOR: Color = Color::BrightGreen;

pub struct AstPrinter {
    code_str: Vec<String>,
}

impl AstPrinter {
    pub fn new() -> Self {
        Self {
            code_str: Vec::new(),
        }
    }

    fn push_colorized(&mut self, text: String, color: Color) {
        self.code_str.push(text.color(color).to_string());
    }

    fn push_int(&mut self, value: i32) {
        self.push_colorized(value.to_string(), INT_COLOR);
    }

    fn push_whitespace(&mut self) {
        self.code_str.push(" ".to_string());
    }

    fn push_newline(&mut self) {
        self.code_str.push("\n".to_string()); 
    }

    fn push_operator(&mut self, operator: &Operator) {
        self.push_colorized(operator.to_string(), OPERATOR_COLOR);
    }

    fn push_identifier(&mut self, variable_name: &str) {
        self.push_colorized(variable_name.to_string(), VARIABLE_COLOR);
    }

    fn push_keyword(&mut self, keyword: &str) {
        self.push_colorized(keyword.to_string(), KEYWORD_COLOR);
    }

    fn push_semicolon(&mut self) {
        self.push_colorized(";".to_string(), OPERATOR_COLOR);
    }

    fn push_equals(&mut self) {
        self.push_colorized("=".to_string(), OPERATOR_COLOR);
    }
    
    fn push_open_parenthese(&mut self) {
        self.push_colorized("(".to_string(), PARENTHESE_COLOR);
    }

    fn push_closed_parenthese(&mut self) {
        self.push_colorized(")".to_string(), PARENTHESE_COLOR);
    }

    fn push_colon(&mut self) {
        self.push_colorized(":".to_string(), OPERATOR_COLOR);
    }

    fn push_type(&mut self, expr_type: ExpressionType) {
        self.push_colorized(expr_type.to_string(), TYPE_COLOR);
    }

}

impl AstExplorer for AstPrinter {

    fn explore_statement(&mut self, ast_storage: &mut AstStorage, statement_id: crate::ast::StatementId) {
        self.explore_statement_default(ast_storage, statement_id);

        self.push_newline();
    }

    fn explore_int_expression(&mut self, value: i32) {
        self.push_int(value);
        
    }

    fn explore_binary_operator_expression(&mut self, ast_storage: &mut AstStorage, binary_operator: &crate::ast::expression::BinaryOperator, expression: ExpressionId) {
        self.push_open_parenthese();
        self.explore_expression(ast_storage, binary_operator.left);
        self.push_whitespace();
        self.push_operator(&binary_operator.operator);
        self.push_whitespace();
        self.explore_expression(ast_storage, binary_operator.right);
        self.push_closed_parenthese();
    }

    fn explore_unary_operator_expression(&mut self, ast_storage: &mut AstStorage, unary_operator: &crate::ast::expression::UnaryOperator, expression: ExpressionId) {
        self.push_open_parenthese();
        self.push_operator(&unary_operator.operator);
        self.explore_expression(ast_storage, unary_operator.expression);
        self.push_closed_parenthese();
    }

    fn explore_variable_expression(&mut self, ast_storage: &mut AstStorage, variable_expression: &VariableExpression, expression_id: ExpressionId) {
        self.push_identifier(&variable_expression.token.get_value());
    }

    fn explore_assignement_expression(&mut self, ast_storage: &mut AstStorage, assignement_expr: &crate::ast::expression::AssignementExpression, expression: ExpressionId) {
        self.explore_expression(ast_storage, assignement_expr.variable);
        self.push_whitespace();
        self.push_equals();
        self.push_whitespace();
        self.explore_expression(ast_storage, assignement_expr.expression);
    }

    fn explore_let_statement(&mut self, ast_storage: &mut AstStorage, let_statement: &crate::ast::statement::LetStatement) {
        self.push_keyword("let");
        self.push_whitespace();
        self.push_identifier(&let_statement.identifier_name);
        self.push_colon();
        self.push_whitespace();
        self.push_type(ast_storage.get_expression(let_statement.initializer).expr_type);
        self.push_whitespace();
        self.push_equals();
        self.push_whitespace();
        self.explore_expression(ast_storage, let_statement.initializer);
        self.push_semicolon();
    }
    
    fn explore_semicolon_terminated_expression(&mut self, ast_storage: &mut AstStorage, expression_id: crate::ast::ExpressionId) {
        self.explore_expression(ast_storage, expression_id);
        self.push_semicolon();
    }

    fn handle_incorrect_expression(&mut self) {
        self.push_colorized("Incorrect".to_string(), Color::Red);
    }
    
    fn explore_bool_expression(&mut self, value: bool) {
        self.push_keyword(&value.to_string());
    }
}

impl Display for AstPrinter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.code_str.iter()
        .map(|code| write!(f, "{}", code))
        .find(|result| result.is_err())
        .unwrap_or(Ok(()))
    }
}