use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::{ast::expression::{BinaryOperator, UnaryOperator}, lexer::{TextPosition, Token, TokenType}, printers::diagnostics_printer::DiagnosticsPrinter};

use crate::typing::ExpressionType;

pub enum Diagnostic {
    Error(DiagnosticError),
}

pub struct DiagnosticError {
    pub error_message: String,
    pub error_pos: TextPosition,
}

pub struct Diagnostics {
    diagnostics: Vec<Diagnostic>,

    source_code: String,
}

pub type DiagnosticsRef = Rc<RefCell<Diagnostics>>;

impl Diagnostics {
    pub fn new(source_code: String) -> Self {
        Self {
            diagnostics: Vec::new(),
            source_code
        }
    }

    pub fn as_ref(self) -> DiagnosticsRef {
        Rc::new(RefCell::new(self))
    }

    fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    fn add_diagnostic_error(&mut self, error_message: String, error_pos: TextPosition) {

        self.add_diagnostic(Diagnostic::Error(DiagnosticError {
            error_pos,
            error_message
        }));
    }

    pub fn unexpected_token_error(&mut self, actual_token: &Token, expected_tokens: &[TokenType]) {
        self.add_diagnostic_error(
            format!("Unexpected token, found '{}' but expects '{:?}'", actual_token.get_type(), expected_tokens),
        actual_token.get_text_pos().clone());
    }

    pub fn missmatched_parens_error(&mut self, text_pos: &TextPosition) {
        self.add_diagnostic_error(format!("Missmatched parentheses"), text_pos.clone());
    }

    pub fn missing_operand_error(&mut self, text_pos: &TextPosition) {
        self.add_diagnostic_error(format!("Missing operand"), text_pos.clone());
    }

    pub fn unknown_token(&mut self, text_pos: &TextPosition) {
        self.add_diagnostic_error(format!("Unknown token"), text_pos.clone());
    }

    pub fn undeclared_variable(&mut self, text_pos: &TextPosition) {
        self.add_diagnostic_error(format!("Undeclared variable"), text_pos.clone());
    }

    pub fn unresolved_binary_expression(&mut self, left: ExpressionType, right: ExpressionType, binary_operator: &BinaryOperator) {
        self.add_diagnostic_error(
            format!("Incompatible types '{:?}', '{:?}' for operator {:?}", left, right, binary_operator.operator),
            binary_operator.text_pos.clone()
        )
    }

    pub fn unresolved_unary_expression(&mut self, expr_type: ExpressionType, unary_operator: &UnaryOperator) {
        self.add_diagnostic_error(
            format!("Incompatible type {:?} with operator {:?}", expr_type, unary_operator.operator),
             unary_operator.text_pos.clone());
    }

    pub fn incompatible_assignement_types(&mut self, left_type: ExpressionType, right_type: ExpressionType, error_pos: TextPosition) {
        self.add_diagnostic_error(
            format!("Cannot assign type '{:?}' to type '{:?}'", right_type, left_type),
            error_pos,
        )
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter()
            .any(|diagnostic| match diagnostic {
                Diagnostic::Error(_) => true,
            })
    }

    pub fn diagnostics(&self) -> &Vec<Diagnostic> {
        &self.diagnostics
    }

}

impl Display for Diagnostics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let diagnostics_printer = DiagnosticsPrinter::new(&self.source_code);
        for diagnostic in &self.diagnostics {
            diagnostics_printer.display_diagnostic(&diagnostic, f)?;
        }
        Ok(())
    }
}