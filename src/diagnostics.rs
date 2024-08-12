use std::{cell::RefCell, rc::Rc};

use crate::lexer::{TextPosition, Token, TokenType};

pub enum Diagnostic {
    Error(DiagnosticError),
}

pub enum DiagnosticErrorType {
    UnexpectedToken(UnexpectedTokenError),
    MissmatchedParens,
    MissingOperand,
    UnknownToken,
    UndeclaredVariable
}

pub struct DiagnosticError {
    pub error_type: DiagnosticErrorType,
    pub error_pos: TextPosition,
}

pub struct UnexpectedTokenError {
    pub actual_token_type: TokenType,
    pub expected_token_types: Vec<TokenType>,
}

pub struct Diagnostics {
    diagnostics: Vec<Diagnostic>,
}

pub type DiagnosticsRef = Rc<RefCell<Diagnostics>>;

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }

    pub fn as_ref(self) -> DiagnosticsRef {
        Rc::new(RefCell::new(self))
    }

    fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    fn add_diagnostic_error(&mut self, error_type: DiagnosticErrorType, error_pos: TextPosition) {

        self.add_diagnostic(Diagnostic::Error(DiagnosticError {
            error_pos,
            error_type
        }));
    }

    pub fn unexpected_token_error(&mut self, actual_token: &Token, expected_tokens: &[TokenType]) {
        self.add_diagnostic_error(
            DiagnosticErrorType::UnexpectedToken(UnexpectedTokenError {
                expected_token_types: expected_tokens.iter().map(|token_type| *token_type).collect(),
                actual_token_type: *actual_token.get_type(), 
            }),
            actual_token.get_text_pos().clone()
        );
    }

    pub fn missmatched_parens_error(&mut self, text_pos: &TextPosition) {
        self.add_diagnostic_error(DiagnosticErrorType::MissmatchedParens, text_pos.clone());
    }

    pub fn missing_operand_error(&mut self, text_pos: &TextPosition) {
        self.add_diagnostic_error(DiagnosticErrorType::MissingOperand, text_pos.clone());
    }

    pub fn unknown_token(&mut self, text_pos: &TextPosition) {
        self.add_diagnostic_error(DiagnosticErrorType::UnknownToken, text_pos.clone());
    }

    pub fn undeclared_variable(&mut self, text_pos: &TextPosition) {
        self.add_diagnostic_error(DiagnosticErrorType::UndeclaredVariable, text_pos.clone());
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