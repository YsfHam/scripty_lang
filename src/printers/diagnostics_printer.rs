use std::{cmp::{max, min}, fmt::{Display, Formatter}};

use crate::{diagnostics::{Diagnostic, DiagnosticError, DiagnosticErrorType, DiagnosticsRef, UnexpectedTokenError}, lexer::{TextPosition, TokenType}};
use colored::Colorize;

pub struct DiagnosticsPrinter<'a> {
    diagnostics: DiagnosticsRef,

    code_lines: Vec<&'a str>
}

impl<'a> DiagnosticsPrinter<'a> {

    const PRE_HIGHLITED_AREA_MAX_LEN: usize = 20;
    const POST_HIGHLITED_AREA_MAX_LEN: usize = 10;

    pub fn new(diagnostics: DiagnosticsRef, source_code: &'a str) -> Self {
        Self {
            diagnostics,
            code_lines: source_code.lines().collect()
        }
    }

    fn display_diagnostic(&self, diagnostic: &Diagnostic, f: &mut Formatter) -> std::fmt::Result {
        match diagnostic {
            Diagnostic::Error(diag_error) => {
                write!(f, "ERROR: ")?;
                self.display_error(diag_error, f)
            }
        }
    }

    fn display_unexpected_token_error(&self, unexpected_token_error: &UnexpectedTokenError, text_pos: &TextPosition, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "Unexpected token, found '{}' but expects ", unexpected_token_error.actual_token_type)?;
        Self::display_tokens_types_list(f, &unexpected_token_error.expected_token_types)?;
        writeln!(f)?;
        self.highlight_text_area(text_pos, f)
    }

    fn display_error(&self, diag_error: &DiagnosticError, f: &mut Formatter) -> std::fmt::Result {
        match &diag_error.error_type {
            DiagnosticErrorType::UnexpectedToken(unexpected_token_error) => {
                self.display_unexpected_token_error(unexpected_token_error, &diag_error.error_pos, f)
            },
            DiagnosticErrorType::MissmatchedParens => {
                writeln!(f, "Missmatched parenthese")?;
                self.highlight_text_area(&diag_error.error_pos, f)
            },
            DiagnosticErrorType::MissingOperand => {
                writeln!(f, "Missing operand")?;
                self.highlight_text_area(&diag_error.error_pos, f)
            },
            DiagnosticErrorType::UnknownToken => {
                writeln!(f, "Unknown symbole")?;
                self.highlight_text_area(&diag_error.error_pos, f)
            }
            DiagnosticErrorType::UndeclaredVariable => {
                writeln!(f, "Undeclared variable")?;
                self.highlight_text_area(&diag_error.error_pos, f)
            }
            
        }
    }

    fn get_next_line(&self, text_pos: &TextPosition) -> (&str, TextPosition) {
        
        let (code_line, new_line_index) = self.next_code_line(text_pos.line as usize).unwrap();

        let current_code_line = self.code_lines.get(text_pos.line as usize - 1).unwrap();

        let diff = code_line.len() as i32 - current_code_line.len() as i32;

        let start = text_pos.col_start as i32 + diff;
        let end = text_pos.col_end as i32 + diff;

        (
            code_line,
            TextPosition {
                line: new_line_index as u32,
                col_start: start as u32,
                col_end: end as u32,
            }
        )
    }

    fn next_code_line(&self, start_pos: usize) -> Option<(&str, usize)> {
        for index in (0..start_pos).rev() {
            if !self.code_lines.get(index).unwrap().trim().is_empty() {
                return Some((self.code_lines.get(index).unwrap(), index + 1));
            }
        }

        return None;
    }

    fn highlight_text_area(&self, text_pos: &TextPosition, f: &mut Formatter) -> std::fmt::Result {

        let (text_area, text_pos) = self.get_next_line(text_pos);

        writeln!(f, "---> {}:{}", text_pos.line, text_pos.col_start)?;
        writeln!(f, " |")?;
        writeln!(f, " |")?;

        write!(f, " | ")?;
        let pre_area_len = min(Self::PRE_HIGHLITED_AREA_MAX_LEN, text_pos.col_start as usize - 1);
        if pre_area_len > 0 {
            let pre_area_start = text_pos.col_start as usize - pre_area_len - 1;
            let pre_area_end = min(pre_area_start + pre_area_len, text_area.len());
            write!(f, "{}", &text_area[pre_area_start..pre_area_end])?;
        }

        let area_start = text_pos.col_start as usize - 1;
        let area_end = text_pos.col_end as usize - 1;
        write!(f, "{}", &text_area[area_start..area_end].red())?;

        let post_area_start = text_pos.col_end as usize - 1;
        let post_area_end = min(post_area_start + Self::POST_HIGHLITED_AREA_MAX_LEN, text_area.len());
        if let Some(area) = text_area.get(post_area_start..post_area_end) {
            write!(f, "{}", area)?;
        }

        writeln!(f)?;
        write!(f, "{}{}", " ".repeat(pre_area_len + 3), "^".repeat(max(area_end - area_start, 1)))?;

        Ok(())
    }

    fn display_tokens_types_list(f: &mut Formatter, list: &[TokenType]) -> std::fmt::Result {
        let mut iter = list.iter();

        write!(f, "[ ")?;
        write!(f, "{}", iter.next().unwrap())?;

        for token_type in iter {
            write!(f, ", ")?;
            write!(f, "{}", token_type)?;
        }

        write!(f," ]")
    }

}

impl Display for DiagnosticsPrinter<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.diagnostics
            .borrow_mut()
            .diagnostics()
            .iter()
            .map(|diag| {
                self.display_diagnostic(diag, f)?;
                writeln!(f)
            })
            .find(|res| res.is_err())
            .unwrap_or(Ok(()))
    }
}