use ast::evaluator::Evaluator;
use compiler::Compiler;
use diagnostics::DiagnosticsRef;

use printers::diagnostics_printer::DiagnosticsPrinter;

pub mod lexer;
pub mod parser;
pub mod ast;
pub mod diagnostics;
pub mod printers;
pub mod compiler;
pub(crate) mod utils;


pub fn run_program() {
    let input = "
    let x = true
    x + 1
    ";

   let compiler = Compiler::new();

   let mut evaluator = Evaluator::new();

   compiler.compile(input, &mut evaluator, |diagnostics| {
        let diagnostics_printer = DiagnosticsPrinter::new(DiagnosticsRef::clone(diagnostics), input);

        println!("{diagnostics_printer}");
   });

   println!("{:?}", evaluator.value);

}