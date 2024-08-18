use ast::evaluator::Evaluator;
use compiler::Compiler;


pub mod lexer;
pub mod parser;
pub mod ast;
pub mod diagnostics;
pub mod printers;
pub mod compiler;
pub mod typing;
pub(crate) mod utils;


pub fn run_program() {
    let input = "
    let y = 6;
    ";
   let compiler = Compiler::new();

   let mut evaluator = Evaluator::new();

   compiler.compile(input, &mut evaluator, |diagnostics| {
     println!("{}", diagnostics.borrow());
   });

   println!("{:?}", evaluator.value);

}