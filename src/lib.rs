use ast::evaluator::Evaluator;
use compiler::Compiler;


pub mod lexer;
pub mod parser;
pub mod ast;
pub mod diagnostics;
pub mod printers;
pub mod compiler;
pub(crate) mod utils;


pub fn run_program() {
    let input = "
          let x = 4
          let y = x = 5
          let c = true && false || true

          z = 4 * 5 /
    ";

   let compiler = Compiler::new();

   let mut evaluator = Evaluator::new();

   compiler.compile(input, &mut evaluator, |diagnostics| {
     println!("{}", diagnostics.borrow());
   });

   println!("{:?}", evaluator.value);

}