use crate::{ast::{resolver::AstResolver, Ast, AstExplorer}, diagnostics::{Diagnostics, DiagnosticsRef}, lexer::Lexer, parser::Parser, printers::ast_printer::AstPrinter};

pub struct CompilationUnit {
    ast: Ast,
    diagnostics: DiagnosticsRef
}

pub struct Compiler {
    __private: ()
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            __private: ()
        }
    }

    fn parse(&self, code: &str, ast: &mut Ast, diagnostics: DiagnosticsRef) {
        let mut lexer = Lexer::new(code);
        let mut parser = Parser::new(&mut lexer, ast, diagnostics);
        parser.parse();
    }

    fn resolve(&self, ast: &mut Ast, diagnostics: DiagnosticsRef) {
        let mut resolver = AstResolver::new(diagnostics);
        ast.explore(&mut resolver);
    }

    pub fn get_compilation_unit(&self, code: &str) -> Result<CompilationUnit, DiagnosticsRef> {

        let mut ast = Ast::new();
        let diagnostics = Diagnostics::new(code.to_string()).as_ref();

        self.parse(code, &mut ast, DiagnosticsRef::clone(&diagnostics));
        if !diagnostics.borrow().has_errors() {
            self.resolve(&mut ast, DiagnosticsRef::clone(&diagnostics));
        }

        let mut ast_printer = AstPrinter::new();
        ast.explore(&mut ast_printer);
        println!("{ast_printer}");

        if diagnostics.borrow().has_errors() {
            return Err(diagnostics)
        }

        Ok(CompilationUnit {
            ast,
            diagnostics
        })
    }

    pub fn compile<F>(&self, code: &str, ast_explorer: &mut dyn AstExplorer, diagnotics_handler: F) 
        where F : FnOnce(&DiagnosticsRef)
    {
        match self.get_compilation_unit(code) {
            Ok(comp_unit) => {
                comp_unit.ast.explore(ast_explorer);
                diagnotics_handler(&comp_unit.diagnostics);
            },
            Err(diagnostics) => diagnotics_handler(&diagnostics)
        }   
    }
}