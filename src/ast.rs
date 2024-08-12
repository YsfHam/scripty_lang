use std::collections::HashMap;

use expression::{AssignementExpression, BinaryOperator, Expression, Operator, UnaryOperator, VariableExpression};
use statement::{LetStatement, Statement};

use crate::{lexer::{TextPosition, Token}, utils::{Counter, CounterType}};

pub mod statement;
pub mod expression;
pub mod evaluator;
pub mod resolver;

#[derive(Debug, Hash, Eq, PartialEq, Copy, Clone)]
pub struct StatementId(CounterType);

#[derive(Debug, Hash, Eq, PartialEq, Copy, Clone)]
pub struct ExpressionId(CounterType);

pub(crate) struct AstNodeIdGen {
    statement_counter: Counter,
    expression_counter: Counter,
}

impl AstNodeIdGen {
    fn new() -> Self {
        Self {
            statement_counter: Counter::new(1),
            expression_counter: Counter::new(1),
        }
    }

    fn next_expression_id(&self) -> ExpressionId {
        let value = self.expression_counter.get();
        self.expression_counter.increment();
        ExpressionId(value)
    }

    fn next_statement_id(&self) -> StatementId {
        let value = self.statement_counter.get();
        self.statement_counter.increment();
        StatementId(value)
    }
}


pub struct Ast {

    statements: HashMap<StatementId, Statement>,
    expressions: HashMap<ExpressionId, Expression>,

    id_gen: AstNodeIdGen,

    primary_statements: Vec<StatementId>,

}

impl Ast {
    pub fn new() -> Self {
        Self {
            statements: HashMap::new(),
            expressions: HashMap::new(),
            id_gen: AstNodeIdGen::new(),
            primary_statements: Vec::new(),
        }
    }

    fn add_statement(&mut self, statement: Statement) -> StatementId {
        let id = self.id_gen.next_statement_id();
        self.statements.insert(id, statement);
        id
    }

    fn add_expression(&mut self, expression: Expression) -> ExpressionId {
        let id = self.id_gen.next_expression_id();
        self.expressions.insert(id, expression);
        id
    }

    pub fn mark_primary_statement(&mut self, statement_id: StatementId) {
        self.primary_statements.push(statement_id);
    }

    pub fn get_expression(&self, id: &ExpressionId) -> &Expression {
        self.expressions.get(id).unwrap()
    }

    pub fn get_statement(&self, id: &StatementId) -> &Statement {
        self.statements.get(id).unwrap()
    }

    pub fn new_let_statement(&mut self, let_statement: LetStatement) -> StatementId {
        self.add_statement(Statement::Let(let_statement))
    }

    pub fn new_expression_statement(&mut self, expression_id: ExpressionId) -> StatementId {
        self.add_statement(Statement::Expression(expression_id))
    }

    pub fn new_semicolon_terminated_expression_statement(&mut self, expression_id: ExpressionId) -> StatementId {
        self.add_statement(Statement::SemicolonTerminatedExpression(expression_id))
    }

    pub fn new_int_expression(&mut self, value: i32) -> ExpressionId {
        self.add_expression(Expression::Int(value))
    }

    pub fn new_bool_expression(&mut self, value: bool) -> ExpressionId {
        self.add_expression(Expression::Bool(value))
    }
 
    pub fn new_binary_operator_expression(&mut self, operator: expression::Operator, left: ExpressionId, right: ExpressionId, text_pos: TextPosition) -> ExpressionId {
        self.add_expression(Expression::BinaryOperator(
                BinaryOperator::new(operator, left, right, text_pos)
            ))
    }

    pub fn new_unary_opertor_expression(&mut self, operator: Operator, expression: ExpressionId, text_pos: TextPosition) -> ExpressionId {
        self.add_expression(Expression::UnaryOperator(
            UnaryOperator::new(operator, expression, text_pos)
        ))
    }

    pub fn new_variable_expression(&mut self, token: Token) -> ExpressionId {
        self.add_expression(Expression::Variable(
            VariableExpression::new(token)
        ))
    }

    pub fn new_assignement_expression(&mut self, variable: ExpressionId, expression: ExpressionId) -> ExpressionId {
        self.add_expression(Expression::Assignement(
            AssignementExpression::new(variable, expression)
        ))
    }

    pub fn new_incorrect_expression(&mut self) -> ExpressionId {
        self.add_expression(Expression::Incorrect)
    }

    pub fn explore(&self, ast_explorer: &mut dyn AstExplorer) {
        self.primary_statements
            .iter()
            .for_each(|statement_id| ast_explorer.explore_statement(self, statement_id));
    }

}


pub trait AstExplorer {

    fn explore_statement(&mut self, ast: &Ast, statement_id: &StatementId) {
        self.explore_statement_default(ast, statement_id);
    }

    fn explore_statement_default(&mut self, ast: &Ast, statement_id: &StatementId) {
        let statement = ast.get_statement(statement_id).clone();
        match statement {
            Statement::Expression(expression_id) => self.explore_expression(ast, &expression_id),
            Statement::SemicolonTerminatedExpression(expression_id) => self.explore_semicolon_terminated_expression(ast, &expression_id),
            Statement::Let(let_statement) => self.explore_let_statement(ast, &let_statement),
        }
    }

    fn explore_expression(&mut self, ast: &Ast, expression_id: &ExpressionId) {
        let expression = ast.get_expression(expression_id).clone();
        match expression {
            Expression::Int(value) => self.explore_int_expression(value),
            Expression::Bool(value) => self.explore_bool_expression(value),
            Expression::BinaryOperator(bin_operator) => self.explore_binary_operator_expression(ast, &bin_operator),
            Expression::UnaryOperator(unary_operator) => self.explore_unary_operator_expression(ast, &unary_operator),
            Expression::Variable(variable_expression) => self.explore_variable_expression(&variable_expression),
            Expression::Assignement(assignement_expr) => self.explore_assignement_expression(ast, assignement_expr),
            Expression::Incorrect => self.handle_incorrect_expression()
        }
    }

    fn handle_incorrect_expression(&mut self) {
        panic!("Found an incorrect expression");
    }

    fn explore_semicolon_terminated_expression(&mut self, ast: &Ast, expression_id: &ExpressionId);
    fn explore_int_expression(&mut self, value: i32);
    fn explore_bool_expression(&mut self, value: bool);
    fn explore_binary_operator_expression(&mut self, ast: &Ast, binary_operator: &BinaryOperator);
    fn explore_unary_operator_expression(&mut self, ast: &Ast, unary_operator: &UnaryOperator);
    fn explore_variable_expression(&mut self, variable_expression: &VariableExpression);
    fn explore_assignement_expression(&mut self, ast: &Ast, assignement_expr: AssignementExpression);
    fn explore_let_statement(&mut self, ast: &Ast, let_statement: &LetStatement);
}