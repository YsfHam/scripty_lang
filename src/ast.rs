use std::collections::HashMap;

use expression::{AssignementExpression, BinaryOperator, Expression, ExpressionKind, Operator, UnaryOperator, VariableExpression};
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

pub struct AstStorage {
    statements: HashMap<StatementId, Statement>,
    expressions: HashMap<ExpressionId, Expression>
}

impl AstStorage {
    pub fn new() -> Self {
        Self {
            statements: HashMap::new(),
            expressions: HashMap::new(),
        }
    }

    fn add_statement(&mut self, id: StatementId, statement: Statement) {
        self.statements.insert(id, statement);
    }

    pub fn get_statement(&self, id: StatementId) -> &Statement {
        self.statements.get(&id).unwrap()
    }

    pub fn get_statement_mut(&mut self, id: StatementId) -> &mut Statement {
        self.statements.get_mut(&id).unwrap()
    }

    fn add_expression(&mut self, id: ExpressionId, expression: Expression) {
        self.expressions.insert(id, expression);
    }

    pub fn get_expression(&self, id: ExpressionId) -> &Expression {
        self.expressions.get(&id).unwrap()
    }

    pub fn get_expression_mut(&mut self, id: ExpressionId) -> &mut Expression {
        self.expressions.get_mut(&id).unwrap()
    }
}

pub struct Ast {

    ast_storage: AstStorage,

    id_gen: AstNodeIdGen,

    primary_statements: Vec<StatementId>,

}

impl Ast {
    pub fn new() -> Self {
        Self {
            ast_storage: AstStorage::new(),
            id_gen: AstNodeIdGen::new(),
            primary_statements: Vec::new(),
        }
    }

    fn add_statement(&mut self, statement: Statement) -> StatementId {
        let id = self.id_gen.next_statement_id();
        self.ast_storage.add_statement(id, statement);
        id
    }

    fn add_expression(&mut self, expr_kind: ExpressionKind) -> ExpressionId {
        let id = self.id_gen.next_expression_id();
        self.ast_storage.add_expression(id, Expression::new(expr_kind));
        id
    }

    pub fn mark_primary_statement(&mut self, statement_id: StatementId) {
        self.primary_statements.push(statement_id);
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
        self.add_expression(ExpressionKind::Int(value))
    }

    pub fn new_bool_expression(&mut self, value: bool) -> ExpressionId {
        self.add_expression(ExpressionKind::Bool(value))
    }
 
    pub fn new_binary_operator_expression(&mut self, operator: expression::Operator, left: ExpressionId, right: ExpressionId, text_pos: TextPosition) -> ExpressionId {
        self.add_expression(ExpressionKind::BinaryOperator(
                BinaryOperator::new(operator, left, right, text_pos)
            ))
    }

    pub fn new_unary_opertor_expression(&mut self, operator: Operator, expression: ExpressionId, text_pos: TextPosition) -> ExpressionId {
        self.add_expression(ExpressionKind::UnaryOperator(
            UnaryOperator::new(operator, expression, text_pos)
        ))
    }

    pub fn new_variable_expression(&mut self, token: Token) -> ExpressionId {
        self.add_expression(ExpressionKind::Variable(
            VariableExpression::new(token)
        ))
    }

    pub fn new_assignement_expression(&mut self, variable: ExpressionId, expression: ExpressionId, text_pos: TextPosition) -> ExpressionId {
        self.add_expression(ExpressionKind::Assignement(
            AssignementExpression::new(variable, expression, text_pos)
        ))
    }

    pub fn new_incorrect_expression(&mut self) -> ExpressionId {
        self.add_expression(ExpressionKind::Incorrect)
    }

    pub fn explore(&mut self, ast_explorer: &mut dyn AstExplorer) {
        for statement_id in &self.primary_statements {
            ast_explorer.explore_statement(&mut self.ast_storage, *statement_id);
        }
    }

}


pub trait AstExplorer {

    fn explore_statement(&mut self, ast_storage: &mut AstStorage, statement_id: StatementId) {
        self.explore_statement_default(ast_storage, statement_id);
    }

    fn explore_statement_default(&mut self, ast_storage: &mut AstStorage, statement_id: StatementId) {
        let statement = ast_storage.get_statement(statement_id).clone();
        match statement {
            Statement::Expression(expression_id) => self.explore_expression(ast_storage, expression_id),
            Statement::SemicolonTerminatedExpression(expression_id) => self.explore_semicolon_terminated_expression(ast_storage, expression_id),
            Statement::Let(let_statement) => self.explore_let_statement(ast_storage, &let_statement),
        }
    }

    fn explore_expression(&mut self, ast_storage: &mut AstStorage, expression_id: ExpressionId) {
        self.explore_expression_default(ast_storage, expression_id);
    }

    fn explore_expression_default(&mut self, ast_storage: &mut AstStorage, expression_id: ExpressionId) {
        let expression = ast_storage.get_expression(expression_id).clone();
        match &expression.kind {
            ExpressionKind::Int(value) => self.explore_int_expression(*value),
            ExpressionKind::Bool(value) => self.explore_bool_expression(*value),
            ExpressionKind::BinaryOperator(bin_operator) => self.explore_binary_operator_expression(ast_storage, &bin_operator, expression_id),
            ExpressionKind::UnaryOperator(unary_operator) => self.explore_unary_operator_expression(ast_storage, &unary_operator, expression_id),
            ExpressionKind::Variable(variable_expression) => self.explore_variable_expression(ast_storage, &variable_expression, expression_id),
            ExpressionKind::Assignement(assignement_expr) => self.explore_assignement_expression(ast_storage, assignement_expr, expression_id),
            ExpressionKind::Incorrect => self.handle_incorrect_expression()
        }
    }

    fn handle_incorrect_expression(&mut self) {
        panic!("Found an incorrect expression");
    }

    fn explore_semicolon_terminated_expression(&mut self, ast_storage: &mut AstStorage, expression_id: ExpressionId);
    
    fn explore_int_expression(&mut self, value: i32);
    fn explore_bool_expression(&mut self, value: bool);
    fn explore_binary_operator_expression(&mut self, ast_storage: &mut AstStorage, binary_operator: &BinaryOperator, expression_id: ExpressionId);
    fn explore_unary_operator_expression(&mut self, ast_storage: &mut AstStorage, unary_operator: &UnaryOperator, expression_id: ExpressionId);
    fn explore_variable_expression(&mut self, ast_storage: &mut AstStorage, variable_expression: &VariableExpression, expression_id: ExpressionId);
    fn explore_assignement_expression(&mut self, ast_storage: &mut AstStorage, assignement_expr: &AssignementExpression, expression_id: ExpressionId);
    fn explore_let_statement(&mut self, ast_storage: &mut AstStorage, let_statement: &LetStatement);
}