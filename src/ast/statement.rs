use super::*;

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(ExpressionId),
    SemicolonTerminatedExpression(ExpressionId),
    Let(LetStatement),
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub identifier_name: String,
    pub initializer: ExpressionId,
}