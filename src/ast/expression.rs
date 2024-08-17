use crate::{lexer::{TextPosition, Token, TokenType}, typing::ExpressionType};

use super::ExpressionId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    // Int operators
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    BitwiseOR,
    BitwiseXOR,
    BitwiseAND,
    
    BitwiseNOT,
    UnaryMinus,
    UnaryPlus,

    // Assignement
    Assignement,
    
    // Bool operators
    And,
    Or,

    Not,
    
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorType {
    Unary,
    Binary,
}

struct OperatorInfo {
    operator: Operator,
    precedence: u8,
    op_type: OperatorType,
    token_type: TokenType
}

static OPERATORS_TABLE: &[OperatorInfo] = &[

    OperatorInfo {operator: Operator::BitwiseOR, precedence: 14, op_type: OperatorType::Binary, token_type: TokenType::Pipe},
    OperatorInfo {operator: Operator::BitwiseXOR, precedence: 15, op_type: OperatorType::Binary, token_type: TokenType::Hat},
    OperatorInfo {operator: Operator::BitwiseAND, precedence: 16, op_type: OperatorType::Binary, token_type: TokenType::Ampersand},
    OperatorInfo {operator: Operator::Plus, precedence: 17, op_type: OperatorType::Binary, token_type: TokenType::Plus},
    OperatorInfo {operator: Operator::Minus, precedence: 17, op_type: OperatorType::Binary, token_type: TokenType::Minus},
    OperatorInfo {operator: Operator::Divide, precedence: 19, op_type: OperatorType::Binary, token_type: TokenType::Slash},
    OperatorInfo {operator: Operator::Modulo, precedence: 19, op_type: OperatorType::Binary, token_type: TokenType::Percent},
    OperatorInfo {operator: Operator::Multiply, precedence: 19, op_type: OperatorType::Binary, token_type: TokenType::Asterisk},
    
    OperatorInfo {operator: Operator::And, precedence: 19, op_type: OperatorType::Binary, token_type: TokenType::DoubleAmpersand},
    OperatorInfo {operator: Operator::Or, precedence: 19, op_type: OperatorType::Binary, token_type: TokenType::DoublePipe},

    OperatorInfo {operator: Operator::Assignement, precedence: 1, op_type: OperatorType::Binary, token_type: TokenType::Equals},


    OperatorInfo {operator: Operator::UnaryMinus, precedence: 20, op_type: OperatorType::Unary, token_type: TokenType::Minus},
    OperatorInfo {operator: Operator::UnaryPlus, precedence: 20, op_type: OperatorType::Unary, token_type: TokenType::Plus},
    OperatorInfo {operator: Operator::BitwiseNOT, precedence: 20, op_type: OperatorType::Unary, token_type: TokenType::Tilde},

    OperatorInfo {operator: Operator::Not, precedence: 20, op_type: OperatorType::Unary, token_type: TokenType::Bang},


];

fn get_operator_info_field<T>(find_func: impl Fn(&&OperatorInfo) -> bool, map_func: impl Fn(&OperatorInfo) -> T) -> Option<T> {
    OPERATORS_TABLE.iter()
        .find(find_func)
        .map(map_func)
}

impl Operator {
    pub fn binary_operator(token_type: &TokenType) -> Option<Self> {

        get_operator_info_field(
            |op_info| op_info.token_type == *token_type && op_info.op_type == OperatorType::Binary,
            |op_info| op_info.operator
        )
    }

    pub fn unary_operator(token_type: &TokenType) -> Option<Self> {
        get_operator_info_field(
            |op_info| op_info.token_type == *token_type && op_info.op_type == OperatorType::Unary,
            |op_info| op_info.operator
        )
    }

    pub fn precedence(&self) -> u8 {
        get_operator_info_field(
            |op_info| op_info.operator == *self,
            |op_info| op_info.precedence
        ).unwrap()
    }

    pub fn get_type(&self) -> OperatorType {
        get_operator_info_field(
            |op_info| op_info.operator == *self,
            |op_info| op_info.op_type
        ).unwrap()
    }

    pub fn is_assignement(&self) -> bool {
        match self {
            Operator::Assignement => true,
            _ => false,
        }
    }

    pub fn to_string(&self) -> String {

        let token_type = get_operator_info_field(
            |op_info| op_info.operator == *self,
            |op_info| op_info.token_type
        ).unwrap();

        format!("{}", token_type)
    }
}


#[derive(Debug, Clone)]
pub struct BinaryOperator {
    pub operator: Operator,
    pub text_pos: TextPosition,
    pub left: ExpressionId,
    pub right: ExpressionId,
}

impl BinaryOperator {
    pub fn new(operator: Operator, left: ExpressionId, right: ExpressionId, text_pos: TextPosition) -> Self {
        Self {
            operator,
            text_pos,
            left,
            right,
        }
    }
}
#[derive(Debug, Clone)]
pub struct UnaryOperator {
    pub operator: Operator,
    pub text_pos: TextPosition,
    pub expression: ExpressionId,
}

impl UnaryOperator {
    pub fn new(operator: Operator, expression: ExpressionId, text_pos: TextPosition) -> Self {
        Self {
            operator,
            text_pos,
            expression
        }
    }
}

#[derive(Debug, Clone)]
pub struct AssignementExpression {
    pub text_pos: TextPosition,
    pub variable: ExpressionId,
    pub expression: ExpressionId,
}

impl AssignementExpression {
    pub fn new(variable: ExpressionId, expression: ExpressionId, text_pos: TextPosition) -> Self {
        Self {
            variable,
            expression,
            text_pos
        }
    }
}

#[derive(Debug, Clone)]
pub struct VariableExpression {
    pub token: Token,
}

impl VariableExpression {
    pub fn new(token: Token) -> Self {
        Self {
            token,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Int(i32),
    Bool(bool),
    Variable(VariableExpression),
    Assignement(AssignementExpression),
    BinaryOperator(BinaryOperator),
    UnaryOperator(UnaryOperator),
    Incorrect,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub expr_type: ExpressionType
}

impl Expression {
    pub fn new(kind: ExpressionKind) -> Self {
        let expr_type = match kind {
            ExpressionKind::Int(_) => ExpressionType::Int,
            ExpressionKind::Bool(_) => ExpressionType::Bool,
            _ => ExpressionType::Unresolved,
        };
        Self {
            kind,
            expr_type
        }
    }
}