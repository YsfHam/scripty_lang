use std::collections::HashMap;

use super::{expression::{ExpressionKind, Operator, VariableExpression}, AstExplorer, AstStorage, ExpressionId};

#[derive(Copy, Clone, Debug)]
pub enum Value {
    Int(i32),
    Bool(bool),
}

impl Value {
    fn add(self, other: Value) -> Option<Value> {
        match (self, other) {
            (Value::Int(x), Value::Int(y)) => Some(Value::Int(x + y)),
            _ => None
        }
    }

    fn substract(self, other: Value) -> Option<Value> {
        match (self, other) {
            (Value::Int(x), Value::Int(y)) => Some(Value::Int(x - y)),
            _ => None
        }
    }

    fn multiply(self, other: Value) -> Option<Value> {
        match (self, other) {
            (Value::Int(x), Value::Int(y)) => Some(Value::Int(x * y)),
            _ => None
        }
    }

    fn divide(self, other: Value) -> Option<Value> {
        match (self, other) {
            (Value::Int(x), Value::Int(y)) => Some(Value::Int(x / y)),
            _ => None
        }
    }

    fn modulo(self, other: Value) -> Option<Value> {
        match (self, other) {
            (Value::Int(x), Value::Int(y)) => Some(Value::Int(x % y)),
            _ => None
        }
    }


    fn bitwise_and(self, other: Value) -> Option<Value> {
        match (self, other) {
            (Value::Int(x), Value::Int(y)) => Some(Value::Int(x & y)),
            _ => None
        }
    }

    fn bitwise_or(self, other: Value) -> Option<Value> {
        match (self, other) {
            (Value::Int(x), Value::Int(y)) => Some(Value::Int(x | y)),
            _ => None
        }
    }

    fn bitwise_xor(self, other: Value) -> Option<Value> {
        match (self, other) {
            (Value::Int(x), Value::Int(y)) => Some(Value::Int(x ^ y)),
            _ => None
        }
    }

    fn bitwise_not(self) -> Option<Value> {
        match self {
            Value::Int(x) => Some(Value::Int(!x)),
            _ => None
        }
    }

    fn unary_minus(self) -> Option<Value> {
        match self {
            Value::Int(x) => Some(Value::Int(-x)),
            _ => None
        }
    }

    fn not(self) -> Option<Value> {
        match self {
            Value::Bool(value) => Some(Value::Bool(!value)),
            _ => None
        }
    }

    fn and(self, other: Value) -> Option<Value> {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => Some(Value::Bool(a && b)),
            _ => None
        }
    }

    fn or(self, other: Value) -> Option<Value> {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => Some(Value::Bool(a || b)),
            _ => None
        }
    }


}

pub struct Evaluator {
    pub value: Option<Value>,

    symbols_table: HashMap<String, Value>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            value: None,
            symbols_table: HashMap::new(),
        }
    }
}

impl AstExplorer for Evaluator {

    fn explore_int_expression(&mut self, value: i32) {
        self.value = Some(Value::Int(value));
    }

    fn explore_binary_operator_expression(&mut self, ast_storage: &mut AstStorage, binary_operator: &super::expression::BinaryOperator, _: ExpressionId) {
        self.explore_expression(ast_storage, binary_operator.left);
        let left_res = self.value.unwrap();
        self.explore_expression(ast_storage, binary_operator.right);
        let right_res = self.value.unwrap();
        let res = match binary_operator.operator {
            Operator::Plus => left_res.add(right_res),
            Operator::Minus => left_res.substract(right_res),
            Operator::Multiply => left_res.multiply(right_res),
            Operator::Divide => left_res.divide(right_res),
            Operator::BitwiseAND => left_res.bitwise_and(right_res),
            Operator::BitwiseOR => left_res.bitwise_or(right_res),
            Operator::BitwiseXOR => left_res.bitwise_xor(right_res),
            Operator::Modulo => left_res.modulo(right_res),
            Operator::And => left_res.and(right_res),
            Operator::Or => left_res.or(right_res),
            op => unreachable!("{op:?} is not a binary operator")
        }.unwrap();

        self.value = Some(res);
    }
    
    fn explore_unary_operator_expression(&mut self, ast_storage: &mut AstStorage, unary_operator: &super::expression::UnaryOperator, _: ExpressionId) {
        self.explore_expression(ast_storage, unary_operator.expression);

        let res = match unary_operator.operator {
            Operator::UnaryMinus => self.value.unwrap().unary_minus().unwrap(),
            Operator::UnaryPlus => self.value.unwrap(),
            Operator::BitwiseNOT => self.value.unwrap().bitwise_not().unwrap(),
            Operator::Not => self.value.unwrap().not().unwrap(),
            op => unreachable!("{op:?} is not a unary operator")
        };


        self.value = Some(res);
    }
    
    fn explore_variable_expression(&mut self, _: &mut AstStorage, variable_expression: &VariableExpression, _: ExpressionId) {
        let name = variable_expression.token.get_value();
        let value = self.symbols_table.get(name).unwrap();
        self.value = Some(*value);

    }
    
    fn explore_assignement_expression(&mut self, ast_storage: &mut AstStorage, assignement_expr: &super::expression::AssignementExpression, _: ExpressionId) {

        let expr_kind = ast_storage.get_expression(assignement_expr.variable).kind.clone();
        let variable_name = match expr_kind {
            ExpressionKind::Variable(name) => Some(name),
            _ => None,
        }.expect("Incorrect l_value");

        self.explore_expression(ast_storage, assignement_expr.expression);

        self.symbols_table.insert(variable_name.token.get_value().clone(), self.value.unwrap());
    }
    
    fn explore_let_statement(&mut self, ast_storage: &mut AstStorage, let_statement: &super::statement::LetStatement) {
        self.explore_expression(ast_storage, let_statement.initializer);
        self.symbols_table.insert(let_statement.identifier_name.clone(), self.value.unwrap());
    }
    
    fn explore_semicolon_terminated_expression(&mut self, ast_storage: &mut AstStorage, expression_id: super::ExpressionId) {
        self.explore_expression(ast_storage, expression_id);
    }
    
    fn explore_bool_expression(&mut self, value: bool) {
        self.value = Some(Value::Bool(value));
    }
}