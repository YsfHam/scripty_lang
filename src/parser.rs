use std::iter::Peekable;


use crate::{ast::{expression::OperatorType, statement::LetStatement, Ast, ExpressionId, StatementId}, diagnostics::DiagnosticsRef, lexer::{TextPosition, Token, TokenType}, utils::Counter};
use crate::ast::expression::Operator;
type TokensIteratorRef<'a> = &'a mut dyn Iterator<Item = Token>;

pub struct Parser<'a> {
    tokens_it: Peekable<TokensIteratorRef<'a>>,
    ast: &'a mut Ast,
    diagnostics: DiagnosticsRef
}

impl<'a> Parser<'a> {
    pub fn new(tokens_it: TokensIteratorRef<'a>, ast: &'a mut Ast, diagnostics: DiagnosticsRef) -> Self {
        Self {
            tokens_it: tokens_it.peekable(),
            ast,
            diagnostics
        }
    }

    pub fn parse(&mut self) {
        while let Some(statement_id) = self.next_statement() {
            self.ast.mark_primary_statement(statement_id);
        }
    }

    fn next_statement(&mut self) -> Option<StatementId> {
        while let Some(TokenType::Semicolon) = self.current().map(|token| token.get_type()) {
            self.consume();
        }
        if self.end_of_tokens() {
            None
        }
        else {
            Some(self.parse_statement())
        }
    }

    fn parse_statement(&mut self) -> StatementId {
        let token = self.current().unwrap().clone();
        match token.get_type() {
            TokenType::LetKeyword => {
                let statement = self.parse_let_statement();
                self.ast.new_let_statement(statement)
            }
            _ => self.parse_expression_statement(),
        }
        
    }

    fn parse_expression_statement(&mut self) -> StatementId {
        let expr_id = self.parse_expression();
        if self.consume_if(&TokenType::Semicolon) {
            self.ast.new_semicolon_terminated_expression_statement(expr_id)
        }
        else {
            self.ast.new_expression_statement(expr_id)
        }
    }

    fn parse_expression(&mut self) -> ExpressionId {
        let expr_id = ExpresionEvaluator::new().eval(self);
        expr_id
    }

    fn parse_let_statement(&mut self) -> LetStatement {
        self.expect_and_consume(&TokenType::LetKeyword);

        let identifier_name = self.parse_identifier();
        self.expect_and_consume(&TokenType::Equals);
        let initializer = self.parse_expression();
        self.consume_if(&TokenType::Semicolon);

        LetStatement {
            identifier_name,
            initializer
        }
    }

    fn parse_identifier(&mut self) -> String {
        self.expect_and_consume(&TokenType::Identifier).get_value().clone()
    }

    fn end_of_tokens(&mut self) -> bool {
        if let Some(token_type) = self.current().map(|token| token.get_type()) {
            *token_type == TokenType::Eof
        }
        else {
            true
        }
    }

    fn current(&mut self) -> Option<&Token> {
        self.tokens_it.peek()
    }

    fn consume(&mut self) -> Token {
        self.tokens_it.next().unwrap()
    }

    fn consume_if(&mut self, token_type: &TokenType) -> bool {
        if self.current().unwrap().get_type() == token_type {
            self.consume();
            true
        }
        else {
            false
        }
    }

    fn expect(&mut self, expected_token_type: &TokenType) {
        let actual_token = self.current().unwrap().clone();

        let actual_token_type = actual_token.get_type();
        if actual_token_type != expected_token_type {
            self.diagnostics.borrow_mut().unexpected_token_error(&actual_token, &[*expected_token_type]);
        }
    }

    fn expect_and_consume(&mut self, expected_token_type: &TokenType) -> Token {
        self.expect(expected_token_type);
        self.consume()
    }

}

struct ParentheseIndex {
    counter: Counter,
    paren_pos: TextPosition,
}

impl ParentheseIndex {
    fn new(paren_pos: TextPosition) -> Self {
        Self {
            counter: Counter::new(0),
            paren_pos
        }
    }
}

struct ExpresionEvaluator {
    output_stack: Vec<ExpressionId>,
    operators_stack: Vec<Operator>,
    operators_pos_stack: Vec<TextPosition>,
    parenthese_index: Vec<ParentheseIndex>,
    

    last_token: Option<Token>
}

static EXPECTED_UNARY_TOKENS: &[TokenType] = &[
    TokenType::Int,
    TokenType::TrueKeyword,
    TokenType::FalseKeyword,
    TokenType::Identifier
];

impl ExpresionEvaluator {

    fn new() -> Self {
        Self {
            output_stack: Vec::new(),
            operators_stack: Vec::new(),
            operators_pos_stack: Vec::new(),
            parenthese_index: Vec::new(),
            last_token: None
        }
    }

    fn eval(&mut self, parser: &mut Parser) -> ExpressionId {

        let mut abort = false;

        loop {
            let continue_parsing = 
                    !parser.end_of_tokens() && 
                    *self.get_token(parser).get_type() != TokenType::Semicolon 
                    && !abort
            ;

            if !continue_parsing {
                break;
            }

            self.parse_primary_expression(parser);
            abort = self.parse_rest(parser);


        };

        if let Some(paren_index) = self.parenthese_index.last() {
            parser.diagnostics.borrow_mut().missmatched_parens_error(&paren_index.paren_pos);
            return parser.ast.new_incorrect_expression();
        }

        while let Some(op) = self.operators_stack.pop() {
            let pos = self.operators_pos_stack.pop().unwrap();
            self.output_operator(op, pos, parser);
        }

        if let Some(expr) = self.output_stack.pop() {
            expr
        }
        else {
            let actual_token = parser.current().unwrap().clone();
            parser.diagnostics.borrow_mut().unexpected_token_error(&actual_token, EXPECTED_UNARY_TOKENS);
            parser.ast.new_incorrect_expression()
        }
    }

    fn parse_primary_expression(&mut self, parser: &mut Parser) {

        loop {

            let token = self.get_token(parser);
            let token_type = token.get_type();

            match token_type {
                TokenType::Int => {
                    parser.consume();
                    let expr_id = parser.ast.new_int_expression(token.get_value().parse().unwrap());
                    self.add_to_output(expr_id);
                    break;
                }
                TokenType::TrueKeyword | TokenType::FalseKeyword => {
                    parser.consume();
                    let expr_id = parser.ast.new_bool_expression(token.get_value().parse().unwrap());
                    self.add_to_output(expr_id);
                    break;
                }
                TokenType::Identifier => {
                    parser.consume();
                    let expr_id = parser.ast.new_variable_expression(token.clone());
                    self.add_to_output(expr_id);
                    break;
                }
                TokenType::OpenParenthese => {
                    self.parenthese_index.push(ParentheseIndex::new(token.get_text_pos().clone()));
                    parser.consume();
                }
                _ => {
                    if let Some(operator) = Operator::unary_operator(token_type) {
                        parser.consume();
                        self.add_operator(operator, token.get_text_pos().clone());
                    }
                    else if Operator::binary_operator(token_type).is_some() {
                        parser.consume();
                        parser.diagnostics.borrow_mut().missing_operand_error(token.get_text_pos());
                    }
                    else {
                        break;
                    }
                }
            }
        }
    }

    fn parse_rest(&mut self, parser: &mut Parser) -> bool {
        loop {
            
            let token = self.get_token(parser);
            let token_type = token.get_type();

            match token_type {
                
                TokenType::Unknown => {
                    parser.consume();
                    parser.diagnostics.borrow_mut().unknown_token(token.get_text_pos());
                    return true;
                },
                TokenType::ClosedParenthese => {
                    parser.consume();
                    if let Some(prenthese_index) = self.parenthese_index.last() {
                        self.eval_parenthized_expression(prenthese_index.counter.get(), parser);
                        self.parenthese_index.pop();
                    }
                    else {
                        parser.diagnostics.borrow_mut().missmatched_parens_error(token.get_text_pos());
                    }
                }
                TokenType::Semicolon | TokenType::Eof => break,
                _ => {
                    if let Some(operator) = Operator::binary_operator(&token_type) {
                        self.eval_while_lower_precedence(&operator, parser);
                        parser.consume();
                        self.add_operator(operator, token.get_text_pos().clone());
                        break;
                    }
                    else {
                        return true;
                    }  
                }
            }
        }

        false
    }

    fn get_token(&mut self, parser: &mut Parser) -> Token {
        let token = parser.current().unwrap().clone();
        let token_type = token.get_type();
        
        if *token_type != TokenType::Eof {
            self.last_token = Some(token.clone());
        }

        token
    }

    fn eval_while_lower_precedence(&mut self, op: &Operator, parser: &mut Parser) {
        while let Some(othr_op) = self.operators_stack.last() {

            if self.parenthese_index.last().is_some_and(|exp_len| exp_len.counter.get() == 0) {
                break;
            }

            if op.precedence() > othr_op.precedence() {
                break;
            }
            
            
            let operator = self.operators_stack.pop().unwrap();
            let pos = self.operators_pos_stack.pop().unwrap();
            self.output_operator(operator, pos, parser);
            if let Some(expr_len) = self.parenthese_index.last() {
                expr_len.counter.decrement();
            }
        }
    }

    fn output_operator(&mut self, op: Operator, text_pos: TextPosition, parser: &mut Parser) {
        match op.get_type() {
                OperatorType::Binary => {
                let right = self.get_stack_value(parser);
                let left = self.get_stack_value(parser);
                
                match op {
                    Operator::Assignement => self.add_to_output(parser.ast.new_assignement_expression(left, right, text_pos)),
                    _ => self.add_to_output(parser.ast.new_binary_operator_expression(op, left, right, text_pos))
                }
            }
            OperatorType::Unary => {
                let value = self.get_stack_value(parser);
                self.add_to_output(parser.ast.new_unary_opertor_expression(op, value, text_pos));
            }
    }
    }

    fn eval_parenthized_expression(&mut self, mut index: u32, parser: &mut Parser) {
        while index > 0 {
            let operator = self.operators_stack.pop().unwrap();
            let pos = self.operators_pos_stack.pop().unwrap();
            self.output_operator(operator, pos, parser);
            index -= 1;
        }
    }

    fn get_stack_value(&mut self, parser: &mut Parser) -> ExpressionId {
        self.output_stack.pop().unwrap_or_else(|| {
            parser.diagnostics.borrow_mut().missing_operand_error(self.last_token.as_ref().unwrap().get_text_pos());
            parser.ast.new_incorrect_expression()
        })
    }

    fn add_to_output(&mut self, expr: ExpressionId) {
        self.output_stack.push(expr);
    }

    fn add_operator(&mut self, operator: Operator, text_pos: TextPosition) {
        self.operators_stack.push(operator);
        self.operators_pos_stack.push(text_pos);
        self.increment_paren_index();
    }

    fn increment_paren_index(&mut self) {
        if let Some(paren_index) = self.parenthese_index.last() {
            paren_index.counter.increment();
        }
    }
}