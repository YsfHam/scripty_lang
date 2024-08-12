use std::{fmt::Display, iter::Peekable, str::Chars};

use crate::utils::Counter;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    // Litteral values
    Int,
    Identifier,

    // Operators,
    Plus,
    Minus,
    Asterisk,
    Slash,
    OpenParenthese,
    ClosedParenthese,
    Pipe,
    Hat,
    Ampersand,
    Percent,
    Tilde,
    Semicolon,
    Equals,
    DoubleAmpersand,
    DoublePipe,
    Bang,

    // Keywords
    LetKeyword,
    TrueKeyword,
    FalseKeyword,

    // Control
    Eof,

    Unknown,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Int => write!(f, "Int"),
            TokenType::Identifier => write!(f, "Identifier"),
            TokenType::Plus => write!(f, "+"),
            TokenType::Minus => write!(f, "-"),
            TokenType::Asterisk => write!(f, "*"),
            TokenType::Slash => write!(f, "/"),
            TokenType::OpenParenthese => write!(f, "("),
            TokenType::ClosedParenthese => write!(f, ")"),
            TokenType::Pipe => write!(f, "|"),
            TokenType::Hat => write!(f, "^"),
            TokenType::Ampersand => write!(f, "&"),
            TokenType::Percent => write!(f, "%"),
            TokenType::Tilde => write!(f, "~"),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::Eof => write!(f, "EOF"),
            TokenType::Equals => write!(f, "="),

            TokenType::LetKeyword => write!(f, "let"),
            TokenType::TrueKeyword => write!(f, "true"),
            TokenType::FalseKeyword => write!(f, "false"),

            TokenType::Unknown => write!(f, "Unknown"),

            TokenType::DoubleAmpersand => write!(f, "&&"),
            TokenType::DoublePipe => write!(f, "||"),
            TokenType::Bang => write!(f, "!"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TextPosition {
    pub line: u32,
    pub col_start: u32,
    pub col_end: u32,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    ty: TokenType,
    value: String,
    text_pos: TextPosition
}

impl Token {
    pub fn new(token_type: TokenType, value: String, text_pos: TextPosition) -> Self {
        Self {
            ty: token_type,
            value,
            text_pos,
        }
    }

    pub fn get_type(&self) -> &TokenType {
        &self.ty
    }

    pub fn get_text_pos(&self) -> &TextPosition {
        &self.text_pos
    }

    pub fn get_value(&self) -> &String {
        &self.value
    }

}
struct LexerCursor {
    line: Counter,
    relative_curr_pos: Counter,
}

impl LexerCursor {
    fn new() -> Self {
        Self {
            line: Counter::new(1),
            relative_curr_pos: Counter::new(1),
        }
    }

    fn increment(&self) {
        self.relative_curr_pos.increment();
    }

    fn new_line(&self) {
        self.line.increment();
        self.relative_curr_pos.set(1);
    }
}


static IDENTIFIERS_SYMBOLS: &str = "abcdefghijklmnopqrstuvwqxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_";

fn is_identifier_symbole(c: char) -> bool {
    IDENTIFIERS_SYMBOLS.chars().any(|sym| sym == c)
}

static OPERATOR_SYMBOLS: &[(&str, TokenType)] = &[
    ("+", TokenType::Plus),
    ("-", TokenType::Minus),
    ("*", TokenType::Asterisk),
    ("/", TokenType::Slash),
    ("(", TokenType::OpenParenthese),
    (")", TokenType::ClosedParenthese),
    ("|", TokenType::Pipe),
    ("^", TokenType::Hat),
    ("&", TokenType::Ampersand),
    ("%", TokenType::Percent),
    ("~", TokenType::Tilde),
    (";", TokenType::Semicolon),
    ("=", TokenType::Equals),
    ("&&", TokenType::DoubleAmpersand),
    ("||", TokenType::DoublePipe),
    ("!", TokenType::Bang),
];

fn is_operator_symbole(sym: &str) -> bool {
    operator_symbole_to_token_type(sym).is_some()
}

fn operator_symbole_to_token_type(sym: &str) -> Option<TokenType> {
    OPERATOR_SYMBOLS.iter()
    .find(|punc| sym == (**punc).0)
    .map(|punc| (*punc).1.clone())
}

static KEYWORDS: &[(&str, TokenType)] = &[
    ("let", TokenType::LetKeyword),
    ("true", TokenType::TrueKeyword),
    ("false", TokenType::FalseKeyword),
];

fn get_keyword_token_type(identifier: &str) -> Option<TokenType> {
    KEYWORDS.iter()
        .find(|keyword| identifier == keyword.0)
        .map(|keyword| keyword.1)
}

pub struct Lexer<'a> {
    text_it: Option<Peekable<Chars<'a>>>,
    cursor: LexerCursor,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            text_it: Some(text.chars().peekable()),
            cursor: LexerCursor::new(),
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {

        if self.text_it.is_none() {
            return None;
        }

        self.skip_whitespaces();
        let next_char_op = self.current();

        if next_char_op.is_none() {
            self.text_it = None;
            return Some(Token::new(
                TokenType::Eof,
                String::from(""),
                TextPosition {
                    line: self.cursor.line.get() ,
                    col_start: self.cursor.relative_curr_pos.get(),
                    col_end: self.cursor.relative_curr_pos.get(),
                }
            ))
        }
        let next_char = *next_char_op.unwrap();
        let col_start = self.cursor.relative_curr_pos.get();
        let (token_type, value) = self.get_next_token_type(next_char);
        let col_end = self.cursor.relative_curr_pos.get();

        Some(Token::new(
            token_type,
            value,
            TextPosition {
                col_start,
                col_end,
                line: self.cursor.line.get(),
            }
        ))

    }

    fn get_next_token_type(&mut self, next_char: char) -> (TokenType, String) {
        if next_char.is_ascii_digit() {
            self.next_int()
        }
        else if is_identifier_symbole(next_char) {
            self.next_identifier()
        }
        else {
            self.next_operator_symbole()
        }
    }

    fn next_operator_symbole(&mut self) -> (TokenType, String) {
        let mut buffer = String::new();
        let mut token_text = buffer.clone();
        while let Some(&sym) = self.current() {
            if sym.is_whitespace() {
                break;
            }
            buffer.push(sym);
            if is_operator_symbole(&buffer) {
                token_text = buffer.clone();
                self.consume();
            }
            else if buffer.len() < 2 {
                self.consume();
            }
            else {
                return (TokenType::Unknown, buffer);
            }
        }
        let token_type = operator_symbole_to_token_type(&token_text);

        (token_type.unwrap(), token_text)
    }

    fn next_int(&mut self) -> (TokenType, String) {

        (TokenType::Int, self.take_while(|c| c.is_ascii_digit()))
    }

    fn next_identifier(&mut self) -> (TokenType, String) {
        let identifier = self.take_while(|c| is_identifier_symbole(c));
        let token_type = get_keyword_token_type(&identifier).unwrap_or(TokenType::Identifier);

        (token_type, identifier)
        
    }

    fn take_while(&mut self, cond: impl Fn(char) -> bool) -> String {
        let mut buffer = String::new();

        while let Some(c) = self.current() {
            if cond(*c) {
                buffer.push(self.consume());
            }
            else {
                break;
            }
        }

        buffer
    }

    fn consume(&mut self) -> char {
        self.cursor.increment();
        self.get_iter().next().unwrap()
    }

    fn current(&mut self) -> Option<&char> {
        self.get_iter().peek()
    }

    fn get_iter(&mut self) -> &mut Peekable<Chars<'a>> {
        self.text_it.as_mut().unwrap()
    }

    fn skip_whitespaces(&mut self) {

        while let Some(c) = self.current() {
            if c.is_whitespace() {
                if *c == '\n' {
                    self.cursor.new_line();
                    self.get_iter().next();
                }
                else {
                    self.consume();
                }
            }
            else {
                break;
            }
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;
    
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
    
}