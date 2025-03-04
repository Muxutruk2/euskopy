#![warn(clippy::pedantic, clippy::nursery, clippy::cargo)]
#![allow(clippy::multiple_crate_versions)]
use env_logger::Env;
use log::debug;
use std::collections::HashMap;
use std::env;
use std::fmt;
use std::fs;
use std::fs::File;
use std::io::{self, Read, Write};

struct FunctionHandler<'a> {
    builtins: HashMap<&'a str, String>,
}

impl FunctionHandler<'_> {
    fn new() -> Self {
        let mut dictionary: HashMap<&str, String> = HashMap::new();
        dictionary.insert("inprimatu", "print".into());
        Self {
            builtins: dictionary,
        }
    }

    fn translate(&self, input: &str) -> String {
        self.builtins
            .get(input)
            .map_or_else(|| input.into(), Into::into)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Ident(String),
    Number(f64),

    Plus,
    Minus,
    Star,
    Slash,
    Modulus,

    Eq,
    Neq,
    Gt,
    Lt,
    Gte,
    Lte,

    Eta,
    Edo,
    Ez,

    Dela,
    Ezarri,
    Bada,
    Bestela,
    Bakoitzeko,
    Artean,

    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Colon,
    Semicolon,

    EOF,
}

struct Lexer {
    input: Vec<char>,
    position: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            position: 0,
        }
    }

    fn next_token(&mut self) -> Result<Token, String> {
        self.skip_whitespace();
        match self.input.get(self.position) {
            Some(&ch) => {
                let token = match ch {
                    '+' => Token::Plus,
                    '-' => Token::Minus,
                    '*' => Token::Star,
                    '/' => Token::Slash,
                    '%' => Token::Modulus,
                    '=' => {
                        if self.peek_char() == '=' {
                            self.position += 1;
                            Token::Eq
                        } else {
                            panic!("Assignment operator is: [VAR] [VALUE] dela ezarri")
                        }
                    }
                    '>' => {
                        if self.peek_char() == '=' {
                            self.position += 1;
                            Token::Gte
                        } else {
                            Token::Gt
                        }
                    }
                    '<' => {
                        if self.peek_char() == '=' {
                            self.position += 1;
                            Token::Lte
                        } else {
                            Token::Lt
                        }
                    }
                    '!' => {
                        if self.peek_char() == '=' {
                            self.position += 1;
                            Token::Neq
                        } else {
                            Token::Ez
                        }
                    }
                    '(' => Token::LParen,
                    ')' => Token::RParen,
                    '{' => Token::LBrace,
                    '}' => Token::RBrace,
                    ',' => Token::Comma,
                    ':' => Token::Colon,
                    ';' => Token::Semicolon,
                    c if c.is_alphabetic() => return Ok(self.read_identifier()),
                    c if c.is_ascii_digit() => return Ok(self.read_number()),
                    _ => {
                        return Err(format!("UNRECOGNIZED TOKEN {} in {}", ch, self.position));
                    }
                };
                self.position += 1;
                Ok(token)
            }
            None => Ok(Token::EOF),
        }
    }

    fn peek_char(&self) -> char {
        if self.position + 1 >= self.input.len() {
            '\0'
        } else {
            self.input[self.position + 1]
        }
    }

    fn skip_whitespace(&mut self) {
        while self.position < self.input.len() && self.input[self.position].is_whitespace() {
            self.position += 1;
        }
    }

    fn read_identifier(&mut self) -> Token {
        let start = self.position;
        while self.position < self.input.len() && self.input[self.position].is_alphanumeric() {
            self.position += 1;
        }

        let identifier: String = self.input[start..self.position].iter().collect();

        match identifier.as_str() {
            "bada" => Token::Bada,
            "dela" => Token::Dela,
            "ezarri" => Token::Ezarri,
            "bestela" => Token::Bestela,
            "bakoitzeko" => Token::Bakoitzeko,
            "artean" => Token::Artean,
            "eta" => Token::Eta,
            "edo" => Token::Edo,
            "ez" => Token::Ez,
            _ => Token::Ident(identifier),
        }
    }
    #[allow(clippy::redundant_closure)]
    fn read_number(&mut self) -> Token {
        let start = self.position;
        while self.position < self.input.len()
            && (self.input[self.position].is_ascii_digit() || self.input[self.position] == '.')
        {
            self.position += 1;
        }

        let number: String = self.input[start..self.position].iter().collect();
        number.parse::<f64>().map_or_else(
            |_| panic!("Invalid number format: {number}"),
            |num| Token::Number(num),
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Expr {
    Ident(String),
    Number(f64),
    BinaryOp(Box<Expr>, Operator, Box<Expr>),
    FunctionCall(String, Vec<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulus,
    GreaterThan,
    LessThan,
    GreaterEqual,
    LessEqual,
    Equals,
    NotEquals,
}
impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Plus => {
                write!(f, "+")
            }
            Self::Minus => {
                write!(f, "-")
            }
            Self::Multiply => {
                write!(f, "*")
            }
            Self::Divide => {
                write!(f, "/")
            }
            Self::Modulus => {
                write!(f, "%")
            }
            Self::GreaterThan => {
                write!(f, ">")
            }
            Self::LessThan => {
                write!(f, "<")
            }
            Self::GreaterEqual => {
                write!(f, ">=")
            }
            Self::LessEqual => {
                write!(f, "<=")
            }
            Self::Equals => {
                write!(f, "==")
            }
            Self::NotEquals => {
                write!(f, "!=")
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Stmt {
    Assign(String, Expr),
    If(Expr, Vec<Stmt>, Option<Vec<Stmt>>),
    Loop(String, Expr, Expr, Vec<Stmt>),
    Expr(Expr),
}

impl Program {
    fn to_python(&self) -> String {
        self.statements
            .iter()
            .map(|stmt| stmt.to_python(0))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

impl Stmt {
    fn to_python(&self, indent: usize) -> String {
        let indentation = "    ".repeat(indent);
        match self {
            Self::Assign(var, expr) => format!("{indentation}{var} = {}", expr.to_python()),
            Self::Expr(expr) => format!("{indentation}{}", expr.to_python()),
            Self::Loop(var, start, end, body) => {
                let body_code = body
                    .iter()
                    .map(|stmt| stmt.to_python(indent + 1))
                    .collect::<Vec<_>>()
                    .join("\n");
                format!(
                    "{indentation}for {var} in range({}, {}):\n{}",
                    start.to_python(),
                    end.to_python(),
                    body_code
                )
            }
            Self::If(condition, then_branch, else_branch) => {
                let then_code = then_branch
                    .iter()
                    .map(|stmt| stmt.to_python(indent + 1))
                    .collect::<Vec<_>>()
                    .join("\n");
                let else_code = else_branch
                    .as_ref()
                    .map(|else_branch| {
                        let else_body = else_branch
                            .iter()
                            .map(|stmt| stmt.to_python(indent + 1))
                            .collect::<Vec<_>>()
                            .join("\n");
                        format!("\n{indentation}else:\n{else_body}")
                    })
                    .unwrap_or_default();

                format!(
                    "{indentation}if {}:\n{}{}",
                    condition.to_python(),
                    then_code,
                    else_code
                )
            }
        }
    }
}

impl Expr {
    fn to_python(&self) -> String {
        match self {
            Self::Ident(var) => var.clone(),
            Self::Number(num) => num.to_string(),
            Self::BinaryOp(lhs, op, rhs) => {
                format!("{} {} {}", lhs.to_python(), op.to_python(), rhs.to_python())
            }
            Self::FunctionCall(name, args) => {
                let args_str = args
                    .iter()
                    .map(Self::to_python)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{name}({args_str})")
            }
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Ident(s) => write!(f, "{s}"),
            Self::Number(n) => write!(f, "{n}"),
            Self::BinaryOp(left, op, right) => {
                write!(f, "({left} {op} {right})")
            }
            Self::FunctionCall(name, args) => {
                let args_str: Vec<String> = args.iter().map(|arg| format!("{arg}")).collect();
                write!(f, "{}({})", name, args_str.join(", "))
            }
        }
    }
}

impl Operator {
    fn to_python(&self) -> String {
        match self {
            Self::Plus => "+".to_string(),
            Self::Minus => "-".to_string(),
            Self::Multiply => "*".to_string(),
            Self::Divide => "/".to_string(),
            Self::Modulus => "%".to_string(),
            Self::GreaterThan => ">".to_string(),
            Self::LessThan => "<".to_string(),
            Self::GreaterEqual => ">=".to_string(),
            Self::LessEqual => "<=".to_string(),
            Self::Equals => "==".to_string(),
            Self::NotEquals => "!=".to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Program {
    statements: Vec<Stmt>,
}

struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    const fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    fn parse(&mut self) -> Program {
        debug!("Parsing...");
        let mut statements = Vec::new();
        while self.position < self.tokens.len() {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
        }
        Program { statements }
    }

    fn parse_statement(&mut self) -> Option<Stmt> {
        let function_handler = FunctionHandler::new();
        match self.peek().cloned() {
            Some(Token::Ident(var)) if self.lookahead_is(&Token::Bakoitzeko) => {
                self.advance();
                self.expect(&Token::Bakoitzeko);

                let start = match self.advance().cloned() {
                    Some(Token::Number(start)) => Expr::Number(start),
                    Some(Token::Ident(start)) => Expr::Ident(start),
                    _ => {
                        panic!("Here1");
                    }
                };

                self.expect(&Token::Eta);

                let end = match self.advance().cloned() {
                    Some(Token::Number(end)) => Expr::Number(end),
                    Some(Token::Ident(end)) => Expr::Ident(end),
                    _ => {
                        panic!("Here1");
                    }
                };

                self.expect(&Token::Artean);
                self.expect(&Token::LBrace);
                let body = self.parse_block();

                Some(Stmt::Loop(var, start, end, body))
            }
            Some(Token::Ident(var)) => {
                debug!("Found token {:?}", &var);
                if self.token_ahead_is(3, &Token::Bada) || self.token_ahead_is(5, &Token::Bada) {
                    debug!(
                        "3 or 5 more tokens after {:?}, found 'Bada', If statement",
                        &var
                    );
                    let condition = self.parse_expression()?;
                    debug!(
                        "Condition of the If Statement where {:?} is: {:?}",
                        &var, &condition
                    );
                    self.expect(&Token::Bada);
                    self.expect(&Token::LBrace);
                    let then_branch = self.parse_block();

                    debug!(
                        "Branch of the If Statement where {:?} is: {:?}",
                        &var, &then_branch
                    );

                    let else_branch = if self.peek() == Some(&Token::Bestela) {
                        self.advance();
                        self.expect(&Token::LBrace);
                        Some(self.parse_block())
                    } else {
                        None
                    };
                    debug!(
                        "Else Branch of the If Statement where {:?} is: {:?}",
                        &var, &else_branch
                    );

                    Some(Stmt::If(condition, then_branch, else_branch))
                } else {
                    debug!("Found {:?}, without 'Bada', Assignment", &var);
                    self.advance()?;
                    let value = self.parse_expression()?;
                    debug!("{:?} will be set to {:?}", &var, &value);
                    self.expect(&Token::Dela);
                    self.expect(&Token::Ezarri);
                    Some(Stmt::Assign(var, value))
                }
            }

            Some(Token::LParen) => {
                self.advance();
                if let Some(Token::Ident(arg)) = self.advance().cloned() {
                    self.expect(&Token::RParen);
                    if let Some(Token::Ident(func)) = self.advance().cloned() {
                        let real_func = function_handler.translate(&func);
                        Some(Stmt::Expr(Expr::FunctionCall(
                            real_func,
                            vec![Expr::Ident(arg)],
                        )))
                    } else {
                        debug!("E");
                        None
                    }
                } else {
                    debug!("F");
                    None
                }
            }
            Some(Token::Semicolon) => {
                self.advance();
                None
            }
            _ => {
                debug!("G: {:?}", self.peek().unwrap());
                self.advance();
                None
            }
        }
    }
    fn parse_expression(&mut self) -> Option<Expr> {
        debug!("Parsing expression");
        let mut left = match self.advance()? {
            Token::Number(n) => Expr::Number(*n),
            Token::Ident(var) => Expr::Ident(var.clone()),
            _ => {
                debug!(
                    "Token of left expression is not a Number or a Variable: {:?}",
                    self.peek()?
                );
                return None;
            }
        };

        while let Some(op_token) = self.peek().cloned() {
            if let Some(op) = token_to_operator(&op_token) {
                self.advance();
                let right = match self.advance()? {
                    Token::Number(n) => Expr::Number(*n),
                    Token::Ident(var) => Expr::Ident(var.clone()),
                    _ => return None,
                };

                left = Expr::BinaryOp(Box::new(left), op, Box::new(right));
            } else {
                break;
            }
        }

        Some(left)
    }

    fn parse_block(&mut self) -> Vec<Stmt> {
        let mut statements = Vec::new();
        while let Some(token) = self.peek() {
            if token == &Token::RBrace {
                self.advance();
                break;
            }
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
        }
        statements
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    fn lookahead_is(&self, token: &Token) -> bool {
        self.tokens.get(self.position + 1) == Some(token)
    }

    fn token_ahead_is(&self, num: usize, token: &Token) -> bool {
        self.tokens.get(self.position + num) == Some(token)
    }

    fn advance(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.position);
        self.position += 1;
        token
    }

    fn expect(&mut self, expected: &Token) {
        if let Some(token) = self.advance() {
            assert!(
                (token == expected),
                "Expected {expected:?}, found {token:?} "
            );
        } else {
            panic!("Unexpected end of input, expected {expected:?}",);
        }
    }
}

const fn token_to_operator(token: &Token) -> Option<Operator> {
    match token {
        Token::Plus => Some(Operator::Plus),
        Token::Minus => Some(Operator::Minus),
        Token::Star => Some(Operator::Multiply),
        Token::Slash => Some(Operator::Divide),
        Token::Modulus => Some(Operator::Modulus),
        Token::Gt => Some(Operator::GreaterThan),
        Token::Lt => Some(Operator::LessThan),
        Token::Gte => Some(Operator::GreaterEqual),
        Token::Lte => Some(Operator::LessEqual),
        Token::Eq => Some(Operator::Equals),
        Token::Neq => Some(Operator::NotEquals),
        _ => None,
    }
}
fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::Builder::from_env(Env::default().default_filter_or("warn")).init();

    let args: Vec<String> = env::args().collect();
    let (input_file, output_file) = match args.len() {
        1 => (None, None),
        2 => (Some(&args[1]), None),
        4 => (Some(&args[1]), Some(&args[3])),
        _ => return Err("Usage: ./euskopy [INPUT] [-o OUTPUT]".into()),
    };

    let source = if let Some(input_file) = input_file {
        fs::read_to_string(input_file)
            .map_err(|e| format!("Failed to read file {input_file}: {e}"))?
    } else {
        let mut input = String::new();
        io::stdin().read_to_string(&mut input)?;
        input
    };

    let mut lexer = Lexer::new(&source);
    let mut tokens = Vec::new();

    while let Ok(token) = lexer.next_token() {
        if token == Token::EOF {
            break;
        }
        tokens.push(token);
    }

    let mut parser = Parser::new(tokens);
    let parsed = parser.parse();
    let python = parsed.to_python();

    println!("{python}");

    if let Some(output_file) = output_file {
        File::create(output_file)?.write_all(python.as_bytes())?;
    }

    Ok(())
}
