use crate::tokenizer::{TokenType, Token, SourceLocation};

#[derive(Debug, PartialEq, Eq)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Exponent,
    Not,
    Equals,
    NotEquals,
    And,
    Or,
    Assign,
}

impl Op {
    fn from_str(value: &str) -> Op {
        match value {
            "+" => Op::Add,
            "-" => Op::Subtract,
            "*" => Op::Multiply,
            "/" => Op::Divide,
            "%" => Op::Modulo,
            "**" => Op::Exponent,
            "not" => Op::Not,
            "==" => Op::Equals,
            "!=" => Op::NotEquals,
            "and" => Op::And,
            "or" => Op::Or,
            "=" => Op::Assign,
            _ => panic!("Unknown operator {:?}", value)
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    IntegerLiteral {
        value: i32,
    },
    BooleanLiteral {
        value: bool,
    },
    Identifier {
        value: String,
    },
    BinaryExpression {
        left: Box<Expression>,
        operator: Op,
        right: Box<Expression>,
    },
    UnaryExpression {
        operand: Box<Expression>,
        operator: Op,
    },
    IfExpression {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Option<Box<Expression>>,
    },
    CallExpression {
        callee: Box<Expression>,
        arguments: Vec<Box<Expression>>
    },
}

struct Parser {
    tokens: Vec<Token>,
    current_index: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            current_index: 0,
        }
    }

    fn peek(&mut self) -> Token {
        self.peek_forward(0)
    }

    fn peek_forward(&mut self, lookahead: usize) -> Token {
        let idx = self.current_index + lookahead;
        if idx < self.tokens.len() {
            if let Some(t) = self.tokens.get(idx) {
                t.clone()
            } else {
                panic!("Unexpected end of file");
            }
        } else {
            Token {
                token_type: TokenType::None,
                value: "".to_string(),
                location: SourceLocation {
                    line: 0,
                    column: 0,
                },
            }
        }
    }

    fn current_is(&mut self, value: &str) -> bool {
        self.peek().value == value
    }

    fn next_is(&mut self, value: &str) -> bool {
        self.peek_forward(1).value == value
    }

    fn consume_with_values(&mut self, expected_type: TokenType, expected_values: &[String]) -> Token {
        let token: Token = self.peek();

        if expected_type == token.token_type && (expected_values.is_empty() || expected_values.contains(&token.value)) {
            self.current_index += 1;
            token
        } else {
            panic!("Unexpected token: {:?}", token);
        }
    }

    fn consume(&mut self, expected_type: TokenType) -> Token {
        self.consume_with_values(expected_type, &[])
    }

    fn consume_with_value(&mut self, expected_type: TokenType, value: &str) -> Token {
        self.consume_with_values(expected_type, &[value.to_string()])
    }

    fn consume_left_paren(&mut self) {
        self.consume_with_value(TokenType::Punctuation, "(");
    }

    fn consume_right_paren(&mut self) {
        self.consume_with_value(TokenType::Punctuation, ")");
    }

    fn consume_comma(&mut self) {
        self.consume_with_value(TokenType::Punctuation, ",");
    }

    fn consume_keyword(&mut self, value: &str) {
        self.consume_with_value(TokenType::Keyword, value);
    }

    fn parse_boolean_literal(&mut self) -> Expression {
        let token = self.consume_with_values(TokenType::BooleanLiteral, &["true".to_string(), "false".to_string()]);
        Expression::BooleanLiteral { value: token.value.starts_with('t') }
    }

    fn parse_int_literal(&mut self) -> Expression {
        let token = self.consume(TokenType::IntegerLiteral);
        Expression::IntegerLiteral {
            value: token.value.parse().expect("Not a valid number"),
        }
    }

    fn parse_identifier(&mut self) -> Expression {
        let token = self.consume(TokenType::Identifier);
        Expression::Identifier {
            value: token.value,
        }
    }

    fn parse_call_expression(&mut self) -> Expression {
        let callee = self.parse_identifier();
        self.consume_left_paren();

        let mut arguments: Vec<Box<Expression>> = vec![];
        if !self.current_is(")") {
            loop {
                let arg = self.parse_expression();
                arguments.push(Box::new(arg));
                if self.current_is(")") {
                    break;
                }
                self.consume_comma();
            }
        }
        self.consume_right_paren();

        Expression::CallExpression { callee: Box::new(callee), arguments }
    }

    fn parse_if_expression(&mut self) -> Expression {
        self.consume_keyword("if");
        let condition = self.parse_expression();
        self.consume_keyword("then");
        let then_branch = self.parse_expression();

        if self.current_is("else") {
            self.consume_keyword("else");
            let else_branch = self.parse_expression();
            Expression::IfExpression {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Some(Box::new(else_branch)),
            }
        } else {
            Expression::IfExpression {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: None,
            }
        }
    }

    fn parse_parentheses(&mut self) -> Expression {
        self.consume_left_paren();
        let expr = self.parse_expression();
        self.consume_right_paren();
        expr
    }

    fn parse_unary_expression(&mut self) -> Expression {
        let op = self.consume(TokenType::Operator);
        let expr = self.parse_factor();
        Expression::UnaryExpression { operand: Box::new(expr), operator: Op::from_str(&op.value) }
    }

    fn parse_factor(&mut self) -> Expression {
        let token = self.peek();

        match token.token_type {
            TokenType::Operator => self.parse_unary_expression(),
            TokenType::IntegerLiteral => self.parse_int_literal(),
            TokenType::BooleanLiteral => self.parse_boolean_literal(),
            TokenType::Keyword => self.parse_if_expression(),
            TokenType::Identifier => {
                if self.next_is("(") {
                    self.parse_call_expression()
                } else {
                    self.parse_identifier()
                }
            },
            TokenType::Punctuation => {
                if token.value == "(" {
                    self.parse_parentheses()
                } else {
                    panic!("Unexpected token: {:?}", token);
                }
            },
            TokenType::None => panic!("Unexpected end of file"),
            // _ => panic!("Unexpected token")
        }
    }

    fn parse_term(&mut self) -> Expression {
        let mut left = self.parse_factor();
        
        while ["*", "/", "and"].contains(&self.peek().value.as_str()) {
            let operator = self.consume(TokenType::Operator);
            let right = self.parse_factor();
            left = Expression::BinaryExpression {
                left: Box::new(left),
                operator: Op::from_str(&operator.value),
                right: Box::new(right),
            };
        }

        left
    }

    fn parse_expression(&mut self) -> Expression {
        let mut left = self.parse_term();
        
        while ["+", "-", "or"].contains(&self.peek().value.as_str()) {
            let operator = self.consume(TokenType::Operator);
            let right = self.parse_term();
            left = Expression::BinaryExpression {
                left: Box::new(left),
                operator: Op::from_str(&operator.value),
                right: Box::new(right),
            };
        }

        left
    }

}

pub fn parse(tokens: Vec<Token>) -> Expression {
    let mut token_list = Parser::new(tokens);
    let expr = token_list.parse_expression();
    if token_list.current_index < token_list.tokens.len() {
        panic!("Unexpected token: {:?}", token_list.peek());
    }
    expr
}

#[cfg(test)]
mod tests {

    use crate::tokenizer::{tokenize, Token};
    use super::*;

    #[test]
    #[should_panic(expected = "Unexpected end of file")]
    fn test_empty_expression() {
        let source = "";
        let tokens: Vec<Token> = tokenize(source);
        parse(tokens);
    }

    #[test]
    #[should_panic(expected = "Unexpected token: Token { token_type: IntegerLiteral, value: \"3\", location: SourceLocation { line: 1, column: 7 } }")]
    fn test_invalid_source() {
        let source = "1 + 2 3 4 haha minttuglitch";
        let tokens: Vec<Token> = tokenize(source);
        parse(tokens);
    }

    #[test]
    fn test_parse_unary_op() {
        let source = "-1";
        let tokens: Vec<Token> = tokenize(source);

        let expression = parse(tokens);
        
        match expression {
            Expression::UnaryExpression { operator, .. } => {
                assert_eq!(operator, Op::Subtract);
            },
            _ => panic!("Expected unary expression"),
        }
    }

    #[test]
    fn test_parse_binary_op() {
        let source = "1 + pepejam";
        let tokens: Vec<Token> = tokenize(source);

        let expression = parse(tokens);
        
        match expression {
            Expression::BinaryExpression { operator, .. } => {
                assert_eq!(operator, Op::Add);
            },
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_associative_binary_op() {
        let source = "minttujam - 2 + 3 - 4";
        let tokens: Vec<Token> = tokenize(source);

        let expression = parse(tokens);
        
        match expression {
            Expression::BinaryExpression { operator, left ,.. } => {
                assert_eq!(operator, Op::Subtract);
                match left.as_ref() {
                    Expression::BinaryExpression { operator, left, .. } => {
                        assert_eq!(*operator, Op::Add);
                        match left.as_ref() {
                            Expression::BinaryExpression { operator, left, .. } => {
                                assert_eq!(*operator, Op::Subtract);
                                match left.as_ref() {
                                    Expression::Identifier { value } => {
                                        assert_eq!(value, "minttujam");
                                    },
                                    _ => panic!("Expected literal minttujam"),
                                }
                            },
                            _ => panic!("Expected - binary expression"),
                        }
                    },
                    _ => panic!("Expected + binary expression"),
                }
            },
            _ => panic!("Expected - binary expression"),
        }
    }

    #[test]
    fn test_precedence_binary_op() {
        let source = "1 + 2 * PI - 4";
        let tokens: Vec<Token> = tokenize(source);

        let expression = parse(tokens);
        
        match expression {
            Expression::BinaryExpression { operator, left, .. } => {
                assert_eq!(operator, Op::Subtract);
                match left.as_ref() {
                    Expression::BinaryExpression { operator, left, right } => {
                        assert_eq!(*operator, Op::Add);
                        match left.as_ref() {
                            Expression::IntegerLiteral { value, .. } => {
                                assert_eq!(*value, 1);
                                match right.as_ref() {
                                    Expression::BinaryExpression { operator, left, .. }  => {
                                        assert_eq!(*operator, Op::Multiply);
                                        match left.as_ref() {
                                            Expression::IntegerLiteral { value } => {
                                                assert_eq!(*value, 2);
                                            },
                                            _ => panic!("Expected literal 2"),
                                        }
                                    },
                                    _ => panic!("Expected * binary expression"),
                                }
                            },
                            _ => panic!("Expected literal 1"),
                        }
                    },
                    _ => panic!("Expected + binary expression"),
                }
            },
            _ => panic!("Expected - binary expression"),
        }
    }

    #[test]
    fn test_parentheses() {
        let source = "(1 + 2) * 3";
        let tokens: Vec<Token> = tokenize(source);

        let expression = parse(tokens);
        
        match expression {
            Expression::BinaryExpression { operator, left, .. } => {
                assert_eq!(operator, Op::Multiply);
                match left.as_ref() {
                    Expression::BinaryExpression { operator, left,.. } => {
                        assert_eq!(*operator, Op::Add);
                        match left.as_ref() {
                            Expression::IntegerLiteral { value } => {
                                assert_eq!(*value, 1);
                            },
                            _ => panic!("Expected literal 1"),
                        }
                    },
                    _ => panic!("Expected + binary expression"),
                }
            },
            _ => panic!("Expected * binary expression"),
        }
    }

    #[test]
    fn test_parse_int_literal() {
        let source = "42";
        let tokens: Vec<Token> = tokenize(source);

        let expression = parse(tokens);
        
        match expression {
            Expression::IntegerLiteral { value, .. } => {
                assert_eq!(value, 42);
            },
            _ => panic!("Expected literal"),
        }
    }

    #[test]
    fn test_parse_identifier() {
        let source = "identifieeeer";
        let tokens: Vec<Token> = tokenize(source);

        let expression = parse(tokens);
        
        match expression {
            Expression::Identifier { value, .. } => {
                assert_eq!(value, "identifieeeer");
            },
            _ => panic!("Expected identifier"),
        }
    }

    #[test]
    fn complex_test_1() {
        let source = "((never + gonna * (give - you)) / (up))";
        let tokens: Vec<Token> = tokenize(source);
        let expression = parse(tokens);
        match expression {
            Expression::BinaryExpression { operator, .. } => {
                assert_eq!(operator, Op::Divide);
            },
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_complex_unary() {
        let tokens = tokenize("2 * -2 + -2");
        let expression = parse(tokens);
    
        match expression {
            Expression::BinaryExpression { left, operator, .. } => {
                assert_eq!(operator, Op::Add);
                match left.as_ref() {
                    Expression::BinaryExpression { operator, right,.. } => {
                        assert_eq!(*operator, Op::Multiply);
                        match right.as_ref() {
                            Expression::UnaryExpression { operator,.. } => {
                                assert_eq!(*operator, Op::Subtract)
                            },
                            _ => panic!("Expected - unary expression")
                        }
                    },
                    _ => panic!("Expected * binary expression"),
                }
                
            }
            _ => panic!("Expected + binary expression")
        }
    }

    #[test]
    fn test_if_else_expression() {
        let source = "if 1 then 2 else 3";
        let tokens: Vec<Token> = tokenize(source);
        let expression = parse(tokens);
        match expression {
            Expression::IfExpression { condition, then_branch, else_branch } => {
                match condition.as_ref() {
                    Expression::IntegerLiteral { value } => {
                        assert_eq!(*value, 1);
                    },
                    _ => panic!("Expected literal 1"),
                }
                match then_branch.as_ref() {
                    Expression::IntegerLiteral { value } => {
                        assert_eq!(*value, 2);
                    },
                    _ => panic!("Expected literal 2"),
                }
                match else_branch {
                    Some(else_branch) => {
                        match else_branch.as_ref() {
                            Expression::IntegerLiteral { value } => {
                                assert_eq!(*value, 3)
                            },
                            _ => panic!("Expected literal 3")
                        }
                    },
                    _ => panic!("Expected literal 3"),
                }
            },
            _ => panic!("Expected if expression"),
        }
    }

    #[test]
    fn test_if_expression() {
        let source = "if 1 then 2";
        let tokens: Vec<Token> = tokenize(source);
        let expression = parse(tokens);
        match expression {
            Expression::IfExpression { condition, then_branch, else_branch } => {
                match condition.as_ref() {
                    Expression::IntegerLiteral { value } => {
                        assert_eq!(*value, 1);
                    },
                    _ => panic!("Expected literal 1"),
                }
                match then_branch.as_ref() {
                    Expression::IntegerLiteral { value } => {
                        assert_eq!(*value, 2);
                    },
                    _ => panic!("Expected literal 2"),
                }
                match else_branch {
                    None => { },
                    _ => panic!("Expected literal 3"),
                }
            },
            _ => panic!("Expected if expression"),
        }
    }

    #[test]
    fn test_complex_if_expression() {
        let source = "1 + (if 1 then 2 else 3) * 2";
        let tokens: Vec<Token> = tokenize(source);
        let expression = parse(tokens);
        match expression {
            Expression::BinaryExpression { right , ..} => {
                match *right {
                    Expression::BinaryExpression { left , ..} => {
                        match *left {
                            Expression::IfExpression { .. } => {
                                //
                            }
                            _ => panic!("Expected if expression"),
                        }
                    },
                    _ => panic!("Expected binary expression"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_boolean_literal() {
        let source = "true";
        let tokens = tokenize(source);
        let expression = parse(tokens);
        match expression {
            Expression::BooleanLiteral { value } => {
                assert_eq!(value, true)
            },
            _ => panic!("Not a boolean")
        }
    }

    #[test]
    fn test_function_call_without_args() {
        let source = "f()";
        let tokens = tokenize(source);
        let expression = parse(tokens);
        match expression {
            Expression::CallExpression { arguments, callee } => {
                match *callee {
                    Expression::Identifier { value } => {
                        assert_eq!(value, "f");
                    },
                    _ => panic!("Callee not identifier")
                }
                assert_eq!(arguments.len(), 0)
            },
            _ => panic!("Not a boolean")
        }
    }

    #[test]
    fn test_function_call_with_arg() {
        let source = "f(1 + 1)";
        let tokens = tokenize(source);
        let expression = parse(tokens);
        match expression {
            Expression::CallExpression { arguments, callee } => {
                match *callee {
                    Expression::Identifier { value } => {
                        assert_eq!(value, "f");
                    },
                    _ => panic!("Callee not identifier")
                }
                assert_eq!(arguments.len(), 1)
            },
            _ => panic!("Not a boolean")
        }
    }

    #[test]
    fn test_function_call_with_args() {
        let source = "f(1, 2, 3)";
        let tokens = tokenize(source);
        let expression = parse(tokens);
        match expression {
            Expression::CallExpression { arguments, callee } => {
                match *callee {
                    Expression::Identifier { value } => {
                        assert_eq!(value, "f");
                    },
                    _ => panic!("Callee not identifier")
                }
                assert_eq!(arguments.len(), 3)
            },
            _ => panic!("Not a boolean")
        }
    }
}
