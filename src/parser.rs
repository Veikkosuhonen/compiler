use crate::tokenizer::{TokenType, Token, SourceLocation};

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
        operator: String,
        right: Box<Expression>,
    },
    IfExpression {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Box<Expression>,
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
        if self.current_index < self.tokens.len() {
            if let Some(t) = self.tokens.get(self.current_index) {
                return t.clone();
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

    fn consume_with_values(&mut self, expected_types: &[TokenType], expected_values: &[String]) -> Token {
        let token: Token = self.peek();

        if expected_types.contains(&token.token_type) && (expected_values.is_empty() || expected_values.contains(&token.value)) {
            self.current_index += 1;
            token
        } else {
            panic!("Unexpected token: {:?}", token);
        }
    }

    fn consume(&mut self, expected_types: &[TokenType]) -> Token {
        self.consume_with_values(expected_types, &[])
    }

    fn parse_boolean_literal(&mut self) -> Expression {
        let token = self.consume_with_values(&[TokenType::BooleanLiteral], &["true".to_string(), "false".to_string()]);
        Expression::BooleanLiteral { value: token.value.starts_with('t') }
    }

    fn parse_int_literal(&mut self) -> Expression {
        let token = self.consume(&[TokenType::IntegerLiteral]);
        Expression::IntegerLiteral {
            value: token.value.parse().expect("Not a valid number"),
        }
    }

    fn parse_identifier(&mut self) -> Expression {
        let token = self.consume(&[TokenType::Identifier]);
        Expression::Identifier {
            value: token.value,
        }
    }

    fn parse_if_expression(&mut self) -> Expression {
        self.consume_with_values(&[TokenType::Keyword], &["if".to_string()]);
        let condition = self.parse_expression();
        self.consume_with_values(&[TokenType::Keyword], &["then".to_string()]);
        let then_branch = self.parse_expression();
        self.consume_with_values(&[TokenType::Keyword], &["else".to_string()]);
        let else_branch = self.parse_expression();
        Expression::IfExpression {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        }
    }

    fn parse_parentheses(&mut self) -> Expression {
        self.consume_with_values(&[TokenType::Punctuation], &["(".to_string()]);
        let expr = self.parse_expression();
        self.consume_with_values(&[TokenType::Punctuation], &[")".to_string()]);
        expr
    }

    fn parse_factor(&mut self) -> Expression {
        let token = self.peek();

        match token.token_type {
            TokenType::IntegerLiteral => self.parse_int_literal(),
            TokenType::BooleanLiteral => self.parse_boolean_literal(),
            TokenType::Keyword => self.parse_if_expression(),
            TokenType::Identifier => self.parse_identifier(),
            TokenType::Punctuation => {
                if token.value == "(" {
                    self.parse_parentheses()
                } else {
                    panic!("Unexpected token: {:?}", token);
                }
            },
            TokenType::None => panic!("Unexpected end of file"),
            _ => panic!("Unexpected token: {:?}", token),
        }
    }

    fn parse_term(&mut self) -> Expression {
        let mut left = self.parse_factor();
        
        while vec!["*", "/", "and"].contains(&self.peek().value.as_str()) {
            let operator = self.consume(&[TokenType::Operator]);
            let right = self.parse_factor();
            left = Expression::BinaryExpression {
                left: Box::new(left),
                operator: operator.value,
                right: Box::new(right),
            };
        }

        left
    }

    fn parse_expression(&mut self) -> Expression {
        let mut left = self.parse_term();
        
        while vec!["+", "-", "or"].contains(&self.peek().value.as_str()) {
            let operator = self.consume(&[TokenType::Operator]);
            let right = self.parse_term();
            left = Expression::BinaryExpression {
                left: Box::new(left),
                operator: operator.value,
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
    fn test_parse_binary_op() {
        let source = "1 + pepejam";
        let tokens: Vec<Token> = tokenize(source);

        let expression = parse(tokens);
        
        match expression {
            Expression::BinaryExpression { operator, .. } => {
                assert_eq!(operator, "+");
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
                assert_eq!(operator, "-");
                match left.as_ref() {
                    Expression::BinaryExpression { operator, left, .. } => {
                        assert_eq!(operator, "+");
                        match left.as_ref() {
                            Expression::BinaryExpression { operator, left, .. } => {
                                assert_eq!(operator, "-");
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
                assert_eq!(operator, "-");
                match left.as_ref() {
                    Expression::BinaryExpression { operator, left, right } => {
                        assert_eq!(operator, "+");
                        match left.as_ref() {
                            Expression::IntegerLiteral { value, .. } => {
                                assert_eq!(*value, 1);
                                match right.as_ref() {
                                    Expression::BinaryExpression { operator, left, .. }  => {
                                        assert_eq!(operator, "*");
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
                assert_eq!(operator, "*");
                match left.as_ref() {
                    Expression::BinaryExpression { operator, left,.. } => {
                        assert_eq!(operator, "+");
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
                assert_eq!(operator, "/");
            },
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_simple_if_expression() {
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
                match else_branch.as_ref() {
                    Expression::IntegerLiteral { value } => {
                        assert_eq!(*value, 3);
                    },
                    _ => panic!("Expected literal 3"),
                }
            },
            _ => panic!("Expected if expression"),
        }
    }

    #[test]
    fn test_complex_if_expression() {
        let source = "1 + 2 * if 4 then 6 else 8 / 0";
        let tokens: Vec<Token> = tokenize(source);
        let expression = parse(tokens);
        match expression {
            Expression::BinaryExpression { left, operator, right } => {

            },
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
}
