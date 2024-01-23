use crate::tokenizer::{TokenType, Token, SourceLocation};

pub enum Expression {
    Literal(Literal),
    BinaryExpression(BinaryExpression),
}

pub struct Literal {
    pub value: String,
}

pub struct Identifier {
    pub value: String,
}

pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
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

    fn parse_int_literal(&mut self) -> Literal {
        let token = self.consume(&[TokenType::IntegerLiteral]);
        Literal {
            value: token.value,
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
            TokenType::IntegerLiteral => Expression::Literal(self.parse_int_literal()),
            TokenType::Punctuation => {
                if token.value == "(" {
                    self.parse_parentheses()
                } else {
                    panic!("Unexpected token: {:?}", token);
                }
            },
            _ => panic!("Unexpected token: {:?}", token),
        }
    }

    fn parse_term(&mut self) -> Expression {
        let mut left = self.parse_factor();
        
        while vec!["*", "/"].contains(&self.peek().value.as_str()) {
            let operator = self.consume(&[TokenType::Operator]);
            let right = self.parse_factor();
            left = Expression::BinaryExpression(BinaryExpression {
                left: Box::new(left),
                operator: operator.value,
                right: Box::new(right),
            });
        }

        left
    }

    fn parse_expression(&mut self) -> Expression {
        let mut left = self.parse_term();
        
        while vec!["+", "-"].contains(&self.peek().value.as_str()) {
            let operator = self.consume(&[TokenType::Operator]);
            let right = self.parse_term();
            left = Expression::BinaryExpression(BinaryExpression {
                left: Box::new(left),
                operator: operator.value,
                right: Box::new(right),
            });
        }

        left
    }

}

pub fn parse(tokens: Vec<Token>) -> Expression {
    let mut token_list = Parser::new(tokens);
    let expr = token_list.parse_expression();
    expr
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::{tokenize, Token};
    use super::*;

    #[test]
    fn test_parse_binary_op() {
        let source = "1 + 2";
        let tokens: Vec<Token> = tokenize(source);
        println!("Tokens: {:?}", tokens);

        let expression = parse(tokens);
        
        match expression {
            Expression::BinaryExpression(bin_expr) => {
                assert_eq!(bin_expr.operator, "+");
            },
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_associative_binary_op() {
        let source = "1 - 2 + 3 - 4";
        let tokens: Vec<Token> = tokenize(source);

        let expression = parse(tokens);
        
        match expression {
            Expression::BinaryExpression(bin_expr) => {
                assert_eq!(bin_expr.operator, "-");
                match bin_expr.left.as_ref() {
                    Expression::BinaryExpression(bin_expr) => {
                        assert_eq!(bin_expr.operator, "+");
                        match bin_expr.left.as_ref() {
                            Expression::BinaryExpression(bin_expr) => {
                                assert_eq!(bin_expr.operator, "-");
                                match bin_expr.left.as_ref() {
                                    Expression::Literal(literal) => {
                                        assert_eq!(literal.value, "1");
                                    },
                                    _ => panic!("Expected literal 1"),
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
        let source = "1 + 2 * 3 - 4";
        let tokens: Vec<Token> = tokenize(source);

        let expression = parse(tokens);
        
        match expression {
            Expression::BinaryExpression(bin_expr) => {
                assert_eq!(bin_expr.operator, "-");
                match bin_expr.left.as_ref() {
                    Expression::BinaryExpression(bin_expr) => {
                        assert_eq!(bin_expr.operator, "+");
                        match bin_expr.left.as_ref() {
                            Expression::Literal(literal) => {
                                assert_eq!(literal.value, "1");
                                match bin_expr.right.as_ref() {
                                    Expression::BinaryExpression(bin_expr) => {
                                        assert_eq!(bin_expr.operator, "*");
                                        match bin_expr.left.as_ref() {
                                            Expression::Literal(literal) => {
                                                assert_eq!(literal.value, "2");
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
            Expression::BinaryExpression(bin_expr) => {
                assert_eq!(bin_expr.operator, "*");
                match bin_expr.left.as_ref() {
                    Expression::BinaryExpression(bin_expr) => {
                        assert_eq!(bin_expr.operator, "+");
                        match bin_expr.left.as_ref() {
                            Expression::Literal(literal) => {
                                assert_eq!(literal.value, "1");
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
            Expression::Literal(literal) => {
                assert_eq!(literal.value, "42");
            },
            _ => panic!("Expected literal"),
        }
    }

    #[test]
    fn complex_test() {
        let source = "((1 + 2 * (3 - 4)) / (5))";
        let tokens: Vec<Token> = tokenize(source);
        let expression = parse(tokens);
        match expression {
            Expression::BinaryExpression(bin_expr) => {
                assert_eq!(bin_expr.operator, "/");
            },
            _ => panic!("Expected binary expression"),
        }
    }
}
