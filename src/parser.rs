use lazy_static::lazy_static;
use crate::tokenizer::{TokenType, Token, SourceLocation, Op};

lazy_static! {
    static ref BINARY_OP_PRECEDENCE: Vec<Vec<Op>> = vec![
        vec![Op::And],
        vec![Op::Or],
        vec![Op::Equals, Op::NotEquals],
        vec![Op::LT, Op::GT, Op::LTE, Op::GTE],
        vec![Op::Add, Op::Sub],
        vec![Op::Mul, Op::Div, Op::Mod],
        vec![Op::Exp],
    ];
    static ref UNARY_OP_PRECEDENCE: Vec<Vec<Op>> = vec![
        vec![Op::Not],
        vec![Op::Sub],
    ];
}

#[derive(Debug)]
pub enum Expression {
    IntegerLiteral {
        value: i32,
    },
    BooleanLiteral {
        value: bool,
    },
    Unit,
    Identifier {
        value: String,
    },
    BlockExpression {
        statements: Vec<Box<Expression>>,
        result: Box<Expression>,
    },
    AssignmentExpression {
        left: Box<Expression>,
        right: Box<Expression>,
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

    fn parse_block_expression(&mut self) -> Expression {
        self.consume_with_value(TokenType::Punctuation, "{");
        let mut statements: Vec<Box<Expression>> = vec![];
        loop {
            if self.current_is("}") {
                self.consume(TokenType::Punctuation);
                return Expression::BlockExpression { statements, result: Box::new(Expression::Unit) }
            }
            let statement = self.parse_expression();
            if self.current_is("}") {
                self.consume(TokenType::Punctuation);
                return Expression::BlockExpression { statements, result: Box::new(statement) }
            }
            statements.push(Box::new(statement));
            self.consume_with_value(TokenType::Punctuation, ";");
        }
    }

    fn parse_factor(&mut self) -> Expression {
        let token = self.peek();
        println!("factor {:?}", token.value);

        match token.token_type {
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
                match token.value.as_str() {
                    "(" => self.parse_parentheses(),
                    "{" => self.parse_block_expression(),
                    _ => panic!("Unexpected token: {:?}", token),
                }
            },
            TokenType::None => panic!("Unexpected end of file"),
            _ => panic!("Unexpected token {:?}", token)
        }
    }

    fn parse_unary_precedence_level(&mut self, level: usize) -> Expression {
        if level >= UNARY_OP_PRECEDENCE.len() {
            return self.parse_factor();
        }
    
        let ops = UNARY_OP_PRECEDENCE.get(level).expect("Invalid precedence level");

        if let Some(operator) = Op::try_from_str(&self.peek().value) {
            if ops.contains(&operator) {
                self.consume(TokenType::Operator);
                let operand = self.parse_unary_precedence_level(level);
                Expression::UnaryExpression { 
                    operator,
                    operand: Box::new(operand),
                }
            } else {
                self.parse_unary_precedence_level(level + 1)
            }
        } else {
            self.parse_factor()
        }
    } 

    fn parse_binary_precedence_level(&mut self, level: usize) -> Expression {
        if level >= BINARY_OP_PRECEDENCE.len() {
            return self.parse_unary_precedence_level(0);
        }
    
        let ops = BINARY_OP_PRECEDENCE.get(level).expect("Invalid precedence level");

        let mut left = self.parse_binary_precedence_level(level + 1);

        loop {
            if let Some(operator) = Op::try_from_str(&self.peek().value) {
                if ops.contains(&operator) {
                    self.consume(TokenType::Operator);
                    let right = self.parse_binary_precedence_level(level + 1);
                    println!("right {:?}", right);
                    left = Expression::BinaryExpression { left: Box::new(left), operator, right: Box::new(right) }
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        left
    }

    fn parse_assignment_expression(&mut self) -> Expression {
        let left = self.parse_binary_precedence_level(0);
        if let Some(op) = Op::try_from_str(&self.peek().value) {
            if let Op::Assign = op {
                self.consume(TokenType::Operator);
                let right = self.parse_assignment_expression();
                Expression::AssignmentExpression { left: Box::new(left), right: Box::new(right) }
            } else {
                self.parse_binary_precedence_level(0)
            }
        } else {
            left
        }
    }

    fn parse_expression(&mut self) -> Expression {
        self.parse_assignment_expression()
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

    fn p(source: &str) -> Expression {
        parse(tokenize(source))
    }

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
                assert_eq!(operator, Op::Sub);
            },
            _ => panic!("Expected unary expression"),
        }
    }

    #[test]
    fn test_parse_multiple_unary_op() {
        let source = "not not not false";
        let tokens: Vec<Token> = tokenize(source);

        let expression = parse(tokens);
        
        match expression {
            Expression::UnaryExpression { operator, operand } => {
                assert_eq!(operator, Op::Not);
                if let Expression::UnaryExpression { operand,.. } = *operand {
                    if let Expression::UnaryExpression { operand,.. } = *operand {
                        if let Expression::BooleanLiteral { value } = *operand {
                            assert!(!value)
                        } else {
                            panic!("Perkele!")
                        }
                    } else {
                        panic!("Expected unary expression")
                    }
                } else {
                    panic!("Expected unary expression")
                }
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
                assert_eq!(operator, Op::Sub);
                match left.as_ref() {
                    Expression::BinaryExpression { operator, left, .. } => {
                        assert_eq!(*operator, Op::Add);
                        match left.as_ref() {
                            Expression::BinaryExpression { operator, left, .. } => {
                                assert_eq!(*operator, Op::Sub);
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
                assert_eq!(operator, Op::Sub);
                match left.as_ref() {
                    Expression::BinaryExpression { operator, left, right } => {
                        assert_eq!(*operator, Op::Add);
                        match left.as_ref() {
                            Expression::IntegerLiteral { value, .. } => {
                                assert_eq!(*value, 1);
                                match right.as_ref() {
                                    Expression::BinaryExpression { operator, left, .. }  => {
                                        assert_eq!(*operator, Op::Mul);
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
                assert_eq!(operator, Op::Mul);
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
                assert_eq!(operator, Op::Div);
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
                        assert_eq!(*operator, Op::Mul);
                        match right.as_ref() {
                            Expression::UnaryExpression { operator,.. } => {
                                assert_eq!(*operator, Op::Sub)
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

    #[test]
    fn test_assignment() {
        let source = "x = 42 + 58";
        let tokens = tokenize(source);
        let expression = parse(tokens);
        if let Expression::AssignmentExpression { left, right } = expression {
            assert!(matches!(*left, Expression::Identifier { .. }));
            assert!(matches!(*right, Expression::BinaryExpression { .. }));
        } else {
            panic!("Wrong!")
        }
    }

    #[test]
    fn test_block() {
        let e = p("{}");
        assert!(matches!(e, Expression::BlockExpression { .. }));
    }

    #[test]
    fn test_example_blocks() {
        p("
        {
            f(a);
            x = y;
            f(x)
        }");

        p("
            if f() then { // <-- the real example had a while loop, but its not yet implemented
                x = 10;
                y = if g(x) then {
                    x = x + 1;
                    x
                } else {
                    g(x)
                }; // <-- (this semicolon will become optional later)
                g(y);
            }; // <------ (this too)
            123
        }
        ");
    }
}
