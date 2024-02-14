use crate::tokenizer::{Op, SourceLocation, Token, TokenType};
use lazy_static::lazy_static;

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
    static ref UNARY_OP_PRECEDENCE: Vec<Vec<Op>> = vec![vec![Op::Not], vec![Op::Sub],];
}

#[derive(Debug)]
pub struct ASTNode {
    pub expr: Expression<ASTNode>,
}

impl ASTNode {
    pub fn new(expr: Expression<ASTNode>) -> ASTNode {
        ASTNode { expr }
    }
}

#[derive(Debug)]
pub enum Expression<T> {
    Unit,
    IntegerLiteral {
        value: i32,
    },
    BooleanLiteral {
        value: bool,
    },
    Identifier {
        value: String,
    },
    BlockExpression {
        statements: Vec<Box<T>>,
        result: Box<T>,
    },
    AssignmentExpression {
        left: Box<T>,
        right: Box<T>,
    },
    VariableDeclaration {
        id: Box<T>,
        init: Box<T>,
    },
    BinaryExpression {
        left: Box<T>,
        operator: Op,
        right: Box<T>,
    },
    UnaryExpression {
        operand: Box<T>,
        operator: Op,
    },
    IfExpression {
        condition: Box<T>,
        then_branch: Box<T>,
        else_branch: Option<Box<T>>,
    },
    WhileExpression {
        condition: Box<T>,
        body: Box<T>,
    },
    CallExpression {
        callee: Box<T>,
        arguments: Vec<Box<T>>,
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
                location: SourceLocation { line: 0, column: 0 },
            }
        }
    }

    fn current_is(&mut self, value: &str) -> bool {
        self.peek().value == value
    }

    fn next_is(&mut self, value: &str) -> bool {
        self.peek_forward(1).value == value
    }

    fn consume_with_values(
        &mut self,
        expected_type: TokenType,
        expected_values: &[String],
    ) -> Token {
        let token: Token = self.peek();

        if expected_type == token.token_type
            && (expected_values.is_empty() || expected_values.contains(&token.value))
        {
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

    fn parse_boolean_literal(&mut self) -> ASTNode {
        let token = self.consume_with_values(
            TokenType::BooleanLiteral,
            &["true".to_string(), "false".to_string()],
        );
        ASTNode::new(Expression::BooleanLiteral {
            value: token.value.starts_with('t'),
        })
    }

    fn parse_int_literal(&mut self) -> ASTNode {
        let token = self.consume(TokenType::IntegerLiteral);
        ASTNode::new(Expression::IntegerLiteral {
            value: token.value.parse().expect("Not a valid number"),
        })
    }

    fn parse_identifier(&mut self) -> ASTNode {
        let token = self.consume(TokenType::Identifier);
        ASTNode::new(Expression::Identifier { value: token.value })
    }

    fn parse_call_expression(&mut self) -> ASTNode {
        let callee = self.parse_identifier();
        self.consume_left_paren();

        let mut arguments: Vec<Box<ASTNode>> = vec![];
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

        ASTNode::new(Expression::CallExpression {
            callee: Box::new(callee),
            arguments,
        })
    }

    fn parse_if_expression(&mut self) -> ASTNode {
        self.consume_keyword("if");
        let condition = self.parse_expression();
        self.consume_keyword("then");
        let then_branch = self.parse_expression();

        if self.current_is("else") {
            self.consume_keyword("else");
            let else_branch = self.parse_expression();
            ASTNode::new(Expression::IfExpression {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Some(Box::new(else_branch)),
            })
        } else {
            ASTNode::new(Expression::IfExpression {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: None,
            })
        }
    }

    fn parse_while_expression(&mut self) -> ASTNode {
        self.consume_keyword("while");
        let condition = self.parse_expression();
        self.consume_keyword("do");
        let body = self.parse_expression();

        ASTNode::new(Expression::WhileExpression {
            condition: Box::new(condition),
            body: Box::new(body),
        })
    }

    fn parse_parentheses(&mut self) -> ASTNode {
        self.consume_left_paren();
        let expr = self.parse_expression();
        self.consume_right_paren();
        expr
    }

    fn parse_block_expression(&mut self) -> ASTNode {
        self.consume_with_value(TokenType::Punctuation, "{");
        let mut statements: Vec<Box<ASTNode>> = vec![];
        loop {
            if self.current_is("}") {
                self.consume(TokenType::Punctuation);
                return ASTNode::new(Expression::BlockExpression {
                    statements,
                    result: Box::new(ASTNode::new(Expression::Unit)),
                });
            }
            let statement = self.parse_statement();
            if self.current_is("}") {
                self.consume(TokenType::Punctuation);
                return ASTNode::new(Expression::BlockExpression {
                    statements,
                    result: Box::new(statement),
                });
            }
            statements.push(Box::new(statement));
            self.consume_with_value(TokenType::Punctuation, ";");
        }
    }

    fn parse_keywordy_factor(&mut self) -> ASTNode {
        if self.current_is("if") {
            self.parse_if_expression()
        } else if self.current_is("while") {
            self.parse_while_expression()
        } else {
            panic!("Unknown keyword {:?}", self.peek())
        }
    }

    fn parse_factor(&mut self) -> ASTNode {
        let token = self.peek();

        match token.token_type {
            TokenType::IntegerLiteral => self.parse_int_literal(),
            TokenType::BooleanLiteral => self.parse_boolean_literal(),
            TokenType::Keyword => self.parse_keywordy_factor(),
            TokenType::Identifier => {
                if self.next_is("(") {
                    self.parse_call_expression()
                } else {
                    self.parse_identifier()
                }
            }
            TokenType::Punctuation => match token.value.as_str() {
                "(" => self.parse_parentheses(),
                "{" => self.parse_block_expression(),
                _ => panic!("Unexpected token: {:?}", token),
            },
            TokenType::None => panic!("Unexpected end of file"),
            _ => panic!("Unexpected token {:?}", token),
        }
    }

    fn parse_unary_precedence_level(&mut self, level: usize) -> ASTNode {
        if level >= UNARY_OP_PRECEDENCE.len() {
            return self.parse_factor();
        }

        let ops = UNARY_OP_PRECEDENCE
            .get(level)
            .expect("Invalid precedence level");

        if let Some(operator) = Op::try_from_str(&self.peek().value) {
            if ops.contains(&operator) {
                self.consume(TokenType::Operator);
                let operand = self.parse_unary_precedence_level(level);
                ASTNode::new(Expression::UnaryExpression {
                    operator,
                    operand: Box::new(operand),
                })
            } else {
                self.parse_unary_precedence_level(level + 1)
            }
        } else {
            self.parse_factor()
        }
    }

    fn parse_binary_precedence_level(&mut self, level: usize) -> ASTNode {
        if level >= BINARY_OP_PRECEDENCE.len() {
            return self.parse_unary_precedence_level(0);
        }

        let ops = BINARY_OP_PRECEDENCE
            .get(level)
            .expect("Invalid precedence level");

        let mut left = self.parse_binary_precedence_level(level + 1);

        loop {
            if let Some(operator) = Op::try_from_str(&self.peek().value) {
                if ops.contains(&operator) {
                    self.consume(TokenType::Operator);
                    let right = self.parse_binary_precedence_level(level + 1);
                    left = ASTNode::new(Expression::BinaryExpression {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right),
                    })
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        left
    }

    fn parse_assignment_expression(&mut self) -> ASTNode {
        let left = self.parse_binary_precedence_level(0);
        if let Some(op) = Op::try_from_str(&self.peek().value) {
            if let Op::Assign = op {
                self.consume(TokenType::Operator);
                let right = self.parse_assignment_expression();
                ASTNode::new(Expression::AssignmentExpression {
                    left: Box::new(left),
                    right: Box::new(right),
                })
            } else {
                self.parse_binary_precedence_level(0)
            }
        } else {
            left
        }
    }

    fn parse_expression(&mut self) -> ASTNode {
        self.parse_assignment_expression()
    }

    fn parse_variable_declaration(&mut self) -> ASTNode {
        self.consume_with_value(TokenType::Keyword, "var");
        let id = self.parse_identifier();
        self.consume_with_value(TokenType::Operator, "=");
        let init = self.parse_expression();
        ASTNode::new(Expression::VariableDeclaration {
            id: Box::new(id),
            init: Box::new(init),
        })
    }

    fn parse_statement(&mut self) -> ASTNode {
        if self.current_is("var") {
            self.parse_variable_declaration()
        } else {
            self.parse_expression()
        }
    }
}

pub fn parse(tokens: Vec<Token>) -> ASTNode {
    let mut token_list = Parser::new(tokens);
    let expr = token_list.parse_statement();
    if token_list.current_index < token_list.tokens.len() {
        panic!("Unexpected token: {:?}", token_list.peek());
    }
    expr
}
