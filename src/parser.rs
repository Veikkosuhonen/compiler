use core::fmt;
use std::str;

use crate::tokenizer::{Op, SourceLocation, Token, TokenType};
use lazy_static::lazy_static;

lazy_static! {
    static ref LOGICAL_OP_PRECEDENCE: Vec<Vec<Op>> = vec![
        vec![Op::And],
        vec![Op::Or],
    ];
    static ref BINARY_OP_PRECEDENCE: Vec<Vec<Op>> = vec![
        vec![Op::Equals, Op::NotEquals],
        vec![Op::LT, Op::GT, Op::LTE, Op::GTE],
        vec![Op::Add, Op::Sub],
        vec![Op::Mul, Op::Div, Op::Mod],
    ];
    static ref UNARY_OP_PRECEDENCE: Vec<Vec<Op>> = vec![
        vec![Op::Not], 
        vec![Op::UnarySub],
        vec![Op::AddressOf, Op::Deref],
        vec![Op::New, Op::Delete],
    ];
}

#[derive(Debug)]
pub struct SyntaxError {
    pub message: String,
    pub start: SourceLocation,
    pub end: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct ASTNode {
    pub expr: Expr<ASTNode>,
    pub start: SourceLocation,
    pub end: SourceLocation,
}

impl ASTNode {
    pub fn new(expr: Expr<ASTNode>, start: SourceLocation, end: SourceLocation) -> ASTNode {
        ASTNode { expr, start, end }
    }

    pub fn implicit_type_annotation(id: &str) -> ASTNode {
        ASTNode { expr: Expr::Identifier { value: String::from(id) }, start: SourceLocation::at(0, 0), end: SourceLocation::at(0, 0) }
    }
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub param_type: Box<ASTNode>,
}

#[derive(Clone)]
pub struct UserDefinedFunction {
    pub id: String,
    pub body: Box<ASTNode>,
    pub params: Vec<Param>,
    pub return_type: Box<ASTNode>,
}

impl fmt::Debug for UserDefinedFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("UserDefinedFunction")
            .field("id", &self.id)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub id: String,
    pub fields: Vec<Param>,
}

#[derive(Debug, Clone)]
pub enum Expr<T> {
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
    Block {
        statements: Vec<Box<T>>,
        result: Box<T>,
    },
    Assignment {
        left: Box<T>,
        right: Box<T>,
    },
    VariableDeclaration {
        id: Box<T>,
        type_annotation: Option<Box<T>>,
        init: Box<T>,
    },
    Logical {
        left: Box<T>,
        operator: Op,
        right: Box<T>,
    },
    Binary {
        left: Box<T>,
        operator: Op,
        right: Box<T>,
    },
    Unary {
        operand: Box<T>,
        operator: Op,
    },
    Member {
        parent: Box<T>,
        name: String,
    },
    If {
        condition: Box<T>,
        then_branch: Box<T>,
        else_branch: Option<Box<T>>,
    },
    While {
        condition: Box<T>,
        body: Box<T>,
    },
    Call {
        callee: Box<T>,
        arguments: Vec<Box<T>>,
    },
    Return {
        result: Box<T>,
    },
    StructInstance {
        struct_name: String,
        fields: Vec<(String, Box<T>)>,
    },
}

#[derive(Debug)]
pub struct Module<F, S> {
    pub functions: Vec<F>,
    pub structs: Vec<S>,
}

impl Module<UserDefinedFunction, Struct> {
    pub fn main(&self) -> &UserDefinedFunction {
        self.functions.iter().find(|func| func.id == "main").expect("Main does not exist")
    }
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

    fn current_start(&self) -> SourceLocation {
        self.peek().unwrap().start
    }

    fn current_end(&self) -> SourceLocation {
        self.peek().unwrap().end
    }

    fn error_here(&self, message: &str) -> SyntaxError {
        let current = self.peek().unwrap();
        SyntaxError {
            message: message.to_string(),
            start: current.start,
            end: current.end,
        }
    }

    fn peek(&self) -> Result<Token, SyntaxError> {
        self.peek_offset(0)
    }

    fn peek_offset(&self, lookahead: i32) -> Result<Token, SyntaxError> {
        let idx: i32 = self.current_index as i32 + lookahead;
        let idx: usize = idx.try_into().expect("Invalid index");

        if idx < self.tokens.len() {
            if let Some(t) = self.tokens.get(idx) {
                Ok(t.clone())
            } else {
                Err(self.error_here("Unexpected end of file"))
            }
        } else {
            Ok(Token {
                token_type: TokenType::None,
                value: "".to_string(),
                start: SourceLocation { line: 0, column: 0 },
                end: SourceLocation { line: 0, column: 0 },
            })
        }
    }

    fn current_is(&mut self, value: &str) -> bool {
        self.peek().is_ok_and(|t| t.value == value)
    }

    fn next_is(&mut self, value: &str) -> bool {
        self.peek_offset(1).is_ok_and(|t| t.value == value)
    }

    fn consume_with_values(
        &mut self,
        expected_type: TokenType,
        expected_values: &[String],
    ) -> Result<Token, SyntaxError> {
        let token: Token = self.peek()?;

        if expected_type == token.token_type
            && (expected_values.is_empty() || expected_values.contains(&token.value))
        {
            self.current_index += 1;
            Ok(token)
        } else {
            Err(SyntaxError {
                message: format!(
                    "Expected {:?} {}, found '{}'",
                    expected_type, 
                    expected_values.iter().map(|v| format!("'{}'", v)).collect::<Vec<String>>().join("|"), 
                    token.value
                ),
                start: token.start,
                end: token.end,
            })
        }
    }

    fn consume(&mut self, expected_type: TokenType) -> Result<Token, SyntaxError>  {
        self.consume_with_values(expected_type, &[])
    }

    fn consume_with_value(&mut self, expected_type: TokenType, value: &str) -> Result<Token, SyntaxError> {
        self.consume_with_values(expected_type, &[value.to_string()])
    }

    fn consume_left_paren(&mut self) -> Result<Token, SyntaxError> {
        self.consume_with_value(TokenType::Punctuation, "(")
    }

    fn consume_right_paren(&mut self) -> Result<Token, SyntaxError> {
        self.consume_with_value(TokenType::Punctuation, ")")
    }

    fn consume_left_curly(&mut self) -> Result<Token, SyntaxError> {
        self.consume_with_value(TokenType::Punctuation, "{")
    }

    fn consume_block_end(&mut self) -> Result<Token, SyntaxError> {
        self.consume_with_value(TokenType::Punctuation, "}")
    }

    /// A statement ends in a semi colon, unless the previous token was a block end
    /// In that case, semi is optional.
    fn consume_statement_end(&mut self) -> Result<Token, SyntaxError> {
        let block_end = self.peek_offset(-1);
        if let Ok(token) = block_end {
            self.consume_available_semi();
            return Ok(token);
        }
        self.consume_with_value(TokenType::Punctuation, ";")
    }

    fn consume_comma(&mut self) -> Result<Token, SyntaxError> {
        self.consume_with_value(TokenType::Punctuation, ",")
    }

    fn consume_keyword(&mut self, value: &str) -> Result<Token, SyntaxError> {
        self.consume_with_value(TokenType::Keyword, value)
    }

    fn consume_available_semi(&mut self) -> bool {
        self.consume_with_value(TokenType::Punctuation, ";").is_ok()
    }

    fn parse_boolean_literal(&mut self) -> Result<ASTNode, SyntaxError> {
        let token = self.consume_with_values(
            TokenType::BooleanLiteral,
            &["true".to_string(), "false".to_string()],
        )?;
        Ok(ASTNode::new(Expr::BooleanLiteral {
            value: token.value.starts_with('t'),
        }, token.start, token.end))
    }

    fn parse_int_literal(&mut self) -> Result<ASTNode, SyntaxError>  {
        let token = self.consume(TokenType::IntegerLiteral)?;

        Ok(ASTNode::new(Expr::IntegerLiteral {
            value: token.value.parse().expect("Not a valid number"),
        }, token.start, token.end))
    }

    fn parse_identifier(&mut self) -> Result<ASTNode, SyntaxError> {
        let token = self.consume(TokenType::Identifier)?;
        Ok(ASTNode::new(Expr::Identifier { value: token.value }, token.start, token.end))
    }

    fn parse_type_annotation(&mut self) -> Result<ASTNode, SyntaxError> {
        let mut identifer = self.parse_identifier()?;
        let start = identifer.start.clone();
        while let Ok(operator) = Op::unary_from_str(&self.peek()?.value) {
            self.consume(TokenType::Operator)?;
            let end = self.current_start().clone();
            identifer = ASTNode::new(Expr::Unary { operand: Box::new(identifer), operator }, start.clone(), end);
        }

        Ok(identifer)
    }

    fn parse_call_expression(&mut self) -> Result<ASTNode, SyntaxError> {
        let callee = self.parse_identifier()?;
        let start = callee.start.clone();
        self.consume_left_paren()?;

        let mut arguments: Vec<Box<ASTNode>> = vec![];
        if !self.current_is(")") {
            loop {
                let arg = self.parse_expression()?;
                arguments.push(Box::new(arg));
                if self.current_is(")") {
                    break;
                }
                self.consume_comma()?;
            }
        }
        let end = self.current_end().clone();
        self.consume_right_paren()?;

        Ok(ASTNode::new(Expr::Call {
            callee: Box::new(callee),
            arguments,
        }, start, end))
    }

    fn parse_if_expression(&mut self) -> Result<ASTNode, SyntaxError> {
        let start = self.current_start().clone();
        self.consume_keyword("if")?;
        let condition = self.parse_expression()?;
        self.consume_keyword("then")?;
        let then_branch = self.parse_expression()?;

        if self.consume_keyword("else").is_ok() {
            let else_branch = self.parse_expression()?;
            let end = else_branch.end.clone();
            Ok(ASTNode::new(Expr::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Some(Box::new(else_branch)),
            }, start, end))
        } else {
            let end = then_branch.end.clone();
            Ok(ASTNode::new(Expr::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: None,
            }, start, end))
        }
    }

    fn parse_while_expression(&mut self) -> Result<ASTNode, SyntaxError> {
        let start = self.current_start().clone();
        self.consume_keyword("while")?;
        let condition = self.parse_expression()?;
        self.consume_keyword("do")?;
        let body = self.parse_expression()?;
        let end = body.end.clone();

        Ok(ASTNode::new(Expr::While {
            condition: Box::new(condition),
            body: Box::new(body),
        }, start, end))
    }

    fn parse_parentheses(&mut self) -> Result<ASTNode, SyntaxError> {
        self.consume_left_paren()?;
        let expr = self.parse_expression()?;
        self.consume_right_paren()?;
        Ok(expr)
    }

    fn parse_keywordy_factor(&mut self) -> Result<ASTNode, SyntaxError> {
        if self.current_is("if") {
            self.parse_if_expression()
        } else if self.current_is("while") {
            self.parse_while_expression()
        } else {
            Err(self.error_here(&format!("Unexpected keyword {:?}", self.peek()?.value)))
        }
    }

    fn parse_struct(&mut self) -> Result<ASTNode, SyntaxError> {
        let start = self.current_start().clone();
        let struct_id = self.consume(TokenType::Identifier)?.value;
        self.consume_left_curly()?;
        let mut fields: Vec<(String, Box<ASTNode>)> = vec![];
        loop {
            if self.current_is("}") {
                self.consume_block_end()?;
                break;
            }
            let field_name = self.consume(TokenType::Identifier)?.value;
            self.consume_with_value(TokenType::Punctuation, ":")?;
            let field_value = self.parse_expression()?;
            fields.push((field_name, Box::new(field_value)));

            if self.consume_comma().is_err() { // No comma? Then it must end.
                self.consume_block_end()?;
                break;
            }
        }

        let end = self.current_end().clone();
        
        Ok(ASTNode::new(Expr::StructInstance {
            struct_name: struct_id,
            fields,
        }, start, end))
    }

    fn parse_factor(&mut self) -> Result<ASTNode, SyntaxError> {
        let token = self.peek()?;

        match token.token_type {
            TokenType::IntegerLiteral => self.parse_int_literal(),
            TokenType::BooleanLiteral => self.parse_boolean_literal(),
            TokenType::Keyword => self.parse_keywordy_factor(),
            TokenType::Identifier => {
                if self.next_is("(") {
                    self.parse_call_expression()
                } else if self.next_is("{") {
                    self.parse_struct()
                } else {
                    self.parse_identifier()
                }
            }
            TokenType::Punctuation => match token.value.as_str() {
                "(" => self.parse_parentheses(),
                "{" => self.parse_block_expression(),
                _ => Err(self.error_here(format!("Unexpected token: {:?}, expected a left brace", token).as_str())),
            },
            TokenType::None => Err(self.error_here("Unexpected end of file")),
            _ => Err(self.error_here(format!("Unexpected token {:?}, expected a factor", token).as_str())),
        }
    }

    fn parse_member_expression(&mut self) -> Result<ASTNode, SyntaxError> {
        let mut parent = self.parse_factor()?;
        loop {
            if !self.current_is(".") {
                break;
            }
            self.consume(TokenType::Operator)?;
            let member_token = self.consume(TokenType::Identifier)?;
            let start = parent.start.clone();
            let end = member_token.end.clone();
            let name = member_token.value;
            parent = ASTNode::new(Expr::Member { parent: Box::new(parent), name }, start, end);
        }

        Ok(parent)
    }

    fn parse_unary_precedence_level(&mut self, level: usize) -> Result<ASTNode, SyntaxError> {
        if level >= UNARY_OP_PRECEDENCE.len() {
            return self.parse_member_expression();
        }

        let ops = UNARY_OP_PRECEDENCE
            .get(level)
            .expect("Invalid precedence level");

        if let Ok(operator) = Op::unary_from_str(&self.peek()?.value) {
            if ops.contains(&operator) {
                let start = self.current_start().clone();
                self.consume(TokenType::Operator)?;
                let operand = self.parse_unary_precedence_level(level)?;
                let end = operand.end.clone();
                Ok(ASTNode::new(Expr::Unary {
                    operator,
                    operand: Box::new(operand),
                }, start, end))
            } else {
                self.parse_unary_precedence_level(level + 1)
            }
        } else {
            self.parse_member_expression()
        }
    }

    fn parse_binary_precedence_level(&mut self, level: usize) -> Result<ASTNode, SyntaxError> {
        if level >= BINARY_OP_PRECEDENCE.len() {
            return self.parse_unary_precedence_level(0);
        }

        let ops = BINARY_OP_PRECEDENCE
            .get(level)
            .expect("Invalid precedence level");

        let mut left = self.parse_binary_precedence_level(level + 1)?;

        loop {
            if let Ok(operator) = Op::binary_from_str(&self.peek()?.value) {
                if ops.contains(&operator) {
                    self.consume(TokenType::Operator)?;
                    let right = self.parse_binary_precedence_level(level + 1)?;
                    let start = left.start.clone();
                    let end = right.end.clone();
                    left = ASTNode::new(Expr::Binary {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right),
                    }, start, end)
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(left)
    }

    fn parse_logical_precedence_level(&mut self, level: usize) -> Result<ASTNode, SyntaxError> {
        if level >= LOGICAL_OP_PRECEDENCE.len() {
            return self.parse_binary_precedence_level(0);
        }

        let ops = LOGICAL_OP_PRECEDENCE
            .get(level)
            .expect("Invalid precedence level");

        let mut left = self.parse_logical_precedence_level(level + 1)?;

        loop {
            if let Ok(operator) = Op::logical_from_str(&self.peek()?.value) {
                if ops.contains(&operator) {
                    self.consume(TokenType::Operator)?;
                    let right = self.parse_logical_precedence_level(level + 1)?;
                    let start = left.start.clone();
                    let end = right.end.clone();
                    left = ASTNode::new(Expr::Logical {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right),
                    }, start, end)
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(left)
    }

    fn parse_assignment_expression(&mut self) -> Result<ASTNode, SyntaxError> {
        let left = self.parse_logical_precedence_level(0)?;
        let start = left.start.clone();
        if let Ok(op) = Op::from_str(&self.peek()?.value) {
            if let Op::Assign = op {
                self.consume(TokenType::Operator)?;
                let right = self.parse_assignment_expression()?;
                let end = right.end.clone();
                Ok(ASTNode::new(Expr::Assignment {
                    left: Box::new(left),
                    right: Box::new(right),
                }, start, end))
            } else {
                self.parse_logical_precedence_level(0)
            }
        } else {
            Ok(left)
        }
    }

    fn parse_expression(&mut self) -> Result<ASTNode, SyntaxError> {
        self.parse_assignment_expression()
    }

    fn parse_variable_declaration(&mut self) -> Result<ASTNode, SyntaxError> {
        let start = self.current_start().clone();
        self.consume_with_value(TokenType::Keyword, "var")?;
        let id = self.parse_identifier()?;
        let mut type_annotation = None;
        if self.consume_with_value(TokenType::Punctuation, ":").is_ok() {
            type_annotation = Some(self.parse_type_annotation()?);
        }
        self.consume_with_value(TokenType::Operator, "=")?;
        let init = self.parse_expression()?;
        let end = init.end.clone();
        Ok(ASTNode::new(Expr::VariableDeclaration {
            id: Box::new(id),
            type_annotation: type_annotation.map(Box::new),
            init: Box::new(init),
        }, start, end))
    }

    fn parse_return_expression(&mut self) -> Result<ASTNode, SyntaxError> {
        let start = self.current_start().clone();
        self.consume_with_value(TokenType::Keyword, "return")?;
        let result = self.parse_expression()?;
        let end = result.end.clone();
        Ok(ASTNode::new(Expr::Return { result: Box::new(result) }, start, end))
    }

    fn parse_statement(&mut self) -> Result<ASTNode, SyntaxError> {
        if self.current_is("var") {
            self.parse_variable_declaration()
        } else if self.current_is("return") {
            self.parse_return_expression()
        } else {
            self.parse_expression()
        }
    }

    fn parse_function_definition(&mut self) -> Result<UserDefinedFunction, SyntaxError> {
        self.consume_keyword("fun")?;
        let id = self.consume(TokenType::Identifier)?.value;
    
        self.consume_left_paren()?;
        let mut params: Vec<Param> = vec![];
        if !self.current_is(")") {
            loop {
                let arg = self.consume(TokenType::Identifier)?.value;
                self.consume_with_value(TokenType::Punctuation, ":")?;
                let type_annotation = self.parse_type_annotation()?;
                params.push(Param { name: arg, param_type: Box::new(type_annotation) });
                if self.current_is(")") {
                    break;
                }
                self.consume_comma()?;
            }
        }
        self.consume_right_paren()?;
        let mut return_type = None;
        if self.consume_with_value(TokenType::Punctuation, ":").is_ok() {
            return_type = Some(self.parse_type_annotation()?);
        }

        let body = self.parse_block_expression()?;

        Ok(UserDefinedFunction {
            id,
            body: Box::new(body),
            params,
            return_type: Box::new( // If not annotated, its unit
                return_type.unwrap_or_else(|| {
                    ASTNode::implicit_type_annotation("Unit")
                })
            )
        })
    }

    fn parse_struct_definition(&mut self) -> Result<Struct, SyntaxError> {
        self.consume_keyword("struct")?;
        let id = self.consume(TokenType::Identifier)?.value;
        self.consume_left_curly()?;
        let mut fields: Vec<Param> = vec![];
        loop {
            if self.current_is("}") {
                self.consume_block_end()?;
                break;
            }
            let field_name = self.consume(TokenType::Identifier)?.value;
            self.consume_with_value(TokenType::Punctuation, ":")?;
            let type_annotation = self.parse_type_annotation()?;
            fields.push(Param { name: field_name, param_type: Box::new(type_annotation) });

            if self.consume_comma().is_err() { // No comma? Then it must end.
                self.consume_block_end()?;
                break;
            }
        }
        
        Ok(Struct { id, fields })
    }

    fn parse_block_expression(&mut self) -> Result<ASTNode, SyntaxError> {
        self.parse_block(false, &mut Vec::new(), &mut Vec::new())
    }

    fn parse_block(&mut self, functions_allowed: bool, functions: &mut Vec<UserDefinedFunction>, structs: &mut Vec<Struct>) -> Result<ASTNode, SyntaxError> {
        self.consume_left_curly()?;
        let start = self.current_start().clone();
        let mut statements: Vec<Box<ASTNode>> = vec![];
        loop {
            if functions_allowed && self.current_is("fun") {
                let fun = self.parse_function_definition()?;
                functions.push(fun);
            } else if functions_allowed && self.current_is("struct") {
                let struct_def = self.parse_struct_definition()?;
                structs.push(struct_def);
            } else {
                if self.consume_block_end().is_ok() {
                    let end: SourceLocation = self.current_end().clone();
                    return Ok(ASTNode::new(Expr::Block {
                        statements,
                        result: Box::new(ASTNode::new(Expr::Unit, end.clone(), end.clone())),
                    }, start, end));
                }
                let statement = self.parse_statement()?;
                if self.consume_block_end().is_ok() {
                    let end = self.current_end().clone();
                    return Ok(ASTNode::new(Expr::Block {
                        statements,
                        result: Box::new(statement),
                    }, start, end));
                }
                statements.push(Box::new(statement));
            }
            self.consume_statement_end()?;
        }
    }

    fn parse_top_level_block(&mut self) -> Result<(ASTNode, Vec<UserDefinedFunction>, Vec<Struct>), SyntaxError> {
        let mut functions: Vec<UserDefinedFunction> = vec![];
        let mut structs: Vec<Struct> = vec![];
        let body = self.parse_block(true, &mut functions, &mut structs)?;

        Ok((
            body,
            functions,
            structs,
        ))
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Module<UserDefinedFunction, Struct>, SyntaxError> {
    let mut parser = Parser::new(tokens);
    let (ast, mut functions, structs) = parser.parse_top_level_block()?;

    if parser.current_index < parser.tokens.len() {
        let token = parser.peek()?;
        let message = format!("Unexpected token {:?} at {:}", token.token_type, token.start.to_string());
        return Err(SyntaxError {
            message,
            start: token.start,
            end: token.end,
        });
    }

    functions.push(UserDefinedFunction { 
        id: String::from("main"), 
        body: Box::new(ast), 
        params: vec![], 
        return_type: Box::new(ASTNode::implicit_type_annotation("Unknown"))
    });

    Ok(Module {
        functions,
        structs,
    })
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::{tokenize, Op, Token};

    use super::*;


fn p(source: &str) -> Box<ASTNode> {
    let node = parse(
        tokenize(source).expect("Should've been able to tokenize the source")
    ).expect("Should've been able to parse the source");
    // Match to block
    match &node.main().body.expr {
        Expr::Block { result,.. } => result.clone(),
        _ => panic!("Parse returned a non block expression")
    }
}

fn parse_module(source: &str) -> Module<UserDefinedFunction, Struct> {
    parse(
        tokenize(source).expect("Should've been able to tokenize the source")
    ).expect("Should've been able to parse the source")
}

#[test]
fn test_invalid_source() {
    let source = "1 + 2 3 4 haha minttuglitch";
    let tokens: Vec<Token> = tokenize(source).expect("Should've been able to tokenize the source");
    let result = parse(tokens);
    println!("{:?}", result);
}

#[test]
fn test_parse_unary_op() {
    let source = "-1";
    let expression = p(source);
    match expression.expr {
        Expr::Unary { operator, .. } => assert_eq!(operator, Op::UnarySub),
        _ => panic!("Expected unary expression"),
    }
}

#[test]
fn test_parse_multiple_unary_op() {
    let source = "not not not false";
    let node = p(source);
    
    match node.expr {
        Expr::Unary { operator, operand } => {
            assert_eq!(operator, Op::Not);
            if let Expr::Unary { operand,.. } = operand.expr {
                if let Expr::Unary { operand,.. } = operand.expr {
                    if let Expr::BooleanLiteral { value } = operand.expr {
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

    let node = p(source);
    
    match node.expr {
        Expr::Binary { operator, .. } => {
            assert_eq!(operator, Op::Add);
        },
        _ => panic!("Expected binary expression"),
    }
}

#[test]
fn test_associative_binary_op() {
    let source = "minttujam - 2 + 3 - 4";

    let node = p(source);
    
    match node.expr {
        Expr::Binary { operator, left ,.. } => {
            assert_eq!(operator, Op::Sub);
            match left.expr {
                Expr::Binary { operator, left, .. } => {
                    assert_eq!(operator, Op::Add);
                    match left.expr {
                        Expr::Binary { operator, left, .. } => {
                            assert_eq!(operator, Op::Sub);
                            match left.expr {
                                Expr::Identifier { value } => {
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

    let node = p(source);
    
    match node.expr {
        Expr::Binary { operator, left, .. } => {
            assert_eq!(operator, Op::Sub);
            match left.expr {
                Expr::Binary { operator, left, right } => {
                    assert_eq!(operator, Op::Add);
                    match left.expr {
                        Expr::IntegerLiteral { value, .. } => {
                            assert_eq!(value, 1);
                            match right.expr {
                                Expr::Binary { operator, left, .. }  => {
                                    assert_eq!(operator, Op::Mul);
                                    match left.expr {
                                        Expr::IntegerLiteral { value } => {
                                            assert_eq!(value, 2);
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

    let node = p(source);
    
    match node.expr {
        Expr::Binary { operator, left, .. } => {
            assert_eq!(operator, Op::Mul);
            match left.expr {
                Expr::Binary { operator, left,.. } => {
                    assert_eq!(operator, Op::Add);
                    match left.expr {
                        Expr::IntegerLiteral { value } => {
                            assert_eq!(value, 1);
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

    let node = p(source);
    
    match node.expr {
        Expr::IntegerLiteral { value, .. } => {
            assert_eq!(value, 42);
        },
        _ => panic!("Expected literal"),
    }
}

#[test]
fn test_parse_identifier() {
    let source = "identifieeeer";

    let node = p(source);
    
    match node.expr {
        Expr::Identifier { value, .. } => {
            assert_eq!(value, "identifieeeer");
        },
        _ => panic!("Expected identifier"),
    }
}

#[test]
fn complex_test_1() {
    let source = "((never + gonna * (give - you)) / (up))";
    let node = p(source);
    match node.expr {
        Expr::Binary { operator, .. } => {
            assert_eq!(operator, Op::Div);
        },
        _ => panic!("Expected binary expression"),
    }
}

#[test]
fn test_complex_unary() {
    let node = p("2 * -2 + -2");

    match node.expr {
        Expr::Binary { left, operator, .. } => {
            assert_eq!(operator, Op::Add);
            match left.expr {
                Expr::Binary { operator, right,.. } => {
                    assert_eq!(operator, Op::Mul);
                    match right.expr {
                        Expr::Unary { operator,.. } => {
                            assert_eq!(operator, Op::UnarySub)
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
    let node = p(source);
    match node.expr {
        Expr::If { condition, then_branch, else_branch } => {
            match condition.expr {
                Expr::IntegerLiteral { value } => {
                    assert_eq!(value, 1);
                },
                _ => panic!("Expected literal 1"),
            }
            match then_branch.expr {
                Expr::IntegerLiteral { value } => {
                    assert_eq!(value, 2);
                },
                _ => panic!("Expected literal 2"),
            }
            match else_branch {
                Some(else_branch) => {
                    match else_branch.expr {
                        Expr::IntegerLiteral { value } => {
                            assert_eq!(value, 3)
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
    let node = p(source);
    match node.expr {
        Expr::If { condition, then_branch, else_branch } => {
            match condition.expr {
                Expr::IntegerLiteral { value } => {
                    assert_eq!(value, 1);
                },
                _ => panic!("Expected literal 1"),
            }
            match then_branch.expr {
                Expr::IntegerLiteral { value } => {
                    assert_eq!(value, 2);
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
    let node = p(source);
    match node.expr {
        Expr::Binary { right , ..} => {
            match right.expr {
                Expr::Binary { left , ..} => {
                    match left.expr {
                        Expr::If { .. } => {
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
    let node = p(source);
    match node.expr {
        Expr::BooleanLiteral { value } => {
            assert_eq!(value, true)
        },
        _ => panic!("Not a boolean")
    }
}

#[test]
fn test_function_call_without_args() {
    let source = "f()";
    let node = p(source);
    match node.expr {
        Expr::Call { arguments, callee } => {
            match callee.expr {
                Expr::Identifier { value } => {
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
    let node = p(source);
    match node.expr {
        Expr::Call { arguments, callee } => {
            match callee.expr {
                Expr::Identifier { value } => {
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
    let node = p(source);
    match node.expr {
        Expr::Call { arguments, callee } => {
            match callee.expr {
                Expr::Identifier { value } => {
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
    let node = p(source);
    if let Expr::Assignment { left, right } = node.expr {
        assert!(matches!(left.expr, Expr::Identifier { .. }));
        assert!(matches!(right.expr, Expr::Binary { .. }));
    } else {
        panic!("Wrong!")
    }
}

#[test]
fn test_block() {
    let n = p("{}");
    assert!(matches!(n.expr, Expr::Block { .. }));
}

#[test]
fn test_example_blocks() {
    p("
    f(a);
    x = y;
    f(x)
    ");

    p("
    if f() then {
        x = 10;
        y = if g(x) then {
            x = x + 1;
            x
        } else {
            g(x)
        } // <-- (this semicolon will become optional later)
        g(y);
    }; // <------ (this too)
    123;
    ");
}

#[test]
fn test_unclosed_block() {
    let source = "{";
    assert!(parse(tokenize(source).expect("Should've been able to tokenize the source")).is_err())
}

#[test]
fn test_variable_declaration() {
    let n = p("
        var x = 42
    ");
    assert!(matches!(n.expr, Expr::VariableDeclaration { .. }))
}

#[test]
fn test_many_variable_declarations() {
    let n = p("
        var x = 42;
        var x = if false then { never() } else { always() }
    ");
    assert!(matches!(n.expr, Expr::VariableDeclaration { .. }))
}

#[test]
fn function_definition() {
    let module = parse_module("
        var x = 313;
    ");
    assert_eq!(module.functions.len(), 1);

    let module2 = parse_module("
        var x = 313;
        fun f() {
            313
        }
    ");
    assert_eq!(module2.functions.len(), 2);

    let fun = module2.functions.get(0).unwrap();
    assert_eq!(fun.id, "f");

    assert_eq!(fun.params.len(), 0);

    if let Expr::Block { result, .. } = &fun.body.expr {
        if let Expr::IntegerLiteral { value } = result.expr {
            assert_eq!(value, 313)
        }
    } else {
        panic!("Function body is not Block")
    }
}

#[test]
fn return_expression() {
    let n = p("
        return 1
    ");
    assert!(matches!(n.expr, Expr::Return { .. }))
}

#[test]
fn type_annotation() {
    let node = p("var x: Int = 313");
    if let Expr::VariableDeclaration { id, type_annotation, .. } = &node.expr {
        if let Expr::Identifier { value } = &id.expr {
            assert_eq!(value, "x");
        } else {
            panic!("Id is not Identifier")
        }
        let type_annotation = type_annotation.as_ref().unwrap();
        if let Expr::Identifier { value,.. } = &type_annotation.expr {
            assert_eq!(value, "Int");
        } else {
            panic!("Type annotation is not Identifier")
        }
    } else {
        panic!("Top level expression is not VariableDeclaration ({:?})", node.expr)
    }
}

#[test]
fn regression_1() {
    p("
    fun identity(x: Int): Int {
        x // This should parse
    }
    
    fun tuplaa(x: Int): Int {
        identity(x) + identity(x)
    }
    
    print_int(tuplaa(69));    
    ");
}

#[test]
fn empty_block_doesnt_return() {
    let n = p("
        {}
    ");
    if let Expr::Block { result,.. } = n.expr {
        assert!(matches!(result.expr, Expr::Unit))
    } else {
        panic!("Wrong")
    }
}


#[test]
fn block_doesnt_need_semi() {
    let n = p("
        {
            { false } 1
        }
    ");
    if let Expr::Block { result,.. } = n.expr {
        assert!(matches!(result.expr, Expr::IntegerLiteral { value: 1 }))
    } else {
        panic!("Wrong")
    }
}


#[test]
fn address_of_op() {
    let n = p("
        &1
    ");
    if let Expr::Unary { operand, operator } = n.expr {
        assert!(matches!(operand.expr, Expr::IntegerLiteral { value: 1 }));
        assert_eq!(operator, Op::AddressOf);
    } else {
        panic!("Wrong")
    }
}


#[test]
fn deref_op() {
    let n = p("
        *1
    ");
    if let Expr::Unary { operand, operator } = n.expr {
        assert!(matches!(operand.expr, Expr::IntegerLiteral { value: 1 }));
        assert_eq!(operator, Op::Deref);
    } else {
        panic!("Wrong")
    }
}

#[test]
fn pointer_type_annotation() {
    let n = p("
        var n: Int** = &1
    ");
    if let Expr::VariableDeclaration { type_annotation,.. } = n.expr {
        if let Some(t) = type_annotation {
            if let Expr::Unary { operand, .. } = t.expr {
                if let Expr::Unary { operand, .. } = operand.expr {
                    if let Expr::Identifier { value } = operand.expr {
                        assert_eq!(value, "Int")
                    } else {
                        panic!("Wrong")
                    }
                } else {
                    panic!("Wrong")
                }
            } else {
                panic!("Wrong")
            }
        } else {
            panic!("Wrong")
        }
    } else {
        panic!("Wrong")
    }
}

#[test]
fn assign_to_deref() {
    let n = p("
        *x = 1
    ");
    if let Expr::Assignment { left, .. } = n.expr {
        if let Expr::Unary { operand, operator } = left.expr {
            assert!(matches!(operand.expr, Expr::Identifier { .. }));
            assert_eq!(operator, Op::Deref);
        } else {
            panic!("Wrong")
        }
    } else {
        panic!("Wrong")
    }
}

#[test]
fn new_and_delete() {
    let _n = p("
        new Int(123)
    ");
    

    let _n = p("
        delete x
    ");
    
}

    #[test]
    fn logical_expr() {
        let _n = p("
            true and false
        ");

        assert!(matches!(_n.expr, Expr::Logical { .. }));
    }

    #[test]
    fn struct_def() {
        let module = parse_module("
            struct Point {
                x: Int,
                y: Int
            }
        ");

        assert_eq!(module.structs.first().unwrap().id, "Point");
    }

    #[test]
    fn struct_def_and_func_def_can_be_followed_by_semicolon() {
        let module = parse_module("
            struct Point { x: Int, y: Int };
            fun point_sum(p: Point*) { p.x + p.y };
        ");

        assert_eq!(module.structs.first().unwrap().id, "Point");
        assert_eq!(module.functions.first().unwrap().id, "point_sum");
    }

    #[test]
    fn struct_instance() {
        let _n = p("
            Point {
                x: 1 + 2,
                y: 2 + 3
            }
        ");

        if let Expr::StructInstance { fields, struct_name } = _n.expr {
            assert_eq!(fields.len(), 2);
            assert_eq!(struct_name, "Point");
        } else {
            panic!("Wrong")
        }
    }

    #[test]
    fn struct_def_and_literal_trailing_comma() {
        let _module = parse_module("
            struct Point {
                x: Int,
                y: Int,
            }

            var p = new Point { x: 1, y: 2, };
        ");
    }

    #[test]
    fn member_access() {
        let _n = p("
            struct Point {
                x: Int,
                y: Int
            }

            var p = new Point { x: 1, y: 2 }

            p.x
        ");

        if let Expr::Member { name,.. } = _n.expr {
            assert_eq!(name, "x");
        } else {
            panic!("Wrong")
        }
    }

}