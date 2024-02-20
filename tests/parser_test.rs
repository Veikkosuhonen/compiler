
use compiler::parser::{parse, ASTNode, Expression};
use compiler::tokenizer::{Token,Op,tokenize};

fn p(source: &str) -> ASTNode {
    let node = parse(tokenize(source));
    // Match to block
    match node.top_ast.expr {
        Expression::BlockExpression { result,.. } => *result,
        _ => panic!("Parse returned a non block expression")
    }
}

#[test]
#[should_panic(expected = "Empty source")]
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
    let expression = p(source);
    match expression.expr {
        Expression::UnaryExpression { operator, .. } => assert_eq!(operator, Op::UnarySub),
        _ => panic!("Expected unary expression"),
    }
}

#[test]
fn test_parse_multiple_unary_op() {
    let source = "not not not false";
    let node = p(source);
    
    match node.expr {
        Expression::UnaryExpression { operator, operand } => {
            assert_eq!(operator, Op::Not);
            if let Expression::UnaryExpression { operand,.. } = operand.expr {
                if let Expression::UnaryExpression { operand,.. } = operand.expr {
                    if let Expression::BooleanLiteral { value } = operand.expr {
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
        Expression::BinaryExpression { operator, .. } => {
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
        Expression::BinaryExpression { operator, left ,.. } => {
            assert_eq!(operator, Op::Sub);
            match left.expr {
                Expression::BinaryExpression { operator, left, .. } => {
                    assert_eq!(operator, Op::Add);
                    match left.expr {
                        Expression::BinaryExpression { operator, left, .. } => {
                            assert_eq!(operator, Op::Sub);
                            match left.expr {
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

    let node = p(source);
    
    match node.expr {
        Expression::BinaryExpression { operator, left, .. } => {
            assert_eq!(operator, Op::Sub);
            match left.expr {
                Expression::BinaryExpression { operator, left, right } => {
                    assert_eq!(operator, Op::Add);
                    match left.expr {
                        Expression::IntegerLiteral { value, .. } => {
                            assert_eq!(value, 1);
                            match right.expr {
                                Expression::BinaryExpression { operator, left, .. }  => {
                                    assert_eq!(operator, Op::Mul);
                                    match left.expr {
                                        Expression::IntegerLiteral { value } => {
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
        Expression::BinaryExpression { operator, left, .. } => {
            assert_eq!(operator, Op::Mul);
            match left.expr {
                Expression::BinaryExpression { operator, left,.. } => {
                    assert_eq!(operator, Op::Add);
                    match left.expr {
                        Expression::IntegerLiteral { value } => {
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
        Expression::IntegerLiteral { value, .. } => {
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
        Expression::Identifier { value, .. } => {
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
        Expression::BinaryExpression { operator, .. } => {
            assert_eq!(operator, Op::Div);
        },
        _ => panic!("Expected binary expression"),
    }
}

#[test]
fn test_complex_unary() {
    let node = p("2 * -2 + -2");

    match node.expr {
        Expression::BinaryExpression { left, operator, .. } => {
            assert_eq!(operator, Op::Add);
            match left.expr {
                Expression::BinaryExpression { operator, right,.. } => {
                    assert_eq!(operator, Op::Mul);
                    match right.expr {
                        Expression::UnaryExpression { operator,.. } => {
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
        Expression::IfExpression { condition, then_branch, else_branch } => {
            match condition.expr {
                Expression::IntegerLiteral { value } => {
                    assert_eq!(value, 1);
                },
                _ => panic!("Expected literal 1"),
            }
            match then_branch.expr {
                Expression::IntegerLiteral { value } => {
                    assert_eq!(value, 2);
                },
                _ => panic!("Expected literal 2"),
            }
            match else_branch {
                Some(else_branch) => {
                    match else_branch.expr {
                        Expression::IntegerLiteral { value } => {
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
        Expression::IfExpression { condition, then_branch, else_branch } => {
            match condition.expr {
                Expression::IntegerLiteral { value } => {
                    assert_eq!(value, 1);
                },
                _ => panic!("Expected literal 1"),
            }
            match then_branch.expr {
                Expression::IntegerLiteral { value } => {
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
        Expression::BinaryExpression { right , ..} => {
            match right.expr {
                Expression::BinaryExpression { left , ..} => {
                    match left.expr {
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
    let node = p(source);
    match node.expr {
        Expression::BooleanLiteral { value } => {
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
        Expression::CallExpression { arguments, callee } => {
            match callee.expr {
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
    let node = p(source);
    match node.expr {
        Expression::CallExpression { arguments, callee } => {
            match callee.expr {
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
    let node = p(source);
    match node.expr {
        Expression::CallExpression { arguments, callee } => {
            match callee.expr {
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
    let node = p(source);
    if let Expression::AssignmentExpression { left, right } = node.expr {
        assert!(matches!(left.expr, Expression::Identifier { .. }));
        assert!(matches!(right.expr, Expression::BinaryExpression { .. }));
    } else {
        panic!("Wrong!")
    }
}

#[test]
fn test_block() {
    let n = p("{}");
    assert!(matches!(n.expr, Expression::BlockExpression { .. }));
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
        }; // <-- (this semicolon will become optional later)
        g(y);
    }; // <------ (this too)
    123
    ");
}

#[test]
fn test_variable_declaration() {
    let n = p("
        var x = 42
    ");
    assert!(matches!(n.expr, Expression::VariableDeclaration { .. }))
}

#[test]
fn test_many_variable_declarations() {
    let n = p("
        var x = 42;
        var x = if false then { never() } else { always() }
    ");
    assert!(matches!(n.expr, Expression::VariableDeclaration { .. }))
}
