use crate::parser::Expression;

pub enum Value {
    Integer(i32),
    Boolean(bool),
    Unit,
}

pub fn interpret(node: Expression) -> Value {
    match node {
        Expression::IntegerLiteral { value } => {
            Value::Integer(value)
        },
        Expression::BooleanLiteral { value } => {
            Value::Boolean(value)
        }
        Expression::BinaryExpression { left, operator, right } => {
            let left_result = interpret(*left);
            let right_result = interpret(*right);

            match left_result {
                Value::Integer(ival1) => {
                    match right_result {
                        Value::Integer(ival2) => {
                            Value::Integer(
                                match operator.as_str() {
                                    "+" => ival1 + ival2,
                                    "-" => ival1 - ival2,
                                    "*" => ival1 * ival2,
                                    "/" => ival1 / ival2,
                                    "%" => ival1 % ival2,
                                    _ => panic!("Unknown integer binary operator {:?}", operator)
                                }
                            )
                        },
                        _ => panic!("Invalid right operand for integer operation {:?}", operator)
                    }
                }
                Value::Boolean(bval1) => {
                    match right_result {
                        Value::Boolean(bval2) => {
                            Value::Boolean(
                                match operator.as_str() {
                                    "or" => bval1 || bval2,
                                    "and" => bval1 && bval2,
                                    _ => panic!("Unknown boolean binary operator {:?}", operator)
                                }
                            )
                        },
                        _ => panic!("Invalid right operand for boolean operation {:?}", operator)
                    }
                },
                Value::Unit => panic!("Unit used as operand in binary operation"),
            }
        },
        Expression::IfExpression { condition, then_branch, else_branch } => {
            let condition_result = interpret(*condition);
            match condition_result {
                Value::Boolean(condition_val) => {
                    if condition_val {
                        interpret(*then_branch)
                    } else if let Some(else_branch) = else_branch {
                        interpret(*else_branch)
                    } else {
                        Value::Unit
                    }
                },
                _ => panic!("If expression condition must be a boolean"),
            }
        },
        Expression::UnaryExpression { operand, operator } => {
            let operand_result = interpret(*operand);
            match operand_result {
                Value::Integer(ival) => {
                    Value::Integer(
                        match operator.as_str() {
                            "-" => {
                                -ival
                            },
                            _ => panic!("Invalid integer unary operator {:?}", operator)
                        }
                    )
                },
                Value::Boolean(bval) => {
                    Value::Boolean(
                        match operator.as_str() {
                            "not" => {
                                !bval
                            },
                            _ => panic!("Invalid boolean unary operator {:?}", operator)
                        }
                    )
                },
                Value::Unit => panic!("Unary operator not permitted for Unit value")
            }
        }
        _ => panic!("Unknown expression {:?}", node)
    }
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::{tokenize, Token};
    use crate::parser::parse;
    use super::*;

    fn i(src: &str) -> Value {
        let tokens: Vec<Token> = tokenize(src);
        let expression = parse(tokens);
        interpret(expression)
    }

    #[test]
    fn test_interpret_integers() {
        let result = i("7 + 3 * 2");

        match result {
            Value::Integer(result) => {
                assert_eq!(result, 13)
            },
            _ => panic!("Wrong return value type"),
        }
    }

    #[test]
    fn test_interpret_booleans() {
        let result = i("true and false or true");

        match result {
            Value::Boolean(result) => {
                assert_eq!(result, true)
            },
            _ => panic!("Wrong return value type"),
        }
    }

    #[test]
    fn test_unary_ops() {
        let mut result = i("not true");
        match result {
            Value::Boolean(result) => {
                assert_eq!(result, false)
            },
            _ => panic!("Wrong return value type"),
        }
        result = i("not not not false");
        match result {
            Value::Boolean(result) => {
                assert_eq!(result, true)
            },
            _ => panic!("Wrong return value type"),
        }
        result = i("-42");
        match result {
            Value::Integer(result) => {
                assert_eq!(result, -42)
            },
            _ => panic!("Wrong return value type"),
        }
    }

    #[test]
    fn test_complex_unary() {
        let result = i("2 * -2 + -2");
        match result {
            Value::Integer(result) => {
                assert_eq!(result, -6);
            },
            _ => panic!("Wrong return value type")
        }
    }

    #[test]
    fn test_interpret_if() {
        let result = i("
        if (true and false and true) 
            then 41");

        match result {
            Value::Unit => {},
            _ => panic!("Wrong return value type")
        }
    }

    #[test]
    fn test_interpret_if_else() {
        let result = i("
        if (true and false or true) 
            then 42 
            else 0 - 999");

        match result {
            Value::Integer(result) => {
                assert_eq!(result, 42)
            },
            _ => panic!("Wrong return value type")
        }
    }
}
