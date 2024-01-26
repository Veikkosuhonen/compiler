
use crate::parser::Expression;

pub enum Value {
    Integer(i32),
    Boolean(bool),
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
                                    _ => panic!("Unknown operator {:?}", operator)
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
                                    _ => panic!("Unknown operator {:?}", operator)
                                }
                            )
                        },
                        _ => panic!("Invalid right operand for boolean operation {:?}", operator)
                    }
                },
            }
        },
        Expression::IfExpression { condition, then_branch, else_branch } => {
            let condition_result = interpret(*condition);
            match condition_result {
                Value::Boolean(condition_val) => {
                    if condition_val {
                        interpret(*then_branch)
                    } else {
                        interpret(*else_branch)
                    }
                },
                _ => panic!("If expression condition must be a boolean"),
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
    fn test_interpret_if() {
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
