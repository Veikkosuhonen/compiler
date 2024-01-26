
use crate::parser::Expression;

pub enum Value {
    IntegerValue(i32),
    BooleanValue(bool),
}

pub fn interpret(node: Expression) -> Value {
    match node {
        Expression::IntegerLiteral { value } => {
            Value::IntegerValue(value)
        },
        Expression::BooleanLiteral { value } => {
            Value::BooleanValue(value)
        }
        Expression::BinaryExpression { left, operator, right } => {
            let left_result = interpret(*left);
            let right_result = interpret(*right);

            match left_result {
                Value::IntegerValue(ival1) => {
                    match right_result {
                        Value::IntegerValue(ival2) => {
                            Value::IntegerValue(
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
                Value::BooleanValue(bval1) => {
                    match right_result {
                        Value::BooleanValue(bval2) => {
                            Value::BooleanValue(
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
        _ => panic!("Unknown expression {:?}", node)
    }
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::{tokenize, Token};
    use crate::parser::parse;
    use super::*;

    #[test]
    fn test_interpret_integers() {
        let source = "7 + 3 * 2";
        let tokens: Vec<Token> = tokenize(source);

        let expression = parse(tokens);

        let result = interpret(expression);

        match result {
            Value::IntegerValue(result) => {
                assert_eq!(result, 13)
            },
            _ => panic!("Wrong return value type"),
        }
    }

    #[test]
    fn test_interpret_booleans() {
        let source = "true and false or true";
        let tokens: Vec<Token> = tokenize(source);

        let expression = parse(tokens);

        let result = interpret(expression);

        match result {
            Value::BooleanValue(result) => {
                assert_eq!(result, true)
            },
            _ => panic!("Wrong return value type"),
        }
    }
}
