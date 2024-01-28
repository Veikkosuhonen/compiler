use crate::parser::Expression;
use crate::tokenizer::Op;

#[derive(Debug)]
pub enum Value {
    Integer(i32),
    Boolean(bool),
    Unit,
}

impl From<Value> for bool {
    fn from(val: Value) -> Self {
        if let Value::Boolean(bval) = val {
            bval
        } else {
            panic!("Tried to convert non-boolean value to bool")
        }
    }
}

impl From<Value> for i32 {
    fn from(val: Value) -> Self {
        if let Value::Integer(ival) = val {
            ival
        } else {
            panic!("Tried to convert non-integer value to i32")
        }
    }
}

impl Op {
    fn eval_binary_integer(&self, ival1: i32, right_expr: Expression) -> Value {
        // Integer ops have no short circuiting so just eval right here
        let ival2: i32 = interpret(right_expr).try_into().expect("Not Value::Integer");

        match self {
            Op::Add =>       Value::Integer(ival1 + ival2),
            Op::Sub =>       Value::Integer(ival1 - ival2),
            Op::Mul =>       Value::Integer(ival1 * ival2),
            Op::Div =>       Value::Integer(ival1 / ival2),
            Op::Mod =>       Value::Integer(ival1 % ival2),
            Op::Exp =>       Value::Integer(i32::pow(ival1, ival2.try_into().unwrap())),
            Op::Equals =>    Value::Boolean(ival1 == ival2),
            Op::NotEquals => Value::Boolean(ival1 == ival2),
            Op::GT =>        Value::Boolean(ival1 > ival2),
            Op::LT =>        Value::Boolean(ival1 < ival2),
            Op::GTE =>       Value::Boolean(ival1 >= ival2),
            Op::LTE =>       Value::Boolean(ival1 <= ival2),
            _ => panic!("Invalid integer binary operator {:?}", &self)
        }
    }

    fn eval_binary_boolean(&self, bval1: bool, right_expr: Expression) -> Value {
        match self {
            Op::Equals =>    Value::Boolean(bval1 == interpret(right_expr).try_into().expect("Not Value::Boolean")),
            Op::NotEquals => Value::Boolean(bval1 != interpret(right_expr).try_into().expect("Not Value::Boolean")),
            Op::And =>       Value::Boolean(bval1 && interpret(right_expr).try_into().expect("Not Value::Boolean")),
            Op::Or =>        Value::Boolean(bval1 || interpret(right_expr).try_into().expect("Not Value::Boolean")),
            _ => panic!("Invalid boolean binary operator {:?}", &self)
        }
    }

    fn eval_unary_integer(&self, ival: i32) -> Value {
        match self {
            Op::Sub => Value::Integer(-ival),
            _ => panic!("invalid integer unary operator {:?}", &self)
        }
    }

    fn eval_unary_boolean(&self, ival: bool) -> Value {
        match self {
            Op::Not => Value::Boolean(!ival),
            _ => panic!("invalid boolean unary operator {:?}", &self)
        }
    }
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

            match left_result {
                Value::Integer(ival1) =>  operator.eval_binary_integer(ival1, *right),
                Value::Boolean(bval1) => operator.eval_binary_boolean(bval1, *right),
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
                Value::Integer(ival) => operator.eval_unary_integer(ival),
                Value::Boolean(bval) => operator.eval_unary_boolean(bval),
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
