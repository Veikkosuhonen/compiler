use super::*;

#[derive(Debug, Clone, Copy)]
pub enum BuiltIn {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,
    Not,
    Equals,
    NotEquals,
    LT,
    GT,
    LTE,
    GTE,
    And,
    Or,
    GetUnit, // Placeholder for BuiltIn function
}

pub fn eval_builtin_binary(builtin: BuiltIn, left: Value, eval_right: impl FnOnce() -> Value) -> Value {
    match left {
        Value::Boolean(bval1) => {
            // Short circuiting mayhem
            Value::Boolean(match builtin {
                BuiltIn::Equals    => bval1 == eval_right().try_into().expect("A boolean"),
                BuiltIn::NotEquals => bval1 != eval_right().try_into().expect("A boolean"),
                BuiltIn::And       => bval1 && eval_right().try_into().expect("A boolean"),
                BuiltIn::Or        => bval1 || eval_right().try_into().expect("A boolean"),
                _ => panic!("Invalid operator for boolean binary operation {:?}", builtin)
            })
        },
        Value::Integer(ival1) => {
            let ival2: i32 = eval_right().try_into().expect("Integer"); // No short circuiting
            match builtin {
                BuiltIn::Add =>       Value::Integer(ival1 + ival2),
                BuiltIn::Sub =>       Value::Integer(ival1 - ival2),
                BuiltIn::Mul =>       Value::Integer(ival1 * ival2),
                BuiltIn::Div =>       Value::Integer(ival1 / ival2),
                BuiltIn::Mod =>       Value::Integer(ival1 % ival2),
                BuiltIn::Exp =>       Value::Integer(i32::pow(ival1, ival2.try_into().expect("A positive exponent"))),
                BuiltIn::Equals =>    Value::Boolean(ival1 == ival2),
                BuiltIn::NotEquals => Value::Boolean(ival1 != ival2),
                BuiltIn::LT =>        Value::Boolean(ival1 < ival2),
                BuiltIn::GT =>        Value::Boolean(ival1 > ival2),
                BuiltIn::LTE =>       Value::Boolean(ival1 <= ival2),
                BuiltIn::GTE =>       Value::Boolean(ival1 >= ival2),
                _ => panic!("Invalid operator for integer binary operation {:?}", builtin)
            }
        },
        _ => panic!("Invalid value for binary operation {:?}", left)
    }
}

pub fn eval_builtin_unary(builtin: BuiltIn, operand: Value) -> Value {
    match operand {
        Value::Boolean(bval) => {
            Value::Boolean(match builtin {
                BuiltIn::Not => !bval,
                _ => panic!("Invalid operator for boolean unary operation {:?}", builtin)
            })
        },
        Value::Integer(ival) => {
            Value::Integer(match builtin {
                BuiltIn::Sub => -ival,
                _ => panic!("Invalid operator for integer unary operation {:?}", builtin)
            })
        },
        _ => panic!("Invalid value for unary operation {:?}", builtin)
    }
}

pub fn get_builtin_function_symbol_mappings() -> Vec<(Symbol, Value)> {
    let ops = vec![
        (Op::Add, BuiltIn::Add),
        (Op::Sub, BuiltIn::Sub),
        (Op::Mul, BuiltIn::Mul),
        (Op::Div, BuiltIn::Div),
        (Op::Mod, BuiltIn::Mod),
        (Op::Exp, BuiltIn::Exp),
        (Op::Not, BuiltIn::Not),
        (Op::Equals, BuiltIn::Equals),
        (Op::NotEquals, BuiltIn::NotEquals),
        (Op::LT, BuiltIn::LT),
        (Op::GT, BuiltIn::GT),
        (Op::LTE, BuiltIn::LTE),
        (Op::GTE, BuiltIn::GTE),
        (Op::And, BuiltIn::And),
        (Op::Or, BuiltIn::Or),

    ];

    let functions = vec![
        ("getUnit", BuiltIn::GetUnit),

    ];

    let mapped_ops = ops.iter().map(|(op, builtin)| {
        (Symbol::Operator(*op), Value::Function(Function::BuiltIn(*builtin)))
    });

    let mapped_funcs = functions.iter().map(|(id, builtin)| {
        (Symbol::Identifier(id.to_string()), Value::Function(Function::BuiltIn(*builtin)))
    });

    mapped_ops.chain(mapped_funcs).collect()
}
