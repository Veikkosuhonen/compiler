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

pub fn eval_builtin_unary() -> Value {
    Value::Unit
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

    let mapped_ops = ops.iter().map(|(op, BuiltIn)| {
        (Symbol::Operator(*op), Value::Function(Function::BuiltIn(*BuiltIn)))
    });

    let mapped_funcs = functions.iter().map(|(id, BuiltIn)| {
        (Symbol::Identifier(id.to_string()), Value::Function(Function::BuiltIn(*BuiltIn)))
    });

    mapped_ops.chain(mapped_funcs).collect()
}