use std::io;

use crate::{interpreter::{Function, Value}, ir_generator::IRVar, sym_table::Symbol, tokenizer::Op, type_checker::{FunctionType, Type}};

#[derive(Debug, Clone, Copy, PartialEq)]
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
    PrintInt,
    PrintBool,
    ReadInt,
}

fn print_int(args: Vec<Value>) -> Value {
    let arg = args.get(0).expect("Number of arguments to print_int should be 1");
    if let Value::Integer(ival) = arg {
        println!("{}", ival);
    } else {
        panic!("Tried to print '{:?}' which is not an integer", arg)
    }
    Value::Unit
}

fn print_bool(args: Vec<Value>) -> Value {
    let arg = args.get(0).expect("Number of arguments to print_int should be 1");
    if let Value::Boolean(bval) = arg {
        println!("{}", bval);
    } else {
        panic!("Tried to print '{:?}' which is not a boolean", arg)
    }
    Value::Unit
}

fn read_int(args: Vec<Value>) -> Value {
    if args.len() > 0 {
        panic!("Number of arguments to read_int should be 0");
    }

    let mut input = String::new();
    io::stdin()
        .read_line(&mut input)
        .expect("Failed to read input");

    let ival: i32 = input.parse().expect("Input is not an integer");

    Value::Integer(ival)
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

pub fn eval_builtin_function(builtin: BuiltIn, arguments: Vec<Value>) -> Value {
    match builtin {
        BuiltIn::PrintInt => print_int(arguments),
        BuiltIn::PrintBool => print_bool(arguments),
        BuiltIn::ReadInt => read_int(arguments),
        _ => panic!("{:?} is not a builtin function", builtin)
    }
}

pub fn get_builtin_function_symbol_value_mappings() -> Vec<(Symbol, Value)> {
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
        ("print_int", BuiltIn::PrintInt),
        ("print_bool", BuiltIn::PrintBool),
        ("read_int", BuiltIn::ReadInt),
    ];

    let mapped_ops = ops.iter().map(|(op, builtin)| {
        (Symbol::Operator(*op), Value::Function(Function::BuiltIn(*builtin)))
    });

    let mapped_funcs = functions.iter().map(|(id, builtin)| {
        (Symbol::Identifier(id.to_string()), Value::Function(Function::BuiltIn(*builtin)))
    });

    mapped_ops.chain(mapped_funcs).collect()
}

pub fn get_builtin_function_symbol_type_mappings() -> Vec<(Symbol, Type)> {
    let ops = vec![
        (Op::Add, FunctionType {
            param_types: vec![Type::Integer, Type::Integer],
            return_type: Type::Integer,
        }),
        (Op::Sub, FunctionType {
            param_types: vec![Type::Integer, Type::Integer],
            return_type: Type::Integer,
        }),
        (Op::Mul, FunctionType {
            param_types: vec![Type::Integer, Type::Integer],
            return_type: Type::Integer,
        }),
        (Op::Div, FunctionType {
            param_types: vec![Type::Integer, Type::Integer],
            return_type: Type::Integer,
        }),
        (Op::Mod, FunctionType {
            param_types: vec![Type::Integer, Type::Integer],
            return_type: Type::Integer,
        }),
        (Op::Exp, FunctionType {
            param_types: vec![Type::Integer, Type::Integer],
            return_type: Type::Integer,
        }),
        (Op::Not, FunctionType {
            param_types: vec![Type::Boolean],
            return_type: Type::Boolean,
        }),
        (Op::Equals, FunctionType {
            param_types: vec![Type::Integer, Type::Integer],
            return_type: Type::Boolean,
        }),
        (Op::NotEquals, FunctionType {
            param_types: vec![Type::Integer, Type::Integer],
            return_type: Type::Boolean,
        }),
        (Op::LT, FunctionType {
            param_types: vec![Type::Integer, Type::Integer],
            return_type: Type::Boolean,
        }),
        (Op::GT, FunctionType {
            param_types: vec![Type::Integer, Type::Integer],
            return_type: Type::Boolean,
        }),
        (Op::LTE, FunctionType {
            param_types: vec![Type::Integer, Type::Integer],
            return_type: Type::Boolean,
        }),
        (Op::GTE, FunctionType {
            param_types: vec![Type::Integer, Type::Integer],
            return_type: Type::Boolean,
        }),
        (Op::And, FunctionType {
            param_types: vec![Type::Boolean, Type::Boolean],
            return_type: Type::Boolean,
        }),
        (Op::Or, FunctionType {
            param_types: vec![Type::Boolean, Type::Boolean],
            return_type: Type::Boolean,
        }),
    ];

    let functions = vec![
        ("print_int", FunctionType {
            param_types: vec![Type::Integer],
            return_type: Type::Unit,
        }),
        ("print_bool", FunctionType {
            param_types: vec![Type::Boolean],
            return_type: Type::Unit,
        }),
        ("read_int", FunctionType {
            param_types: vec![],
            return_type: Type::Integer,
        }),
    ];

    let mapped_ops = ops.iter().map(|(op, ftype)| {
        (Symbol::Operator(*op), Type::Function(Box::new(ftype.clone())))
    });

    let mapped_funcs = functions.iter().map(|(id, ftype)| {
        (Symbol::Identifier(id.to_string()),  Type::Function(Box::new(ftype.clone())))
    });

    mapped_ops.chain(mapped_funcs).collect()
}

pub fn get_builtin_function_ir_vars() -> Vec<(String, IRVar)> {
    let ops = get_builtin_function_symbol_type_mappings();
    ops.iter().map(|(symbol, var_type)| 
        (symbol.to_string(), IRVar { name: symbol.to_string(), var_type: var_type.clone() })
    ).collect()
}
