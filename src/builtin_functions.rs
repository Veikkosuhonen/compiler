use std::io;

use crate::{interpreter::{Address, Function, Stack, Value}, ir_generator::IRVar, sym_table::Symbol, tokenizer::Op, type_checker::{FunctionType, Type}};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BuiltIn {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Not,
    Equals,
    NotEquals,
    LT,
    GT,
    LTE,
    GTE,
    And,
    Or,
    AddressOf,
    Deref,
    PrintInt,
    PrintBool,
    ReadInt,
}

fn print_int(args: Vec<Address>, stack: &mut Stack) -> Address {
    let arg = args.get(0).expect("Number of arguments to print_int should be 1");
    if let Value::Integer(ival) = stack.get_addr(arg) {
        println!("{}", ival);
    } else {
        panic!("Tried to print '{:?}' which is not an integer", arg)
    }
    stack.unit()
}

fn print_bool(args: Vec<Address>, stack: &mut Stack) -> Address {
    let arg = args.get(0).expect("Number of arguments to print_int should be 1");
    if let Value::Boolean(bval) = stack.get_addr(arg) {
        println!("{}", if *bval { 1 } else { 0 });
    } else {
        panic!("Tried to print '{:?}' which is not a boolean", arg)
    }
    stack.unit()
}

fn read_int(args: Vec<Address>, stack: &mut Stack) -> Address {
    if args.len() > 0 {
        panic!("Number of arguments to read_int should be 0");
    }

    let mut input = String::new();
    io::stdin()
        .read_line(&mut input)
        .expect("Failed to read input");

    let ival: i32 = input.trim().parse().expect("Input is not an integer");

    stack.push(Value::Integer(ival))
}

pub fn eval_builtin_binary(builtin: BuiltIn, left: Address, eval_right: impl FnOnce(&mut Stack) -> Address, stack: &mut Stack) -> Address {
    let addr = match stack.get_addr(&left).clone() {
        Value::Boolean(bval1) => Value::Boolean({
            // Short circuiting mayhem
            let short_circuit_eval_right = |stack: &mut Stack| -> Value {
                let addr = eval_right(stack);
                stack.get_addr(&addr).clone()
            };
            match builtin {
                BuiltIn::Equals    => bval1 == short_circuit_eval_right(stack).try_into().expect("A boolean"),
                BuiltIn::NotEquals => bval1 != short_circuit_eval_right(stack).try_into().expect("A boolean"),
                BuiltIn::And       => bval1 && short_circuit_eval_right(stack).try_into().expect("A boolean"),
                BuiltIn::Or        => bval1 || short_circuit_eval_right(stack).try_into().expect("A boolean"),
                _ => panic!("Invalid operator for boolean binary operation {:?}", builtin)
            }
        }),
        Value::Integer(ival1) => {
            let right_addr = eval_right(stack);
            let ival2: i32 = stack.get_addr(&right_addr).try_into().expect("Integer"); // No short circuiting
            match builtin {
                BuiltIn::Add =>       Value::Integer(ival1 + ival2),
                BuiltIn::Sub =>       Value::Integer(ival1 - ival2),
                BuiltIn::Mul =>       Value::Integer(ival1 * ival2),
                BuiltIn::Div =>       Value::Integer(ival1 / ival2),
                BuiltIn::Mod =>       Value::Integer(ival1 % ival2),
                BuiltIn::Equals =>    Value::Boolean(ival1 == ival2),
                BuiltIn::NotEquals => Value::Boolean(ival1 != ival2),
                BuiltIn::LT =>        Value::Boolean(ival1 < ival2),
                BuiltIn::GT =>        Value::Boolean(ival1 > ival2),
                BuiltIn::LTE =>       Value::Boolean(ival1 <= ival2),
                BuiltIn::GTE =>       Value::Boolean(ival1 >= ival2),
                _ => panic!("Invalid operator for integer binary operation {:?}", builtin)
            }
        },
        _ => panic!("Invalid value for binary operation {:?}", stack.get_addr(&left))
    };

    stack.push(addr)
}

pub fn eval_builtin_unary(builtin: BuiltIn, operand: Address, stack: &mut Stack) -> Address {
    match stack.get_addr(&operand) {
        Value::Boolean(bval) => stack.push({
            match builtin {
                BuiltIn::Not => Value::Boolean(!bval),
                BuiltIn::AddressOf => Value::Pointer(operand),
                _ => panic!("Invalid operator for boolean unary operation {:?}", builtin)
            }
        }),
        Value::Integer(ival) => stack.push({
            match builtin {
                BuiltIn::Sub => Value::Integer(-ival),
                BuiltIn::AddressOf => Value::Pointer(operand),
                _ => panic!("Invalid operator for integer unary operation {:?}", builtin)
            }
        }),
        Value::Function(..) => stack.push({
            match builtin {
                BuiltIn::AddressOf => Value::Pointer(operand),
                _ => panic!("Invalid operator for function unary operation {:?}", builtin)
            }
        }),
        Value::Unit=> stack.push({
            match builtin {
                BuiltIn::AddressOf => Value::Pointer(operand),
                _ => panic!("Invalid operator for unit unary operation {:?}", builtin)
            }
        }),
        Value::Pointer(addr) => {
            match builtin {
                BuiltIn::Deref => addr.clone(),
                _ => panic!("Invalid operator for pointer unary operation {:?}", builtin)
            }
        },
    }
}

pub fn eval_builtin_function(builtin: BuiltIn, arguments: Vec<Address>, stack: &mut Stack) -> Address {
    match builtin {
        BuiltIn::PrintInt => print_int(arguments, stack),
        BuiltIn::PrintBool => print_bool(arguments, stack),
        BuiltIn::ReadInt => read_int(arguments, stack),
        _ => panic!("{:?} is not a builtin function", builtin)
    }
}

pub fn get_builtin_function_symbol_value_mappings() -> Vec<(Symbol, Value)> {
    let ops = vec![
        (Op::Add, BuiltIn::Add),
        (Op::Sub, BuiltIn::Sub),
        (Op::UnarySub, BuiltIn::Sub),
        (Op::Mul, BuiltIn::Mul),
        (Op::Div, BuiltIn::Div),
        (Op::Mod, BuiltIn::Mod),
        (Op::Not, BuiltIn::Not),
        (Op::Equals, BuiltIn::Equals),
        (Op::NotEquals, BuiltIn::NotEquals),
        (Op::LT, BuiltIn::LT),
        (Op::GT, BuiltIn::GT),
        (Op::LTE, BuiltIn::LTE),
        (Op::GTE, BuiltIn::GTE),
        (Op::And, BuiltIn::And),
        (Op::Or, BuiltIn::Or),
        (Op::AddressOf, BuiltIn::AddressOf),
        (Op::Deref, BuiltIn::Deref),
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
        (Op::UnarySub, FunctionType {
            param_types: vec![Type::Integer],
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
        (Op::Not, FunctionType {
            param_types: vec![Type::Boolean],
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
        (Op::Equals, FunctionType {
            param_types: vec![Type::generic("T"), Type::generic("T")],
            return_type: Type::Boolean,
        }),
        (Op::NotEquals, FunctionType {
            param_types: vec![Type::generic("T"), Type::generic("T")],
            return_type: Type::Boolean,
        }),
        (Op::AddressOf, FunctionType {
            param_types: vec![Type::generic("T")],
            return_type: Type::Pointer(Box::new(Type::generic("T"))),
        }),
        (Op::Deref, FunctionType {
            param_types: vec![Type::Pointer(Box::new(Type::generic("T")))],
            return_type: Type::generic("T"),
        })
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
