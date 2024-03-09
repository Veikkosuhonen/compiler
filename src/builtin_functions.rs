use std::io;

use lazy_static::lazy_static;

use crate::{interpreter::{EvalRes, Memory, Value}, ir_generator::IRVar, sym_table::Symbol, tokenizer::Op, type_checker::{FunctionType, Type}};

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
        println!("{}", if *bval { 1 } else { 0 });
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

    let ival: i32 = input.trim().parse().expect("Input is not an integer");

    Value::Integer(ival)
}

pub fn eval_builtin_function(id: String, args: Vec<Value>) -> Value {
    match id.as_str() {
        "print_int" => print_int(args),
        "print_bool" => print_bool(args),
        "read_int" => read_int(args),
        "Int"  => args.first().unwrap().clone(),
        "Bool" => args.first().unwrap().clone(),
        _ => panic!("Unknown builtin function {id}")
    }
}

pub fn eval_builtin_binary(op: Op, left: Value, right: Value) -> Value {
    match left {
        Value::Integer(ival1) => {
            let ival2: i32 = right.into();
            match op {
                Op::Add =>       Value::Integer(ival1 + ival2),
                Op::Sub =>       Value::Integer(ival1 - ival2),
                Op::Mul =>       Value::Integer(ival1 * ival2),
                Op::Div =>       Value::Integer(ival1 / ival2),
                Op::Mod =>       Value::Integer(ival1 % ival2),
                Op::Equals =>    Value::Boolean(ival1 == ival2),
                Op::NotEquals => Value::Boolean(ival1 != ival2),
                Op::LT =>        Value::Boolean(ival1 < ival2),
                Op::GT =>        Value::Boolean(ival1 > ival2),
                Op::LTE =>       Value::Boolean(ival1 <= ival2),
                Op::GTE =>       Value::Boolean(ival1 >= ival2),
                _ => panic!("Invalid operator for integer binary operation {:?}", op)
            }
        },
        Value::Boolean(bval1) => {
            let bval2: bool = right.into();
            match op {
                Op::Equals =>    Value::Boolean(bval1 == bval2),
                Op::NotEquals => Value::Boolean(bval1 != bval2),
                _ => panic!("Invalid operator for boolean binary operation {:?}", op)
            }
        },
        _ => panic!("Invalid value for binary operation {:?}", &left),
    }
}

pub fn eval_builtin_unary(op: Op, operand: EvalRes, stack: &mut Memory) -> Value {
    match operand.0 {
        Value::Boolean(bval) => {
            match op {
                Op::Not => Value::Boolean(!bval),
                _ => panic!("Invalid operator for boolean unary operation {:?}", op)
            }
        },
        Value::Integer(ival) => {
            match op {
                Op::UnarySub => Value::Integer(-ival),
                _ => panic!("Invalid operator for integer unary operation {:?}", op)
            }
        },
        Value::Pointer(addr) => {
            match op {
                Op::Deref => stack.get_addr(&addr).clone(),
                Op::Delete => { stack.free(&addr); Value::Unit },
                _ => panic!("Invalid operator for pointer unary operation {:?}", op)
            }
        },
        _ => panic!("Invalid operand for unary operation")
    }
}

lazy_static! {
    static ref BUILTIN_REFERRABLE_TYPES: [(Symbol, Type); 4] = [
        (Symbol::Identifier(String::from("Int")), Type::Integer), 
        (Symbol::Identifier(String::from("Bool")), Type::Boolean),
        (Symbol::Identifier(String::from("Unit")), Type::Unit),
        (Symbol::Identifier(String::from("Unknown")), Type::Unknown),
        // (Symbol::Identifier(String::from("Pointer")), Type::Pointer(Box::new(Type::generic("T"))))
    ];
}

pub fn get_builtin_referrable_types() -> Vec<(Symbol, Type)> {
    BUILTIN_REFERRABLE_TYPES.iter().map(|(sym, t)| (sym.clone(), Type::Typeref(Box::new(t.clone())))).collect()
}

pub fn get_builtin_function_types() -> Vec<(Symbol, Type)> {
    let functions = vec![
        ("print_int", FunctionType ::unnamed_params(
             vec![Type::Integer],
            Type::Unit,
        )),
        ("print_bool", FunctionType ::unnamed_params(
             vec![Type::Boolean],
            Type::Unit,
        )),
        ("read_int", FunctionType ::unnamed_params(
             vec![],
            Type::Integer,
        )),
    ];

    functions.iter().map(|(id, ftype)| {
        (Symbol::Identifier(id.to_string()),  Type::Function(Box::new(ftype.clone())))
    }).collect()
}

pub fn get_builtin_function_values() -> Vec<(Symbol, (Value, Type))> {
    get_builtin_function_types().iter().chain(get_builtin_referrable_types().iter()).map(|(sym, _type)| (
        sym.clone(),
        (
            Value::Function(crate::interpreter::Function::BuiltIn(sym.to_string())),
            _type.clone()
        )
    )).collect()
}

pub fn get_builtin_function_and_operator_types() -> Vec<(Symbol, Type)> {
    let ops = vec![
        (Op::Add, FunctionType ::unnamed_params(
             vec![Type::Integer, Type::Integer],
            Type::Integer,
        )),
        (Op::Sub, FunctionType ::unnamed_params(
             vec![Type::Integer, Type::Integer],
            Type::Integer,
        )),
        (Op::UnarySub, FunctionType ::unnamed_params(
             vec![Type::Integer],
            Type::Integer,
        )),
        (Op::Mul, FunctionType ::unnamed_params(
             vec![Type::Integer, Type::Integer],
            Type::Integer,
        )),
        (Op::Div, FunctionType ::unnamed_params(
             vec![Type::Integer, Type::Integer],
            Type::Integer,
        )),
        (Op::Mod, FunctionType ::unnamed_params(
             vec![Type::Integer, Type::Integer],
            Type::Integer,
        )),
        (Op::Not, FunctionType ::unnamed_params(
             vec![Type::Boolean],
            Type::Boolean,
        )),
        (Op::LT, FunctionType ::unnamed_params(
             vec![Type::Integer, Type::Integer],
            Type::Boolean,
        )),
        (Op::GT, FunctionType ::unnamed_params(
             vec![Type::Integer, Type::Integer],
            Type::Boolean,
        )),
        (Op::LTE, FunctionType ::unnamed_params(
             vec![Type::Integer, Type::Integer],
            Type::Boolean,
        )),
        (Op::GTE, FunctionType ::unnamed_params(
             vec![Type::Integer, Type::Integer],
            Type::Boolean,
        )),
        (Op::And, FunctionType ::unnamed_params(
             vec![Type::Boolean, Type::Boolean],
            Type::Boolean,
        )),
        (Op::Or, FunctionType ::unnamed_params(
             vec![Type::Boolean, Type::Boolean],
            Type::Boolean,
        )),
        (Op::Equals, FunctionType ::unnamed_params(
             vec![Type::generic("T"), Type::generic("T")],
            Type::Boolean,
        )),
        (Op::NotEquals, FunctionType ::unnamed_params(
             vec![Type::generic("T"), Type::generic("T")],
            Type::Boolean,
        )),
        (Op::AddressOf, FunctionType ::unnamed_params(
             vec![Type::generic("T")],
            Type::Pointer(Box::new(Type::generic("T"))),
        )),
        (Op::Deref, FunctionType ::unnamed_params(
             vec![Type::Pointer(Box::new(Type::generic("T")))],
            Type::generic("T"),
        )),
        (Op::New, FunctionType ::unnamed_params(
             vec![Type::Constructor(Box::new(Type::generic("T")))],
            Type::Pointer(Box::new(Type::generic("T"))),
        )),
        (Op::Delete, FunctionType ::unnamed_params(
             vec![Type::Pointer(Box::new(Type::generic("T")))],
            Type::Unit,
        )),
    ];

    let mut mapped_ops: Vec<(Symbol, Type)> = ops.iter().map(|(op, ftype)| {
        (Symbol::Operator(*op), Type::Function(Box::new(ftype.clone())))
    }).collect();

    mapped_ops.extend(get_builtin_function_types());

    mapped_ops
}

pub fn get_builtin_function_ir_vars() -> Vec<(String, IRVar)> {
    let ops = get_builtin_function_and_operator_types();
    ops.iter().map(|(symbol, var_type)| 
        (symbol.to_string(), IRVar { name: symbol.to_string(), var_type: var_type.clone() })
    ).collect()
}

pub fn get_builtin_type_constructor_ir_vars() -> Vec<(String, IRVar)> {
    get_builtin_referrable_types().iter().map(|(symbol, var_type)| 
        (symbol.to_string(), IRVar { name: symbol.to_string(), var_type: Type::Function(Box::new(var_type.get_callable_type())) })
    ).collect()
}
