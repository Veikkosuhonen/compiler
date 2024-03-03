use std::io;

use lazy_static::lazy_static;

use crate::{interpreter::{EvalRes, Stack, Value}, ir_generator::IRVar, sym_table::Symbol, tokenizer::Op, type_checker::{FunctionType, Type}};

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

pub fn eval_builtin_unary(op: Op, operand: EvalRes, stack: &mut Stack) -> Value {
    if Op::AddressOf == op {
        let addr = operand.1.unwrap_or_else(|| stack.push(operand.0));
        return Value::Pointer(addr)
    }
    if Op::New == op {
        let addr = stack.alloc(operand.0);
        return Value::Pointer(addr)
    }

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

pub fn get_builtin_function_values() -> Vec<(Symbol, Value)> {
    [
        "print_int",
        "print_bool",
        "read_int",
        "Int",
        "Bool",
    ].iter().map(|name| (
        Symbol::Identifier(name.to_string()),
        Value::Function(crate::interpreter::Function::BuiltIn(name.to_string()))
    )).collect()
}

pub fn get_builtin_function_types() -> Vec<(Symbol, Type)> {
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
        }),
        (Op::New, FunctionType {
            param_types: vec![Type::Constructor(Box::new(Type::generic("T")))],
            return_type: Type::Pointer(Box::new(Type::generic("T"))),
        }),
        (Op::Delete, FunctionType {
            param_types: vec![Type::Pointer(Box::new(Type::generic("T")))],
            return_type: Type::Unit,
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
    let ops = get_builtin_function_types();
    ops.iter().map(|(symbol, var_type)| 
        (symbol.to_string(), IRVar { name: symbol.to_string(), var_type: var_type.clone() })
    ).collect()
}

pub fn get_builtin_type_constructor_ir_vars() -> Vec<(String, IRVar)> {
    get_builtin_referrable_types().iter().map(|(symbol, var_type)| 
        (symbol.to_string(), IRVar { name: symbol.to_string(), var_type: Type::Function(Box::new(var_type.get_callable_type())) })
    ).collect()
}
