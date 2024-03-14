use std::mem;
use std::rc::Rc;
use std::vec;

use crate::sym_table::{SymTable, Symbol};
use crate::parser::{Expr, Module};
use crate::tokenizer::{Op, SourceLocation};
use crate::builtin_functions::*;
use crate::type_checker::{FunctionType, Type, TypedASTNode, TypedStruct, TypedUserDefinedFunction};

#[derive(Debug, Clone)]
pub enum Function {
    BuiltIn(String),
    UserDefined(Rc<TypedUserDefinedFunction>),
    Constructor(FunctionType),
}

impl PartialEq for Function {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub enum Value {
    Integer(i32),
    Boolean(bool),
    Function(Function),
    Pointer(Address),
    #[default] Unit,
}

impl From<&Value> for bool {
    fn from(val: &Value) -> Self {
        if let Value::Boolean(bval) = val {
            *bval
        } else {
            panic!("Tried to convert non-boolean value to bool")
        }
    }
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

impl From<&Value> for i32 {
    fn from(val: &Value) -> Self {
        if let Value::Integer(ival) = val {
            *ival
        } else {
            panic!("Tried to convert non-integer value to i32")
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

#[derive(Clone, Default, PartialEq, Debug)]
pub enum AddressKind { #[default] Stack, Heap }

#[derive(Clone, Default, PartialEq, Debug)]
pub struct Address {
    pub addr: usize,
    pub kind: AddressKind,
}

impl Address {
    pub fn memory(addr: usize) -> Self { Address { addr, kind: AddressKind::Stack } }
    pub fn heap(addr: usize)  -> Self { Address { addr, kind: AddressKind::Heap } }
}

pub struct Memory {
    pub sym_table: Box<SymTable<Symbol,(Address, Type)>>,
    pub stack: Vec<Value>,
    pub heap: Vec<Value>,
}

impl Memory {
    pub fn new(sym_table: Box<SymTable<Symbol,(Address, Type)>>) -> Memory {
        Memory { sym_table, stack: vec![], heap: vec![] }
    }

    pub fn assign(&mut self, addr: &Address, val: Value) {
        match addr.kind {
            AddressKind::Stack => self.stack[addr.addr] = val,
            AddressKind::Heap => self.heap[addr.addr] = val
        }
    }

    pub fn create(&mut self, sym: Symbol, val: Value, val_type: Type) {
        let addr = self.push(val);
        self.sym_table.symbols.insert(sym, (addr, val_type));
    }

    pub fn push(&mut self, val: Value) -> Address {
        let addr = self.stack.len();
        self.stack.push(val);
        Address::memory(addr)
    }

    pub fn alloc(&mut self, val: Value) -> Address {
        let addr = self.heap.len();
        self.heap.push(val);
        Address::heap(addr)
    }

    pub fn free(&mut self, _addr: &Address) {
        // Todo what
    }

    pub fn get_addr(&self, addr: &Address) -> &Value {
        match addr.kind {
            AddressKind::Stack => &self.stack[addr.addr],
            AddressKind::Heap => &self.heap[addr.addr],
        }
    }

    pub fn get(&self, sym: Symbol) -> &Value {
        let addr = &self.sym_table.get_ref(&sym).0;
        self.get_addr(addr)
    }

    /// A mess!
    pub fn block_scope(&mut self, f: impl FnOnce(&mut Self) -> EvalRes) -> EvalRes {
        let outer_symtab = mem::replace(&mut self.sym_table, Default::default());
        let inner_symtab = SymTable::new(Some(outer_symtab));
        let _ = mem::replace(&mut self.sym_table, inner_symtab);
        let original_stack_size = self.stack.len();
        let mut result = f(self);
        let outer_symtab = mem::replace(&mut self.sym_table.parent, Default::default());
        let returns = self.sym_table.returns.clone();
        let _ = mem::replace(&mut self.sym_table, outer_symtab.unwrap());
        // self.debug();
        if let Some((return_value_addr, return_type)) = returns {
            // Copy return value from block's local memory to the return value position
            let return_value = self.stack.swap_remove(return_value_addr.addr);
            self.stack.truncate(original_stack_size);
            let return_value_addr = self.push(return_value);
            self.sym_table.returns = Some((return_value_addr.clone(), return_type));
            result.1 = Some(return_value_addr);
        } else {
            self.stack.truncate(original_stack_size);
            result.1 = None;
        }
        result
    }

    pub fn function_scope(&mut self, args: &Vec<(Symbol, (Value, Type))>, f: impl FnOnce(&mut Self) -> EvalRes) -> EvalRes {
        let outer_symtab = mem::replace(&mut self.sym_table, Default::default());
        // Save memory size before pushing arguments
        let original_stack_size = self.stack.len();
        let mut inner_symtab = SymTable::new(Some(outer_symtab));
        for (arg_symbol, arg) in args {
            inner_symtab.symbols.insert(arg_symbol.clone(), (self.push(arg.0.clone()), arg.1.clone()));
        }
        let _ = mem::replace(&mut self.sym_table, inner_symtab);
        // Eval function. Return value is given in result.0
        let mut result = f(self);
        // Restore memory to original size, dropping function locals
        self.stack.truncate(original_stack_size);
        // Make sure return value has no address, it would be invalid.
        result.1 = None;
        let outer_symtab = mem::replace(&mut self.sym_table.parent, Default::default());
        let _ = mem::replace(&mut self.sym_table, outer_symtab.unwrap());
        result
    }

    pub fn debug(&self) {
        println!("{}", self.stack.iter().enumerate().map(|(addr, v)| {
            let name = self.sym_table.symbols.iter().find(|(_k, v)| v.0.addr == addr).map(|m| m.0);
            format!("{}| {:?} <-- {:?}", addr, v, name)
        }).collect::<Vec<String>>().join("\n"));
        println!("\n{}", self.heap.iter().enumerate().map(|(addr, v)| {
            let name = self.sym_table.symbols.iter().find(|(_k, v)| v.0.addr == addr).map(|m| m.0);
            format!("{}| {:?} <-- {:?}", addr, v, name)
        }).collect::<Vec<String>>().join("\n"));
    }
}

pub type EvalRes = (Value, Option<Address>);

fn eval_binary_op(
    left_node: &Box<TypedASTNode>, 
    right_node: &Box<TypedASTNode>, 
    operator: &Op, 
    memory: &mut Memory
) -> EvalRes {
    let left  = interpret(&left_node, memory).0;
    let right = interpret(&right_node, memory).0;
    (eval_builtin_binary(*operator, left, right), None)
}

fn eval_unary_op(
    operand: &Box<TypedASTNode>,
    operator: &Op,
    memory: &mut Memory
) -> EvalRes {
    let operand_type = &operand.node_type;
    let operand = interpret(&operand, memory);

    if Op::AddressOf == *operator {
        let addr = operand.1.unwrap_or_else(|| memory.push(operand.0));
        return (Value::Pointer(addr), None)
    }

    if Op::New == *operator {
        let operand_size = match operand_type {
            Type::Constructor(ctype) => match ctype.as_ref() {
                Type::Struct(struct_type) => struct_type.fields.len(),
                _ => 1,
            },
            _ => unreachable!(),
        };

        let mut addresses = vec![];
        // If this value is on memory, copy it from there to heap. Otherwise take the direct value
        if let Some(Address { addr, kind: AddressKind::Stack }) = operand.1 {
            for i in 0..operand_size {
                addresses.push(memory.alloc(memory.stack[addr + i].clone()));
            }
        } else {
            addresses.push(memory.alloc(operand.0));
        }
        let struct_addr = addresses.first().unwrap();
        return (Value::Pointer(struct_addr.clone()), Some(struct_addr.clone()))
    }

    (eval_builtin_unary(*operator, operand, memory), None)
}

fn eval_call_expression(
    callee: &Box<TypedASTNode>,
    argument_expr: &Vec<Box<TypedASTNode>>,
    memory: &mut Memory
) -> EvalRes {
    if let Expr::Identifier { value: function_id } = &callee.expr {
        // This is horribly inefficient, we might be cloning a massive function.
        let called_function = memory.get(Symbol::Identifier(function_id.to_string())).clone();
        if let Value::Function(called_function) = called_function {
            let mut args = vec![];
            for node in argument_expr {
                let v = interpret(node, memory).0;
                args.push((String::new(), node.node_type.clone(), v));
            }
            eval_call(called_function, args, memory)
        } else {
            panic!("Calling undefined function {:?}", function_id);
        }
    } else {
        panic!("Callee of a call expression must be an identifier");
    }
}

fn eval_call(
    func: Function,
    args: Vec<(String, Type, Value)>,
    memory: &mut Memory,
) -> EvalRes {
    match func {
        Function::BuiltIn(id) => (eval_builtin_function(id, args.iter().map(|(_, _, v)| v.clone()).collect()), None),
        Function::UserDefined(function) => eval_user_defined_function(function, args, memory),
        Function::Constructor(constructor) => eval_constructor(constructor, args, memory),
    }
}

fn eval_user_defined_function(
    function: Rc<TypedUserDefinedFunction>,
    args: Vec<(String, Type, Value)>,
    memory: &mut Memory
) -> EvalRes {

    let mut named_arguments: Vec<(Symbol, (Value, Type))> = vec![];

    for (idx, param) in function.params.iter().enumerate() {
        let (_, arg_type, arg_val) = args.get(idx).expect("Argument length to match param list length");
        named_arguments.push((
            Symbol::Identifier(param.to_string()),
            (arg_val.clone(), arg_type.clone()),
        ))
    }

    memory.function_scope(&named_arguments, |inner| {
        let implicit_return = interpret(&function.body, inner);
        let ret = inner.sym_table.returns.as_ref();
        if let Some((addr, _)) = ret {
            return (
                inner.get_addr(addr).clone(),
                Some(addr.clone())
            )
        }
        implicit_return
    })
}

fn eval_constructor(
    constructor_type: FunctionType,
    args: Vec<(String, Type, Value)>,
    memory: &mut Memory
) -> EvalRes {
    let mut fields = vec![];
    for param in constructor_type.param_types {
        let arg_val = args.iter().find(|(name, _,_)| *name == param.name).unwrap().2.clone();
        fields.push(arg_val);
    }
    let mut addresses = vec![];
    for field in fields {
        addresses.push(memory.push(field));
    }
    (Value::Unit, addresses.first().cloned())
}

fn eval_assignment_left_side(node: &TypedASTNode, memory: &mut Memory) -> Address {
    match &node.expr {
        Expr::Identifier { value: id } => {
            memory.sym_table.get(&Symbol::Identifier(id.clone())).0
        },
        Expr::Unary { operator: Op::Deref, operand } => {
            let pointer_addr = eval_assignment_left_side(operand, memory);
            let pointer = memory.get_addr(&pointer_addr);
            if let Value::Pointer(pointed_addr) = pointer {
                pointed_addr.clone()
            } else {
                panic!("Left hand side of an assignment must be an identifier or a pointer dereference")
            }
        },
        Expr::Member { parent, name } => {
            let r = eval_member_expr(parent, name, memory);
            r.1.unwrap()
        },
        _ => panic!("Left side of an assignment must be an identifier, member expression or a pointer dereference"),
    }
}

fn eval_member_expr(parent: &TypedASTNode, name: &String, memory: &mut Memory) -> EvalRes {
    let (parent_value, address) = interpret(parent, memory);
    let addr = match parent_value {
        Value::Pointer(addr) => addr,
        _ => match &parent.node_type {
            Type::Struct(_) => address.unwrap(),
            _ => unreachable!(),
        },
    };
    let (member_idx,_) = match &parent.node_type {
        Type::Pointer(pointer_type) => match pointer_type.as_ref() {
            Type::Struct(struct_type) => struct_type.get_member(name),
            _ => unreachable!()
        },
        Type::Struct(struct_type) => struct_type.get_member(name),
        _ => unreachable!()
    };
    let mut member_addr = addr.clone();
    member_addr.addr += member_idx;
    memory.debug();
    (memory.get_addr(&member_addr).clone(), Some(member_addr))
    
}

fn interpret(node: &TypedASTNode, memory: &mut Memory) -> EvalRes {
    match &node.expr {
        Expr::IntegerLiteral { value } => {
            (Value::Integer(*value), None)
        },
        Expr::BooleanLiteral { value } => {
            (Value::Boolean(*value), None)
        },
        Expr::Logical { left, operator, right } => {
            let left =  interpret(left, memory).0;
            let left_bval: bool = (&left).into();
            if *operator == Op::Or {
                if left_bval { return (left, None) };
            } else {
                if !left_bval { return (left, None) };
            }
            interpret(&right, memory)
        },
        Expr::Binary { left, operator, right } => {
            eval_binary_op(left, right, operator, memory)
        },
        Expr::Unary { operand, operator } => {
            eval_unary_op(operand, operator, memory)
        }
        Expr::If { condition, then_branch, else_branch } => {
            if interpret(&condition, memory).0.into() {
                interpret(&then_branch, memory)
            } else if let Some(else_branch) = else_branch {
                interpret(&else_branch, memory)
            } else {
                (Value::Unit, None)
            }
        },
        Expr::While { condition, body } => {
            while interpret(&condition, memory).0.into() {
                interpret(&body, memory);
                if memory.sym_table.returns.is_some() {
                    break;
                }
            }
            (Value::Unit, None)
        },
        Expr::Block { statements, result } => {
            memory.block_scope(|inner| {
                for node in statements {
                    interpret(&node, inner);
                    if inner.sym_table.returns.is_some() {
                        return (Value::Unit, None);
                    }
                }
                interpret(&result, inner)
            })
        },
        Expr::Return { result } => {
            let value = interpret(&result, memory).0;
            memory.sym_table.returns = Some((memory.push(value), result.node_type.clone()));
            (Value::Unit, None)
        },
        Expr::Identifier { value } => {
            let addr = memory.sym_table.get(&Symbol::Identifier(value.clone())).0;
            (
                memory.get_addr(&addr).clone(),
                Some(addr), 
            )
        },
        Expr::Assignment { left, right } => {
            let dest_addr = eval_assignment_left_side(left, memory);
            let value = interpret(&right, memory).0;
            memory.assign(&dest_addr, value.clone());
            (value, Some(dest_addr))
        },
        Expr::VariableDeclaration { id, init,.. } => {
            if let Expr::Identifier { value } = &id.expr {
                let init_value = interpret(&init, memory).0;
                let address = memory.push(init_value);
                memory.sym_table.symbols.insert(Symbol::Identifier(value.clone()), (address, init.node_type.clone()));
                (Value::Unit, None)
            } else {
                panic!("Id of a variable declaration must be an identifier");
            }
        },
        Expr::Call { callee, arguments } => {
            eval_call_expression(callee, arguments, memory)
        },
        Expr::StructInstance { fields, struct_name } => {
            let struct_constructor = memory.get(Symbol::Identifier(struct_name.clone())).clone();
            if let Value::Function(Function::Constructor(constructor)) = struct_constructor {
                let mut args = vec![];
                for param in &constructor.param_types {
                    let arg = fields.iter().find(|(name,_)| *name == param.name).expect("Unknown param");
                    let arg_v = interpret(&arg.1, memory).0;
                    args.push((arg.0.clone(), arg.1.node_type.clone(), arg_v));
                }
                eval_constructor(constructor, args, memory)
            } else {
                panic!("Unknown struct '{struct_name}'")
            }
        },
        Expr::Unit => (Value::Unit, None),
        Expr::Member { parent, name } => eval_member_expr(parent, name, memory),
    }
}

/// Returns the "memory"
fn get_toplevel_sym_table() -> Memory {
    let sym_table = SymTable::new(None);
    let mut memory = Memory::new(sym_table);
    for (sym, val) in get_builtin_function_values() {
        let addr = memory.push(val.0);
        memory.sym_table.symbols.insert(sym, (addr, val.1));
    }
    memory
}

pub fn interpret_program(module: &Module<TypedUserDefinedFunction, TypedStruct>) -> Value {
    let mut memory = get_toplevel_sym_table();
    
    for func in &module.functions {
        memory.create(
            Symbol::Identifier(func.id.clone()), 
            Value::Function(Function::UserDefined(Rc::new(func.clone()))), 
            Type::Function(Box::new(func.func_type.clone()))
        );
    }

    for s in &module.structs {
        let constructor = Type::Struct(s.clone()).get_constructor_type();
        memory.create(
            Symbol::Identifier(s.id.clone()), 
            Value::Function(Function::Constructor(constructor.clone())),
            Type::Function(Box::new(constructor)),
        );
    }

    let main_ref = TypedASTNode::new(
        Expr::Identifier { value: String::from("main") },
        module.main().func_type.return_type.clone(),
        SourceLocation::at(0, 0),
        SourceLocation::at(0, 0)
    );

    let return_value = eval_call_expression(&Box::new(main_ref), &vec![], &mut memory);

    // eprintln!("memory len = {}", memory.stack.len());

    return_value.0
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::{tokenize, Token};
    use crate::parser::parse;
    use crate::type_checker::typecheck_program;
    use super::*;

    fn i(src: &str) -> Value {
        let tokens: Vec<Token> = tokenize(src).expect("Should've been able to tokenize the source");
        let module = parse(tokens).expect("Should've been able to parse the source");
        let module = typecheck_program(module);
        // println!("{:?}", module.main().body);
        interpret_program(&module)
    }

    #[test]
    fn test_interpret_integers() {
        let res = i("7 + 3 * 2");
        assert_eq!(13, res.try_into().expect("Not an integer!"));
    }

    #[test]
    fn test_interpret_booleans() {
        let res = i("true and false or true");
        assert_eq!(true, res.try_into().expect("Not a bool!"));
    }

    #[test]
    fn test_unary_ops() {
        let mut res = i("not true");
        assert_eq!(false, res.try_into().expect("Not a bool!"));
        res = i("not not not false");
        assert_eq!(true, res.try_into().expect("Not a bool!"));
        res = i("-42");
        assert_eq!(-42, res.try_into().expect("Not an integer!"));
    }

    #[test]
    fn test_complex_unary() {
        let res = i("2 * -2 + -2");
        assert_eq!(-6, res.try_into().expect("Not an integer!"));
    }

    #[test]
    fn test_interpret_if() {
        let res = i("
        if (true and false and true) 
            then 41");
        assert!(matches!(res, Value::Unit))
    }

    #[test]
    fn test_interpret_if_else() {
        let res = i("
        if (true and false or true) 
            then 42 
            else 0 - 999");
        assert_eq!(42, res.try_into().expect("Not an integer!"));
    }

    #[test]
    fn interpret_while() {
        let res = i("
        {
            var x = 0;
            while (x < 10) do {
                x = x + 1;
            };
            x
        }");
        assert_eq!(10, res.try_into().expect("Not an integer!"));
    }

    #[test]
    fn block_expression() {
        let res = i("
        {
            1 + 1;
            1 + 2;
            11 * 2 + 10 * 2
        }
        ");
        assert_eq!(42, res.try_into().expect("Not an integer!"));
    }

    #[test]
    fn variable_declaration() {
        let res = i("
            {
                var minttu = true;
            }
        ");
        assert!(matches!(res, Value::Unit));
    }

    #[test]
    fn symbol_reference() {
        let res = i("
            {
                var minttu = 50000;
                minttu
            }
        ");
        assert_eq!(50000, res.try_into().expect("Not an integer!"));
    }

    #[test]
    fn outer_symbol_reference() {
        let res = i("
            {
                var minttu = 50000;
                {
                    minttu + 1
                }
            }
        ");
        println!("{:?}", res);
        assert_eq!(50001, res.try_into().expect("Not an integer!"));
    }

    #[test]
    #[should_panic(expected = "Accessing undefined symbol Identifier(\"minttuglitch\")")]
    fn undefined_reference() {
        i("
            {
                var minttu = 50000;
                minttuglitch
            }
        ");
    }

    #[test]
    fn shadowing() {
        let res = i("
            {
                var minttu = 50000;
                {
                    var minttu = 90000;
                    minttu
                }
            }
        ");
        assert_eq!(90000, res.try_into().expect("Not an integer!"));
    }

    #[test]
    fn assign_to_outside_scope() {
        let res = i("
            {
                var minttu = 50000;
                {
                    minttu = 90000;
                };
                minttu
            }
        ");
        println!("{:?}", res);
        assert_eq!(90000, res.try_into().expect("Not an integer!"));
    }

    #[test]
    fn call_builtin_function() {
        let res = i("
            {
                print_bool(false);
            }
        ");
        assert!(matches!(res, Value::Unit));
        let res2 = i("
            {
                var x = 42;
                print_int(x);

                x
            }
        ");
        assert_eq!(42, res2.try_into().expect("Not an integer!"));
    }

    #[test]
    fn user_function() {
        let res = i("
            fun fuubaar(x: Int, y: Int): Int {
                x + y
            }
            var x = fuubaar(2, 2);
            x
        ");
        assert!(matches!(res, Value::Integer(4)));
    }

    #[test]
    fn compare_booleans() {
        let res = i("true == false");
        assert_eq!(res, Value::Boolean(false));
    }

    #[test]
    fn early_return() {
        let res = i("
        fun foo(): Int {
            var x = 1;
            if 1 == 1 then { return 2; };
            x
        }
        foo()
        ");
        assert_eq!(res, Value::Integer(2));
    }

    #[test]
    fn implicit_return() {
        let res = i("
        fun foo(): Int {
            var x = 1;
            if 1 == 2 then { return 2; };
            x
        }
        foo()
        ");
        assert_eq!(res, Value::Integer(1));
    }

    #[test]
    fn main_can_return() {
        let res = i("
        var x = 1;
        return 2;
        x
        ");
        assert_eq!(res, Value::Integer(2));
    }

    #[test]
    fn can_use_int_pointer() {
        let res = i("
        var x = 69;
        var y = &x;
        y
        ");
        assert!(matches!(res, Value::Pointer(_)));
    }

    #[test]
    fn can_deref_int_pointer() {
        let res = i("
        var x = 69;
        var y = &x;
        *y
        ");

        if let Value::Integer(i) = res {
            assert_eq!(i, 69);
        }
    }

    #[test]
    fn can_deref_assign_int_pointer() {
        let res = i("
        var x = 69;
        var y = &x;
        *y = 420;
        x
        ");
        if let Value::Integer(i) = res {
            assert_eq!(i, 420);
        }
    }

    #[test]
    fn pointers_example() {
        let res = i("
        fun square(p: Int*): Unit {
            *p = *p * *p;
        }
        
        var x: Int = 3;
        square(&x);
        x  //  9
        ");
        if let Value::Integer(i) = res {
            assert_eq!(i, 9);
        }
    }

    #[test]
    fn can_use_function_pointer() {
        let res = i("
        fun sign(x: Int): Int { if x > 0 then 1 else -1 }

        var some_func = &sign;

        some_func
        ");
        
        assert!(matches!(res, Value::Pointer(_)));
    }

    #[test]
    fn can_deref_function_pointer() {
        let res = i("
        fun sign(x: Int): Int { if x > 0 then 1 else -1 }

        var some_func_pointer = &sign;
        var some_func = *some_func_pointer;

        some_func(-87)
        ");
        if let Value::Integer(i) = res {
            assert_eq!(i, -1);
        }
    }

    #[test]
    fn new_and_delete() {
        let res = i("
        var x: Int* = new Int(123);
        var y = 1;
        print_int(*x);
        delete x;
        -1
        ");
        if let Value::Integer(i) = res {
            assert_eq!(i, -1);
        } else {
            panic!("Wrong, got {:?}", res)
        }
    }

    #[test]
    fn heap_alloc_works() {
        let res = i("
        fun alloc(): Int* {
            var p = new Int(123);
            return p;
        }
        var x: Int* = alloc();
        *x
        ");
        if let Value::Integer(i) = res {
            assert_eq!(i, 123);
        } else {
            panic!("Wrong, got {:?}", res)
        }
    }

    #[test]
    fn can_assign_to_heap() {
        let res = i("
        var x: Int* = new Int(123);
        *x = 321;
        *x
        ");
        if let Value::Integer(i) = res {
            assert_eq!(i, 321);
        } else {
            panic!("Wrong, got {:?}", res)
        }
    }

    #[test]
    fn struct_creation() {
        let _res = i("
        struct Point {
            x: Int,
            y: Int
        }
        var p = new Point { x: 1, y: 2 };
        p
        ");
    }

    #[test]
    fn member_access() {
        let res = i("
            struct Vector { x: Int, y: Int, z: Int }
            struct Particle {
                pos: Vector*,
                vel: Vector*
            }
            var p = new Particle { 
                pos: new Vector { x: 0, y: 0, z: -10 },
                vel: new Vector { x: 0, y: -1, z: 0 }
            }
            p.pos.z
        ");
        if let Value::Integer(i) = res {
            assert_eq!(i, -10);
        } else {
            panic!("Wrong, got {:?}", res)
        }
    }

    #[test]
    fn member_assignment() {
        let res = i("
            struct Vector { x: Int, y: Int, z: Int }
            struct Particle {
                pos: Vector*,
                vel: Vector*
            }
            var p = new Particle { 
                pos: new Vector { x: 0, y: 0, z: -10 },
                vel: new Vector { x: 0, y: -1, z: 0 }
            }
            p.pos.z = 1000;
            p.pos.z
        ");
        if let Value::Integer(i) = res {
            assert_eq!(i, 1000);
        } else {
            panic!("Wrong, got {:?}", res)
        }
    }
}
