use std::collections::HashMap;
use std::mem;
use std::rc::Rc;
use std::vec;
use std::fmt;

use crate::sym_table::{SymTable, Symbol};
use crate::parser::{ASTNode, Expr, Module};
use crate::tokenizer::{Op, SourceLocation};
use crate::builtin_functions::*;

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub param_type: Box<ASTNode>,
}

#[derive(Clone)]
pub struct UserDefinedFunction {
    pub id: String,
    pub body: Box<ASTNode>,
    pub params: Vec<Param>,
    pub return_type: Box<ASTNode>,
}

impl fmt::Debug for UserDefinedFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("UserDefinedFunction")
            .field("id", &self.id)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub enum Function {
    BuiltIn(String),
    UserDefined(Rc<UserDefinedFunction>),
}

impl PartialEq for Function {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub id: String,
    pub fields: Vec<Param>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructValue {
    pub fields: HashMap<String, Value>,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub enum Value {
    Integer(i32),
    Boolean(bool),
    Function(Function),
    Struct(StructValue),
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
    pub fn stack(addr: usize) -> Self { Address { addr, kind: AddressKind::Stack } }
    pub fn heap(addr: usize)  -> Self { Address { addr, kind: AddressKind::Heap } }
}

pub struct Memory {
    pub sym_table: Box<SymTable<Address>>,
    pub stack: Vec<Value>,
    pub heap: Vec<Value>,
}

impl Memory {
    pub fn new(sym_table: Box<SymTable<Address>>) -> Memory {
        Memory { sym_table, stack: vec![], heap: vec![] }
    }

    pub fn create(&mut self, sym: Symbol, val: Value) {
        let addr = self.push(val);
        self.sym_table.symbols.insert(sym, addr);
    }

    pub fn push(&mut self, val: Value) -> Address {
        let addr = self.stack.len();
        self.stack.push(val);
        Address::stack(addr)
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
        let addr = self.sym_table.get_ref(&sym);
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
        if let Some(return_value_addr) = returns {
            // Copy return value from block's local memory to the return value position
            let return_value = self.stack.swap_remove(return_value_addr.addr);
            self.stack.truncate(original_stack_size);
            let return_value_addr = self.push(return_value);
            self.sym_table.returns = Some(return_value_addr.clone());
            result.1 = Some(return_value_addr);
        } else {
            self.stack.truncate(original_stack_size);
            result.1 = None;
        }
        result
    }

    pub fn function_scope(&mut self, args: &Vec<(Symbol, Value)>, f: impl FnOnce(&mut Self) -> EvalRes) -> EvalRes {
        let outer_symtab = mem::replace(&mut self.sym_table, Default::default());
        // Save stack size before pushing arguments
        let original_stack_size = self.stack.len();
        let mut inner_symtab = SymTable::new(Some(outer_symtab));
        for (arg_symbol, arg_val) in args {
            inner_symtab.symbols.insert(arg_symbol.clone(), self.push(arg_val.clone()));
        }
        let _ = mem::replace(&mut self.sym_table, inner_symtab);
        // Eval function. Return value is given in result.0
        let mut result = f(self);
        // Restore stack to original size, dropping function locals
        self.stack.truncate(original_stack_size);
        // Make sure return value has no address, it would be invalid.
        result.1 = None;
        let outer_symtab = mem::replace(&mut self.sym_table.parent, Default::default());
        let _ = mem::replace(&mut self.sym_table, outer_symtab.unwrap());
        result
    }

    pub fn debug(&self) {
        println!("{}", self.stack.iter().enumerate().map(|(addr, v)| format!("{}| {:?}", addr, v)).collect::<Vec<String>>().join("\n"));
    }
}

pub type EvalRes = (Value, Option<Address>);

fn eval_binary_op(
    left_node: &Box<ASTNode>, 
    right_node: &Box<ASTNode>, 
    operator: &Op, 
    stack: &mut Memory
) -> EvalRes {
    let left  = interpret(&left_node, stack).0;
    let right = interpret(&right_node, stack).0;
    (eval_builtin_binary(*operator, left, right), None)
}

fn eval_unary_op(
    operand: &Box<ASTNode>,
    operator: &Op,
    stack: &mut Memory
) -> EvalRes {
    let operand = interpret(&operand, stack);
    (eval_builtin_unary(*operator, operand, stack), None)
}

fn eval_call_expression(
    callee: &Box<ASTNode>,
    argument_expr: &Vec<Box<ASTNode>>,
    stack: &mut Memory
) -> EvalRes {
    if let Expr::Identifier { value: function_id } = &callee.expr {
        // This is horribly inefficient, we might be cloning a massive function.
        let called_function = stack.get(Symbol::Identifier(function_id.to_string())).clone();
        if let Value::Function(called_function) = called_function {
            match called_function {
                Function::BuiltIn(id) => {
                    let mut args: Vec<Value> = vec![];
                    for expr in argument_expr {
                        let arg_v= interpret(&expr, stack).0;
                        args.push(arg_v);
                    }
                    let result = eval_builtin_function(id, args);
                    (result, None)
                },
                Function::UserDefined(function) => eval_user_defined_function(function, argument_expr, stack),
            }
        } else {
            panic!("Calling undefined function {:?}", function_id);
        }
    } else {
        panic!("Callee of a call expression must be an identifier");
    }
}

fn eval_user_defined_function(
    function: Rc<UserDefinedFunction>,
    argument_expr: &Vec<Box<ASTNode>>,
    stack: &mut Memory
) -> EvalRes {
    let mut args: Vec<Value> = vec![];
    for expr in argument_expr {
        args.push(interpret(&expr, stack).0);
    }

    let mut named_arguments: Vec<(Symbol, Value)> = vec![];

    for (idx, param) in function.params.iter().enumerate() {
        named_arguments.push((
            Symbol::Identifier(param.name.to_string()),
            args.get(idx).expect("Argument length to match param list length").clone()
        ))
    }

    stack.function_scope(&named_arguments, |inner| {
        let implicit_return = interpret(&function.body, inner);
        let ret_addr = inner.sym_table.returns.as_ref();
        if let Some(addr) = ret_addr {
            return (
                inner.get_addr(addr).clone(),
                Some(addr.clone())
            )
        }
        implicit_return
    })
}

fn eval_assignment_left_side(node: &ASTNode, stack: &mut Memory) -> Address {
    match &node.expr {
        Expr::Identifier { value: id } => {
            stack.sym_table.get(&Symbol::Identifier(id.clone()))
        },
        Expr::Unary { operator: Op::Deref, operand } => {
            let pointer_addr = eval_assignment_left_side(operand, stack);
            let pointer = stack.get_addr(&pointer_addr);
            if let Value::Pointer(pointed_addr) = pointer {
                pointed_addr.clone()
            } else {
                panic!("Left hand side of an assignment must be an identifier or a pointer dereference")
            }
        },
        _ => panic!("Left side of an assignment must be an identifier or a pointer dereference"),
    }
}

fn interpret(node: &ASTNode, stack: &mut Memory) -> EvalRes {
    match &node.expr {
        Expr::IntegerLiteral { value } => {
            (Value::Integer(*value), None)
        },
        Expr::BooleanLiteral { value } => {
            (Value::Boolean(*value), None)
        },
        Expr::Logical { left, operator, right } => {
            let left =  interpret(left, stack).0;
            let left_bval: bool = (&left).into();
            if *operator == Op::Or {
                if left_bval { return (left, None) };
            } else {
                if !left_bval { return (left, None) };
            }
            interpret(&right, stack)
        },
        Expr::Binary { left, operator, right } => {
            eval_binary_op(left, right, operator, stack)
        },
        Expr::Unary { operand, operator } => {
            eval_unary_op(operand, operator, stack)
        }
        Expr::If { condition, then_branch, else_branch } => {
            if interpret(&condition, stack).0.into() {
                interpret(&then_branch, stack)
            } else if let Some(else_branch) = else_branch {
                interpret(&else_branch, stack)
            } else {
                (Value::Unit, None)
            }
        },
        Expr::While { condition, body } => {
            while interpret(&condition, stack).0.into() {
                interpret(&body, stack);
                if stack.sym_table.returns.is_some() {
                    break;
                }
            }
            (Value::Unit, None)
        },
        Expr::Block { statements, result } => {
            stack.block_scope(|inner| {
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
            let value = interpret(&result, stack).0;
            stack.sym_table.returns = Some(stack.push(value));
            (Value::Unit, None)
        },
        Expr::Identifier { value } => {
            let addr = stack.sym_table.get(&Symbol::Identifier(value.clone()));
            (
                stack.get_addr(&addr).clone(),
                Some(addr)
            )
        },
        Expr::Assignment { left, right } => {
            let dest_addr = eval_assignment_left_side(left, stack);
            let value = interpret(&right, stack).0;
            stack.stack[dest_addr.addr] = value.clone();
            (value, Some(dest_addr))
        },
        Expr::VariableDeclaration { id, init,.. } => {
            if let Expr::Identifier { value } = &id.expr {
                let init_value = interpret(&init, stack).0;
                let address = stack.push(init_value);
                stack.sym_table.symbols.insert(Symbol::Identifier(value.clone()), address);
                (Value::Unit, None)
            } else {
                panic!("Id of a variable declaration must be an identifier");
            }
        },
        Expr::Call { callee, arguments } => {
            eval_call_expression(callee, arguments, stack)
        },
        Expr::StructInstance { fields,.. } => {
            let mut struct_fields = HashMap::new();
            for (field_name, field_value) in fields {
                let value = interpret(&field_value, stack).0;
                struct_fields.insert(field_name.clone(), value);
            }
            (Value::Struct(StructValue { fields: struct_fields }), None)
        },
        Expr::Unit => (Value::Unit, None),
        Expr::Member { parent, name } => {
            let parent = interpret(parent, stack).0;
            match parent {
                Value::Pointer(addr) => match stack.get_addr(&addr) {
                    Value::Struct(struct_value) => (struct_value.fields.get(name).expect("Struct has no such value").clone(), None),
                    _ => panic!("Pointer in a member expression does not point to a struct"),
                },
                Value::Struct(struct_value) => (struct_value.fields.get(name).expect("Struct has no such value").clone(), None),
                _ => panic!("Tried to do member access on a value that is not a struct or a pointer to struct")
            }
        },
    }
}

/// Returns the "stack"
fn get_toplevel_sym_table() -> Memory {
    let sym_table = SymTable::new(None);
    let mut stack = Memory::new(sym_table);
    for (sym, val) in get_builtin_function_values() {
        let addr = stack.push(val);
        stack.sym_table.symbols.insert(sym, addr);
    }
    stack
}

pub fn interpret_program(module: &Module<UserDefinedFunction, Struct>) -> Value {
    let mut stack = get_toplevel_sym_table();
    
    for func in &module.functions {
        stack.create(Symbol::Identifier(func.id.clone()), Value::Function(Function::UserDefined(Rc::new(func.clone()))));
    }

    let main_ref = ASTNode::new(
        Expr::Identifier { value: String::from("main") },
        SourceLocation::at(0, 0),
        SourceLocation::at(0, 0)
    );

    let return_value = eval_call_expression(&Box::new(main_ref), &vec![], &mut stack);

    eprintln!("stack len = {}", stack.stack.len());

    return_value.0
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::{tokenize, Token};
    use crate::parser::parse;
    use super::*;

    fn i(src: &str) -> Value {
        let tokens: Vec<Token> = tokenize(src).expect("Should've been able to tokenize the source");
        let module = parse(tokens).expect("Should've been able to parse the source");
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
        
        var x: int = 3;
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
    fn can_pass_function_pointer_as_arg() {
        let res = i("
        fun sign(x: Int): Int { if x > 0 then 1 else -1 }

        var func_pointer = &sign;
        
        fun call(f: Unknown, value: Unknown): Unknown {
            var f = *f;
            f(value)
        }

        call(func_pointer, -87)
        ");
        if let Value::Integer(i) = res {
            assert_eq!(i, -1);
        } else {
            panic!("Wrong, got {:?}", res)
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
}
