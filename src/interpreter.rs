use std::mem;
use std::rc::Rc;
use std::vec;

use crate::sym_table::{SymTable, Symbol};
use crate::parser::{ASTNode, Expr, Module};
use crate::tokenizer::{Op, SourceLocation};
use crate::builtin_functions::*;

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub param_type: Box<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct UserDefinedFunction {
    pub id: String,
    pub body: Box<ASTNode>,
    pub params: Vec<Param>,
    pub return_type: Box<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum Function {
    BuiltIn(BuiltIn),
    UserDefined(Rc<UserDefinedFunction>),
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
pub struct Address {
    pub addr: usize,
}

pub struct Stack {
    pub sym_table: Box<SymTable<Address>>,
    pub memory: Vec<Value>,
}

impl Stack {
    pub fn new(sym_table: Box<SymTable<Address>>) -> Stack {
        let mut stack = Stack { sym_table, memory: vec![] };
        stack.push(Value::Unit);
        stack
    }

    pub fn create(&mut self, sym: Symbol, val: Value) {
        let addr = self.push(val);
        self.sym_table.symbols.insert(sym, addr);
    }

    pub fn push(&mut self, val: Value) -> Address {
        let addr = self.memory.len();
        self.memory.push(val);
        Address { addr }
    }

    pub fn get_addr(&self, addr: &Address) -> &Value {
        &self.memory[addr.addr]
    }

    pub fn get(&self, sym: Symbol) -> &Value {
        let addr = self.sym_table.get_ref(&sym);
        self.get_addr(addr)
    }

    pub fn unit(&self) -> Address {
        Address { addr: 0 }
    }

    /// A mess!
    pub fn block_scope(&mut self, f: impl FnOnce(&mut Self) -> Address) -> Address {
        let outer_symtab = mem::replace(&mut self.sym_table, Default::default());
        let inner_symtab = SymTable::new(Some(outer_symtab));
        let _ = mem::replace(&mut self.sym_table, inner_symtab);
        let result = f(self);
        let outer_symtab = mem::replace(&mut self.sym_table.parent, Default::default());
        let returns = self.sym_table.returns.clone();
        let _ = mem::replace(&mut self.sym_table, outer_symtab.unwrap());
        if returns.is_some() {
            self.sym_table.returns = returns;
        }
        result
    }

    pub fn function_scope(&mut self, args: &Vec<(Symbol, Address)>, f: impl FnOnce(&mut Self) -> Address) -> Address {
        let outer_symtab = mem::replace(&mut self.sym_table, Default::default());
        let mut inner_symtab = SymTable::new(Some(outer_symtab));
        for (arg_symbol, arg_val) in args {
            inner_symtab.symbols.insert(arg_symbol.clone(), arg_val.clone());
        }
        let _ = mem::replace(&mut self.sym_table, inner_symtab);
        let result = f(self);
        let outer_symtab = mem::replace(&mut self.sym_table.parent, Default::default());
        let _ = mem::replace(&mut self.sym_table, outer_symtab.unwrap());
        result
    }

    pub fn debug(&self) {
        println!("{}", self.memory.iter().enumerate().map(|(addr, v)| format!("{}| {:?}", addr, v)).collect::<Vec<String>>().join("\n"));
    }
}

fn eval_binary_op(
    left_node: &Box<ASTNode>, 
    right_node: &Box<ASTNode>, 
    operator: &Op, 
    stack: &mut Stack
) -> Address {
    let left_val = interpret(&left_node, stack);

    if let Value::Function(op_function) = stack.get(Symbol::Operator(*operator)) {
        // Lazy eval to enable short circuiting
        let eval_right = |stack: &mut Stack| { interpret(&right_node, stack) };
        match op_function {
            Function::BuiltIn(builtin) => eval_builtin_binary(*builtin, left_val, eval_right, stack),
            Function::UserDefined(_) => panic!("Not yet implemented")
        }
    } else {
        panic!("Undefined operator {:?}", operator)
    }
}

fn eval_unary_op(
    operand: &Box<ASTNode>,
    operator: &Op,
    stack: &mut Stack
) -> Address {
    let operand = interpret(&operand, stack);
    if let Value::Function(op_function) = stack.get(Symbol::Operator(*operator)) {
        match op_function {
            Function::BuiltIn(builtin) => eval_builtin_unary(*builtin, operand, stack),
            Function::UserDefined(_) => panic!("Not yet implemented")
        }
    } else {
        panic!("Undefined operator {:?}", operator)
    }
}

fn eval_call_expression(
    callee: &Box<ASTNode>,
    argument_expr: &Vec<Box<ASTNode>>,
    stack: &mut Stack
) -> Address {
    if let Expr::Identifier { value: function_id } = &callee.expr {
        // This is horribly inefficient, we might be cloning a massive function.
        let called_function = stack.get(Symbol::Identifier(function_id.to_string())).clone();
        if let Value::Function(called_function) = called_function {
            match called_function {
                Function::BuiltIn(builtin) => {
                    let builtin = builtin.clone(); // Clone here so no more immutable ref
                    let mut args: Vec<Address> = vec![];
                    for expr in argument_expr {
                        let addr = interpret(&expr, stack);
                        args.push(addr);
                    }
                    eval_builtin_function(builtin, args, stack)
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
    stack: &mut Stack
) -> Address {
    let mut args: Vec<Address> = vec![];
    for expr in argument_expr {
        args.push(interpret(&expr, stack));
    }

    let mut named_arguments: Vec<(Symbol, Address)> = vec![];

    for (idx, param) in function.params.iter().enumerate() {
        named_arguments.push((
            Symbol::Identifier(param.name.to_string()),
            args.get(idx).expect("Argument length to match param list length").clone()
        ))
    }

    stack.function_scope(&named_arguments, |inner| {
        let implicit_return = interpret(&function.body, inner);
        inner.sym_table.returns.as_ref().unwrap_or(&implicit_return).clone()
    })
}

fn interpret(node: &ASTNode, stack: &mut Stack) -> Address {
    match &node.expr {
        Expr::IntegerLiteral { value } => {
            stack.push(Value::Integer(*value))
        },
        Expr::BooleanLiteral { value } => {
            stack.push(Value::Boolean(*value))
        }
        Expr::Binary { left, operator, right } => {
            eval_binary_op(left, right, operator, stack)
        },
        Expr::Unary { operand, operator } => {
            eval_unary_op(operand, operator, stack)
        }
        Expr::If { condition, then_branch, else_branch } => {
            let condition_result = interpret(&condition, stack);
            match stack.get_addr(&condition_result) {
                Value::Boolean(condition_val) => {
                    if *condition_val {
                        interpret(&then_branch, stack)
                    } else if let Some(else_branch) = else_branch {
                        interpret(&else_branch, stack)
                    } else {
                        stack.unit()
                    }
                },
                _ => panic!("If expression condition must be a boolean"),
            }
        },
        Expr::While { condition, body } => {
            fn eval_cond(stack: &mut Stack, cond: &Box<ASTNode>) -> Value {
                let addr = interpret(&cond, stack);
                stack.get_addr(&addr).clone()
            }
            while eval_cond(stack, condition).try_into().expect("While expression condition must return a boolean") {
                interpret(&body, stack);
                if stack.sym_table.returns.is_some() {
                    break;
                }
            }
            stack.unit()
        },
        Expr::Block { statements, result } => {
            stack.block_scope(|inner| {
                for node in statements {
                    interpret(&node, inner);
                    if inner.sym_table.returns.is_some() {
                        return inner.unit();
                    }
                }
                interpret(&result, inner)
            })
        },
        Expr::Return { result } => {
            let value = interpret(&result, stack);
            stack.sym_table.returns = Some(value);
            stack.unit()
        },
        Expr::Identifier { value } => stack.sym_table.get(&Symbol::Identifier(value.clone())),
        Expr::Assignment { left, right } => {
            match &left.expr {
                Expr::Identifier { value } => {
                    // Get where the new value is stored, and overwrite old value with that.
                    let new_value_addr = interpret(&right, stack);
                    let value_addr = stack.sym_table.get(&Symbol::Identifier(value.clone()));
                    stack.memory[value_addr.addr] = stack.memory[new_value_addr.addr].clone();
                    value_addr
                },
                Expr::Unary { operator,.. } => {
                    match operator {
                        Op::Deref => {
                            let value_addr = interpret(left, stack);
                            let new_value_addr = interpret(&right, stack);
                            println!("Deref assign to {:?} with value from {:?}", value_addr, new_value_addr);
                            stack.memory[value_addr.addr] = stack.memory[new_value_addr.addr].clone();
                            value_addr
                        },
                        _ => panic!("Left side of an assignment must be an identifier or a deref unary op"),
                    }
                },
                _ => panic!("Left side of an assignment must be an identifier or a deref unary op"),
            }
        },
        Expr::VariableDeclaration { id, init,.. } => {
            if let Expr::Identifier { value } = &id.expr {
                let init_value = interpret(&init, stack);
                stack.sym_table.symbols.insert(Symbol::Identifier(value.clone()), init_value);
                stack.unit()
            } else {
                panic!("Id of a variable declaration must be an identifier");
            }
        },
        Expr::Call { callee, arguments } => {
            eval_call_expression(callee, arguments, stack)
        },
        Expr::Unit => stack.unit(),
    }
}

/// Returns the "stack"
fn get_toplevel_sym_table() -> Stack {
    let sym_table = SymTable::new(None);
    let mut stack = Stack::new(sym_table);
    let builtins = get_builtin_function_symbol_value_mappings();
    for (symbol, val) in builtins {
        stack.create(symbol, val);
    }
    stack
}

pub fn interpret_program(module: &Module<UserDefinedFunction>) -> Value {
    let mut stack = get_toplevel_sym_table();
    
    for func in &module.functions {
        stack.create(Symbol::Identifier(func.id.clone()), Value::Function(Function::UserDefined(Rc::new(func.clone()))));
    }

    let main_ref = ASTNode::new(
        Expr::Identifier { value: String::from("main") },
        SourceLocation::at(0, 0),
        SourceLocation::at(0, 0)
    );

    let return_address = eval_call_expression(&Box::new(main_ref), &vec![], &mut stack);

    // stack.debug();

    stack.get_addr(&return_address).clone()
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
        let _res = i("
        fun sign(x: Int): Int { if x > 0 then 1 else -1 }

        var func_pointer = &sign;
        
        fun call(f: Unknown, value: Unknown): Unknown {
            var f = *f;
            f(value)
        }

        call(func_pointer, -87)
        ");
        // if let Value::Integer(i) = res {
        //     assert_eq!(i, -1);
        // }
    }

    #[test]
    fn can_heap_alloc() {
        let _res = i("
        var x: Int* = new Int(123);
        print_int(*x);
        delete x;
        ");
        // if let Value::Integer(i) = res {
        //     assert_eq!(i, -1);
        // }
    }
}
