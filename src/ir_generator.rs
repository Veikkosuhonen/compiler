use std::collections::HashMap;
use std::mem;

use crate::{builtin_functions::{get_builtin_function_ir_vars, get_builtin_type_constructor_ir_vars}, parser::{Expr, Module}, sym_table::SymTable, tokenizer::Op, type_checker::{Type, TypedASTNode, TypedStruct, TypedUserDefinedFunction}};

#[derive(Clone, Debug, PartialEq, Default)]
pub struct IRVar {
    pub parent: Option<Box<IRVar>>,
    pub name: String,
    pub var_type: Type
}

impl IRVar {
    pub fn new(name: String, var_type: Type) -> IRVar {
        IRVar { name, parent: None, var_type }
    }

    fn unit() -> IRVar {
        IRVar { name: String::from("U"), parent: None, var_type: Type::Unit }
    }

    pub fn to_string(&self) -> String {
        match &self.parent {
            Some(parent) => format!("{}.{}: {:?}", parent.to_short_string(), self.name.to_string(), self.var_type),
            None => format!("{}: {:?}", self.name.to_string(), self.var_type),
        }
    }

    pub fn to_short_string(&self) -> String {
        match &self.parent {
            Some(parent) => format!("{}.{}", parent.to_short_string(), self.name.to_string()),
            None => format!("{}", self.name.to_string()),
        }
    }

    pub fn size(&self) -> usize {
        self.var_type.size()
    }
}

struct IRVarTable {
    vars: Box<SymTable<String, IRVar>>,
    var_idx: usize,
    label_idx: usize,
    fun_name: String,
}

impl IRVarTable {
    fn new(fun_name: String) -> IRVarTable {
        IRVarTable { vars: SymTable::new(None), var_idx: 0, label_idx: 0, fun_name }
    }

    fn unconflicting_name(&mut self, mut name: String) -> String {
        while self.vars.symbols.contains_key(&name) {
            self.var_idx += 1;
            name = format!("var_{}", self.var_idx);
        }
        name
    }

    fn create(&mut self, name: String, var_type: Type) -> IRVar {
        let name = self.unconflicting_name(name);
        let var = IRVar::new(name.clone(), var_type);
        self.vars.declare(name, var.clone());
        var
    }

    fn create_with_parent(&mut self, name: String, parent: IRVar, var_type: Type) -> IRVar {
        IRVar { name, parent: Some(Box::new(parent)), var_type }
    }

    fn create_unnamed(&mut self, var_type: Type) -> IRVar {
        let var = self.create(format!("var_{}", self.var_idx), var_type);
        self.var_idx += 1;
        var
    }

    fn get(&self, name: &String) -> IRVar {
        self.vars.get(name).clone()
    }

    fn create_local_label(&mut self) -> String {
        let label = format!(".L{}_{}", self.fun_name, self.label_idx);
        self.label_idx += 1;
        label
    }

    /// Some copypasta from interpreter/Memory::block_scope
    pub fn block_scope(&mut self, f: impl FnOnce(&mut Self) -> IRVar) -> IRVar {
        let outer_symtab: Box<SymTable<_, _>> = mem::replace(&mut self.vars, Default::default());
        let inner_symtab = SymTable::new(Some(outer_symtab));
        let mut inner_var_table = IRVarTable { vars: inner_symtab, var_idx: self.var_idx, label_idx: self.label_idx, fun_name: self.fun_name.clone() };
        let result = f(&mut inner_var_table);
        let _ = mem::replace(&mut self.vars, inner_var_table.vars.parent.unwrap());
        result
    }
}

#[derive(Debug)]
pub enum Instr {
    LoadIntConst { value: i32, dest: Box<IRVar> },
    LoadBoolConst { value: bool, dest: Box<IRVar> },
    Copy { source: Box<IRVar>, dest: Box<IRVar> },
    Call { fun: Box<IRVar>, args: Vec<Box<IRVar>>, dest: Box<IRVar> },
    Jump(String),
    CondJump { cond: Box<IRVar>, then_label: String, else_label: String },
    FunctionLabel { name: String, params: Vec<IRVar> },
    Declare { var: Box<IRVar> },
    Label(String),
}

#[derive(Debug)]
pub struct IREntry {
    // location: SourceLocation,
    pub instruction: Instr
}

impl IREntry {
    fn copy(src: IRVar, dest: IRVar) -> IREntry {
        IREntry { instruction: Instr::Copy { source: Box::new(src), dest: Box::new(dest) } }
    }
}

impl IREntry {
    pub fn to_string(&self) -> String {
        match &self.instruction {
            Instr::Declare { var } => format!("Declare({})", var.to_string()),
            Instr::Label(label) => format!("Label({})", label.clone()),
            Instr::FunctionLabel { name, params } => format!("Function({}({}))", name.clone(), params.iter().map(|p| { p.name.clone() }).collect::<Vec<String>>().join(", ")),
            Instr::Jump(label) => format!("Jump({})", label.clone()),
            Instr::CondJump { cond, else_label, then_label } => format!("CondJump({}, {}, {})", cond.to_string(), then_label.clone(), else_label.clone()),
            Instr::LoadIntConst { value, dest } => format!("LoadIntConst({}, {})", value, dest.to_string()),
            Instr::LoadBoolConst { value, dest } => format!("LoadBoolConst({}, {})", value, dest.to_string()),
            Instr::Copy { source, dest } => format!("Copy({}, {})", source.to_string(), dest.to_string()),
            Instr::Call { fun, args, dest } => format!("Call({}, [{}], {})", fun.to_string(), args.iter().map(|arg| arg.to_string()).collect::<Vec<String>>().join(","), dest.to_string()),
        }
    }
}

pub fn generate_ir_code(ir: HashMap<String, Vec<IREntry>>) -> String {
    ir.iter().map(|(_, ir)| ir).map(|func_ir| { func_ir.iter().map(|ir| { ir.to_string() }).collect::<Vec<String>>().join("\n") }).collect::<Vec<String>>().join("\n\n")
}

pub fn generate_ir(module: Module<TypedUserDefinedFunction, TypedStruct>) -> HashMap<String, Vec<IREntry>> {

    let mut module_functions_ir: HashMap<String, Vec<IREntry>> = HashMap::new();

    for function in &module.functions {
        let mut var_table = create_top_level_var_table(&module.functions, &function.id);
        let mut params: Vec<IRVar> = vec![];
        for (idx, param_name) in function.params.iter().enumerate() {
            let param = function.func_type.param_types.get(idx).unwrap();
            let ir_var = var_table.create(param_name.clone(), param.param_type.clone());
            params.push(ir_var);
        }
        let function_ir = generate_function_ir(function, params, var_table);
        module_functions_ir.insert(function.id.to_string(), function_ir);
    }
    
    module_functions_ir
}

fn generate_function_ir(func: &TypedUserDefinedFunction, params: Vec<IRVar>, mut var_table: IRVarTable) -> Vec<IREntry> {
    let mut function_instructions: Vec<IREntry> = vec![];
    function_instructions.push(IREntry { instruction: Instr::FunctionLabel { name: func.id.clone(), params  } });
    let return_var = var_table.create(String::from("_return"), func.func_type.return_type.clone());
    let _ = generate(&func.body, &mut function_instructions, &mut var_table, Some(return_var.clone()));
    function_instructions.push(IREntry { instruction: Instr::Label(format!(".L{}_end", func.id)) });
    if func.id == "main" {
        // Return 0
        function_instructions.push(IREntry { instruction: Instr::LoadIntConst { value: 0, dest: Box::new(return_var) } });
    }
    function_instructions
}

fn create_top_level_var_table(global_functions: &Vec<TypedUserDefinedFunction>, fun_name: &String) -> IRVarTable {
    let mut var_table = IRVarTable::new(fun_name.clone());

    for (name, var) in get_builtin_function_ir_vars() {
        var_table.vars.declare(name, var);
    }
    for (name, var) in get_builtin_type_constructor_ir_vars() {
        var_table.vars.declare(name, var);
    }

    for function in global_functions {
        let sym = function.id.clone();
        var_table.vars.declare(
            sym.clone(), 
            IRVar::new(sym, Type::Function(Box::new(function.func_type.clone())))
        );
    }

    var_table
}

fn generate(node: &TypedASTNode, instructions: &mut Vec<IREntry>, var_table: &mut IRVarTable, dest: Option<IRVar>) -> IRVar {
    let dest = dest.unwrap_or_else(|| var_table.create_unnamed(node.node_type.clone()));

    match &node.expr {
        Expr::Unit => IRVar::unit(),
        Expr::IntegerLiteral { value } => {
            instructions.push(IREntry { 
                // location: , 
                instruction: Instr::LoadIntConst { 
                    value: *value,
                    dest: Box::new(dest.clone()),
                }
            });
            dest.clone()
        },
        Expr::BooleanLiteral { value } => {
            instructions.push(IREntry { 
                // location: , 
                instruction: Instr::LoadBoolConst { 
                    value: *value,
                    dest: Box::new(dest.clone()),
                }
            });
            dest.clone()
        },
        Expr::Identifier { value } => {
            let src = var_table.get(value);
            src
        },
        Expr::VariableDeclaration { id, init,.. } => {
            if let Expr::Identifier { value } = &id.expr {
                let init = generate(&init, instructions, var_table, None);
                var_table.vars.declare(value.clone(), init.clone());
                init
            } else {
                panic!("Id of a variable declaration must be an Identifier")
            }
        },
        Expr::Unary { operand, operator } => {
            // If the operator is not addressof, we can greedily evaluate the operand value to the result address.
            // If its an AddressOf op, we can not overwrite the operand value.
            let op_dest = if *operator == Op::AddressOf { None } else { Some(dest.clone()) };
            let operand = generate(&operand, instructions, var_table, op_dest);

            // Special case for Deref operator. Treat it as a IR ref to the .value member of the pointer.
            if *operator == Op::Deref {
                return var_table.create_with_parent(String::from("value"), operand, node.node_type.clone());
            }

            let fun = var_table.get(&operator.to_string());
            instructions.push(IREntry { 
                // location: , 
                instruction: Instr::Call { 
                    fun: Box::new(fun), 
                    args: vec![Box::new(operand)], 
                    dest: Box::new(dest.clone()),
                }
            });
            dest
        },
        Expr::Logical { left, operator, right } => {
            let fun = var_table.get(&operator.to_string());
            
            match fun.name.as_str() {
                "and" => {
                    let left = generate(&left, instructions, var_table, Some(dest.clone()));
                    let return_left_label = var_table.create_local_label();
                    let end_label = var_table.create_local_label();
                    let return_right_label = var_table.create_local_label();
                    instructions.push(IREntry { instruction: Instr::CondJump { cond: Box::new(left.clone()), then_label: return_right_label.clone(), else_label: return_left_label.clone() } });
                    instructions.push(IREntry { instruction: Instr::Label(return_left_label) });
                    instructions.push(IREntry::copy(left, dest.clone()));
                    instructions.push(IREntry { instruction: Instr::Jump(end_label.clone()) });
                    instructions.push(IREntry { instruction: Instr::Label(return_right_label) });
                    let right = generate(&right, instructions, var_table, None);
                    instructions.push(IREntry::copy(right, dest.clone()));
                    instructions.push(IREntry { instruction: Instr::Label(end_label) });
                },
                "or" => {
                    let left = generate(&left, instructions, var_table, Some(dest.clone()));
                    let return_left_label = var_table.create_local_label();
                    let end_label = var_table.create_local_label();
                    let return_right_label = var_table.create_local_label();
                    instructions.push(IREntry { instruction: Instr::CondJump { cond: Box::new(left.clone()), then_label: return_left_label.clone(), else_label: return_right_label.clone() } });
                    instructions.push(IREntry { instruction: Instr::Label(return_left_label) });
                    instructions.push(IREntry::copy(left, dest.clone()));
                    instructions.push(IREntry { instruction: Instr::Jump(end_label.clone()) });
                    instructions.push(IREntry { instruction: Instr::Label(return_right_label) });
                    let right = generate(&right, instructions, var_table, None);
                    instructions.push(IREntry::copy(right, dest.clone()));
                    instructions.push(IREntry { instruction: Instr::Label(end_label) });
                },
                _ => panic!("Unknown logical operation {}", fun.name)
            }

            dest.clone()
        },
        Expr::Binary { left, operator, right } => {
            let fun = var_table.get(&operator.to_string());
            let left = generate(&left, instructions, var_table, Some(dest.clone()));
            let right = generate(&right, instructions, var_table, None);
            instructions.push(IREntry { 
                // location: , 
                instruction: Instr::Call { 
                    fun: Box::new(fun), 
                    args: vec![Box::new(left), Box::new(right)], 
                    dest: Box::new(dest.clone()),
                }
            });
            
            dest
        },
        Expr::Call { callee, arguments } => generate_call(callee, arguments, instructions, var_table),
        Expr::Return { result } => {
            let return_var = generate(&result, instructions, var_table, Some(dest));
            instructions.push(IREntry::copy(return_var.clone(), var_table.get(&String::from("_return"))));
            instructions.push(IREntry { instruction: Instr::Jump(format!(".L{}_end", var_table.fun_name)) });
            return_var
        },
        Expr::Assignment { left, right } => {
            let left = generate_assignment_left_side(left, instructions, var_table);
            let right = generate(&right, instructions, var_table, None);
            instructions.push(IREntry::copy(right.clone(), left));
            right
        },
        Expr::Block { statements, result } => {
            let result = var_table.block_scope(|inner_var_table| {
                for stmnt in statements {
                    generate(&stmnt, instructions, inner_var_table, None);
                }
                generate(&result, instructions, inner_var_table, Some(dest.clone()))
            });
            instructions.push(IREntry::copy(result, dest.clone()));
            dest
        },
        Expr::If { condition, then_branch, else_branch } => {
            let then_label = var_table.create_local_label();
            let end_label = var_table.create_local_label();
            let else_label = if else_branch.is_some() { var_table.create_local_label() } else { end_label.clone() };

            let condition = generate(&condition, instructions, var_table, None);
            instructions.push(IREntry { instruction: Instr::CondJump { 
                cond: Box::new(condition), 
                then_label: then_label.clone(),
                else_label: else_label.clone(),
            }});

            instructions.push(IREntry { instruction: Instr::Label(then_label) });
            let then_branch = generate(&then_branch, instructions, var_table, Some(dest.clone()));
            instructions.push(IREntry::copy(then_branch, dest.clone()));

            if let Some(else_branch) = else_branch {
                instructions.push(IREntry { instruction: Instr::Jump(end_label.clone()) });
                instructions.push(IREntry { instruction: Instr::Label(else_label) });
                let else_branch = generate(&else_branch, instructions, var_table, Some(dest.clone()));
                instructions.push(IREntry::copy(else_branch, dest.clone()));
            }

            instructions.push(IREntry { instruction: Instr::Label(end_label) });

            dest.clone()
        },
        Expr::While { condition, body } => {
            let while_label = var_table.create_local_label();
            let do_label = var_table.create_local_label();
            let end_label = var_table.create_local_label();

            instructions.push(IREntry { instruction: Instr::Label(while_label.clone()) });

            let condition = generate(&condition, instructions, var_table, None);
            instructions.push(IREntry { instruction: Instr::CondJump { 
                cond: Box::new(condition), 
                then_label: do_label.clone(),
                else_label: end_label.clone(),
            }});

            instructions.push(IREntry { instruction: Instr::Label(do_label) });
            let body = generate(&body, instructions, var_table, None);
            instructions.push(IREntry::copy(body, dest.clone()));
            instructions.push(IREntry { instruction: Instr::Jump(while_label) });

            instructions.push(IREntry { instruction: Instr::Label(end_label) });

            dest.clone()
        },
        Expr::StructInstance { fields,.. } => {
            let constructor_type = node.node_type.clone();
            generate_named_params_constructor_call(constructor_type, fields, instructions, var_table)
        },
        Expr::Member { parent, name } => {
            let parent_var = match &parent.expr {
                Expr::Identifier { value } => var_table.get(value),
                _ => generate(&parent, instructions, var_table, None)
            };

            let source = var_table.create_with_parent(name.clone(), parent_var, node.node_type.clone());
            // instructions.push(IREntry::copy(source, dest.clone()));
            source // dest.clone()
        }
    }
}

fn generate_assignment_left_side(node: &Box<TypedASTNode>, instructions: &mut Vec<IREntry>, var_table: &mut IRVarTable) -> IRVar {
    match &node.expr {
        Expr::Identifier { value } => {
            let var = var_table.get(&value);
            // eprintln!("Assigning to {}", var.name);
            var
        },
        Expr::Unary { operand, operator: Op::Deref } => {
            let pointer_var = generate_assignment_left_side(&operand, instructions, var_table);
            // eprintln!("{:?}", pointer_var);
            if let Type::Pointer(pointer_type) = &pointer_var.var_type {
                var_table.create_with_parent(String::from("value"), pointer_var.clone(), *pointer_type.clone())
            } else {
                unreachable!()
            }
        },
        Expr::Member { parent, name } => {
            let parent_var = generate_assignment_left_side(&parent, instructions, var_table);
            let dest = var_table.create_with_parent(name.clone(), parent_var.clone(), node.node_type.clone());
            // eprintln!("Assigning to struct member {}", dest.to_string());
            dest
        },
        _ => panic!("Encountered invalid assignment left side when generating IR: {:?}", node)
    }
}

fn generate_call(callee: &Box<TypedASTNode>, arguments: &Vec<Box<TypedASTNode>>, instructions: &mut Vec<IREntry>, var_table: &mut IRVarTable) -> IRVar {
    if let Expr::Identifier { value } = &callee.expr {
        let fun = var_table.get(value);
        let fun_type = fun.var_type.get_callable_type();

        if let Type::Constructor(ctype) = fun_type.return_type {
            generate_constructor_call(*ctype, arguments, instructions, var_table)
        } else {
            generate_function_call(fun, fun_type.return_type, arguments, instructions, var_table)
        }
    } else {
        panic!("Callee must be an identifier");
    }
}

fn generate_named_params_constructor_call(constructor_type: Type, arguments: &Vec<(String, Box<TypedASTNode>)>, instructions: &mut Vec<IREntry>, var_table: &mut IRVarTable) -> IRVar {
    if let Type::Constructor(constructed_type) = constructor_type {
        let func_type = constructed_type.get_constructor_type();

        // Struct var
        let struct_var = var_table.create_unnamed(*constructed_type.clone());
        instructions.push(IREntry { instruction: Instr::Declare { var: Box::new(struct_var.clone()) } });


        // Generate args
        for param in func_type.param_types {
            let (_, arg_node) = arguments.iter().find(|(name, _)| *name == param.name).unwrap();

            let field_var = var_table.create_with_parent(param.name.clone(), struct_var.clone(), param.param_type.clone());
            let arg_var = generate(arg_node, instructions, var_table, Some(field_var.clone()));
            instructions.push(IREntry::copy(arg_var, field_var));
        }
        
        struct_var
    } else {
        unreachable!()
    }
}

fn generate_constructor_call(constructor_type: Type, arguments: &Vec<Box<TypedASTNode>>, instructions: &mut Vec<IREntry>, var_table: &mut IRVarTable) -> IRVar {
    let dest = var_table.create_unnamed(constructor_type);

    // Generate args
    for arg in arguments {
        let arg_var = generate(&arg, instructions, var_table, None);
        instructions.push(IREntry::copy(arg_var, dest.clone()));
    }
    
    dest
}

fn generate_function_call(fun: IRVar, return_type: Type, arguments: &Vec<Box<TypedASTNode>>, instructions: &mut Vec<IREntry>, var_table: &mut IRVarTable) -> IRVar {
    let dest = var_table.create_unnamed(return_type);
    let mut argument_vars: Vec<Box<IRVar>> = vec![];
    for arg in arguments {
        argument_vars.push(Box::new(generate(&arg, instructions, var_table, None)));
    }
    instructions.push(IREntry { 
        // location: , 
        instruction: Instr::Call { 
            fun: Box::new(fun), 
            args: argument_vars, 
            dest: Box::new(dest.clone()),
        }
    });
    dest
}

#[cfg(test)]
mod tests {
    use crate::{parse_source, type_checker::typecheck_program};

    use super::*;

    fn i(src: &str) -> HashMap<String, Vec<IREntry>> {
        let a = parse_source(src.to_string());
        let t = typecheck_program(a);
        generate_ir(t)
    }

    #[test]
    fn literals() {
        let ir = i("789");
        assert!(matches!(&ir.get("main").unwrap()[1].instruction, Instr::LoadIntConst { value: 789, .. }));
        let ir2 = i("true");
        assert!(matches!(&ir2.get("main").unwrap()[1].instruction, Instr::LoadBoolConst { value: true, .. }))
    }

    #[test]
    fn var_declaration() {
        let ir = i("{ var x = 789 }");
        assert!(matches!(&ir.get("main").unwrap()[1].instruction, Instr::LoadIntConst { value: 789, .. }));
    }

    #[test]
    fn function() {
        let _ir = i("
            fun f(x: Int): Int {
                x + 1
            }

            print_int(f(41));
        ");
    }

    #[test]
    fn address_of_op() {
        let _ir = i("
            var x = 1;
            &x
        ");
    }

    #[test]
    fn pointer_nested_assignment() {
        let _ir = i("
            var x = 1;
            var y = &&x;
            **y = 2;
            x
        ");
    }

    #[test]
    fn shadowing() {
        let _ir = i("
        var x = 0;
        {
            var x = 1;
            print_int(x);
        }
        print_int(x);
        ");

        // println!("{}", _ir.get("main").unwrap().iter().map(|i| i.to_string()).collect::<Vec<String>>().join("\n"))
    }

    #[test]
    fn logical_expr() {
        let _ir = i("
            true and false
        ");
    }

    #[test]
    fn heap_alloc() {
        let _ir = i("
            var x = new Int(123);
        ");
    }

    #[test]
    fn struct_instance() {
        let _ir = i("
            struct Dog { size: Int, isHungry: Bool }
            var doggo = new Dog { isHungry: false, size: 100 };
            var hungry_doggo = new Dog { isHungry: true, size: 200 };
            hungry_doggo
        ");

        // println!("{}", _ir.get("main").unwrap().iter().map(|i| i.to_string()).collect::<Vec<String>>().join("\n"))
    }

    #[test]
    fn struct_member_access() {
        let _ir = i("
            struct Tail { length: Int }
            struct Dog { size: Int, isHungry: Bool, tail: Tail* }
            var doggo = new Dog { isHungry: false, size: 100, tail: new Tail { length: 30 } };
            doggo.tail.length
        ");

        // println!("{}", _ir.get("main").unwrap().iter().map(|i| i.to_string()).collect::<Vec<String>>().join("\n"))
    }

    #[test]
    fn correct_arg_vars() {
        let _ir = i("
                print_int(1 + 2);
                ");

        // println!("{}", _ir.get("main").unwrap().iter().map(|i| i.to_string()).collect::<Vec<String>>().join("\n"))
    }

    #[test]
    fn struct_created_by_function() {
        let _ir = i("
            struct Dog { size: Int, isHungry: Bool }
            fun create_dog(size: Int, isHungry: Bool): Dog* {
                new Dog { size: size, isHungry: isHungry }
            }
            var doggo = create_dog(100, false);
            doggo
        ");

        // println!("{}", _ir.get("main").unwrap().iter().map(|i| i.to_string()).collect::<Vec<String>>().join("\n"))
    }

    #[test]
    fn assign_to_struct_member() {
        let _ir = i("
            struct Dog { size: Int, isHungry: Bool }
            var doggo = new Dog { size: 100, isHungry: false };
            doggo.size = 200;
        ");

        // println!("{}", _ir.get("main").unwrap().iter().map(|i| i.to_string()).collect::<Vec<String>>().join("\n"))
    }

    #[test]
    fn assign_to_nested_struct() {
        let _ir = i("
            struct Tail { length: Int }
            struct Dog { size: Int, isHungry: Bool, tail: Tail* }
            var doggo = new Dog { size: 100, isHungry: false, tail: new Tail { length: 10 } }
            doggo.tail.length = 20;
        ");

        // println!("{}", _ir.get("main").unwrap().iter().map(|i| i.to_string()).collect::<Vec<String>>().join("\n"))
    }

    #[test]
    fn regression_1() {
        let _ir = i("
            var x: Int* = &3;
            var y: Int* = &4;            
            *x = *x * *y;
            print_int(*x); // Prints 12
        ");
        // println!("{}", _ir.get("main").unwrap().iter().map(|i| i.to_string()).collect::<Vec<String>>().join("\n"))
    }

    #[test]
    fn struct_member_addr() {
        let _ir = i("
            struct Dog { iq: Int }
            var dog = new Dog { iq: 0 };
            var iq: Int* = &dog.iq;
            *iq = 10;
        ");
        // println!("{}", _ir.get("main").unwrap().iter().map(|i| i.to_string()).collect::<Vec<String>>().join("\n"))
    }
}
