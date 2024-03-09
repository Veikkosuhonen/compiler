use std::collections::HashMap;

use crate::{builtin_functions::{get_builtin_function_ir_vars, get_builtin_type_constructor_ir_vars}, parser::{Expr, Module}, tokenizer::Op, type_checker::{Type, TypedASTNode, TypedStruct, TypedUserDefinedFunction}};

#[derive(Clone, Debug)]
pub struct IRVar {
    pub name: String,
    pub var_type: Type
}

impl IRVar {
    fn new(name: String, var_type: Type) -> IRVar {
        IRVar { name, var_type }
    }

    fn unit() -> IRVar {
        IRVar { name: String::from("U"), var_type: Type::Unit }
    }

    fn to_string(&self) -> String {
        self.name.to_string()
    }

    pub fn size(&self) -> usize {
        match &self.var_type {
            Type::Integer => 8,
            Type::Boolean => 8,
            _ => todo!(),
        }
    }
}

struct IRVarTable {
    vars: HashMap<String, IRVar>,
    var_idx: usize,
    label_idx: usize,
    fun_name: String,
}

impl IRVarTable {
    fn new(fun_name: String) -> IRVarTable {
        IRVarTable { vars: HashMap::new(), var_idx: 0, label_idx: 0, fun_name }
    }

    fn unconflicting_name(&mut self, mut name: String) -> String {
        if self.vars.contains_key(&name) {
            name = format!("{name}0");
        }
        name
    }

    fn create(&mut self, name: String, var_type: Type) -> IRVar {
        IRVar::new(self.unconflicting_name(name), var_type)
    }

    fn create_unnamed(&mut self, var_type: Type) -> IRVar {
        let var = IRVar::new(self.unconflicting_name(format!("var_{}", self.var_idx)), var_type);
        self.var_idx += 1;
        var
    }

    fn get(&self, name: &String) -> IRVar {
        self.vars.get(name).expect(&format!("IRVar '{}' should be defined", name.to_string())).clone()
    }

    fn create_local_label(&mut self) -> String {
        let label = format!(".L{}_{}", self.fun_name, self.label_idx);
        self.label_idx += 1;
        label
    }
}

#[derive(Debug)]
pub enum Instr {
    LoadIntConst { value: i32, dest: Box<IRVar> },
    LoadBoolConst { value: bool, dest: Box<IRVar> },
    Copy { source: Box<IRVar>, dest: Box<IRVar>, pointer_nesting: usize },
    Call { fun: Box<IRVar>, args: Vec<Box<IRVar>>, dest: Box<IRVar> },
    Jump(String),
    CondJump { cond: Box<IRVar>, then_label: String, else_label: String },
    FunctionLabel { name: String, params: Vec<IRVar> },
    Label(String),
}

#[derive(Debug)]
pub struct IREntry {
    // location: SourceLocation,
    pub instruction: Instr
}

impl IREntry {
    fn copy(src: IRVar, dest: IRVar, pointer_nesting: usize) -> IREntry {
        IREntry { instruction: Instr::Copy { source: Box::new(src), dest: Box::new(dest), pointer_nesting } }
    }
}

impl IREntry {
    pub fn to_string(&self) -> String {
        match &self.instruction {
            Instr::Label(label) => format!("Label({})", label.clone()),
            Instr::FunctionLabel { name, params } => format!("Function({}({}))", name.clone(), params.iter().map(|p| { p.name.clone() }).collect::<Vec<String>>().join(", ")),
            Instr::Jump(label) => format!("Jump({})", label.clone()),
            Instr::CondJump { cond, else_label, then_label } => format!("CondJump({}, {}, {})", cond.to_string(), then_label.clone(), else_label.clone()),
            Instr::LoadIntConst { value, dest } => format!("LoadIntConst({}, {})", value, dest.to_string()),
            Instr::LoadBoolConst { value, dest } => format!("LoadBoolConst({}, {})", value, dest.to_string()),
            Instr::Copy { source, dest, pointer_nesting } => format!("Copy({}, {}{}{})", source.to_string(), "(".repeat(*pointer_nesting), dest.to_string(), ")".repeat(*pointer_nesting)),
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
            var_table.vars.insert(param_name.clone(), ir_var.clone());
            params.push(ir_var);
        }
        let return_var = var_table.create(String::from("_return"), function.func_type.return_type.clone());
        var_table.vars.insert(return_var.name.clone(), return_var);
        let function_ir = generate_function_ir(function, params, var_table);
        module_functions_ir.insert(function.id.to_string(), function_ir);
    }
    
    module_functions_ir
}

fn generate_function_ir(func: &TypedUserDefinedFunction, params: Vec<IRVar>, mut var_table: IRVarTable) -> Vec<IREntry> {
    let mut function_instructions: Vec<IREntry> = vec![];
    function_instructions.push(IREntry { instruction: Instr::FunctionLabel { name: func.id.clone(), params  } });
    let result = generate(&func.body, &mut function_instructions, &mut var_table, String::from("_return"));
    function_instructions.push(IREntry::copy(result, var_table.get(&String::from("_return")), 0));
    function_instructions.push(IREntry { instruction: Instr::Label(format!(".L{}_end", func.id)) });
    function_instructions
}

fn create_top_level_var_table(global_functions: &Vec<TypedUserDefinedFunction>, fun_name: &String) -> IRVarTable {
    let mut var_table = IRVarTable::new(fun_name.clone());

    for (name, var) in get_builtin_function_ir_vars() {
        var_table.vars.insert(name, var);
    }
    for (name, var) in get_builtin_type_constructor_ir_vars() {
        var_table.vars.insert(name, var);
    }

    for function in global_functions {
        let sym = function.id.clone();
        var_table.vars.insert(
            sym.clone(), 
            IRVar { name: sym, var_type: Type::Function(Box::new(function.func_type.clone())) }
        );
    }

    var_table
}

fn generate(node: &TypedASTNode, instructions: &mut Vec<IREntry>, var_table: &mut IRVarTable, dest_name: String) -> IRVar {
    match &node.expr {
        Expr::Unit => IRVar::unit(),
        Expr::IntegerLiteral { value } => {
            let dest = var_table.create_unnamed(node.node_type.clone());
            instructions.push(IREntry { 
                // location: , 
                instruction: Instr::LoadIntConst { 
                    value: *value,
                    dest: Box::new(dest.clone()),
                }
            });
            dest
        },
        Expr::BooleanLiteral { value } => {
            let dest = var_table.create_unnamed( node.node_type.clone());
            instructions.push(IREntry { 
                // location: , 
                instruction: Instr::LoadBoolConst { 
                    value: *value,
                    dest: Box::new(dest.clone()),
                }
            });
            dest
        },
        Expr::Identifier { value } => {
            var_table.get(value)
        },
        Expr::VariableDeclaration { id, init,.. } => {
            if let Expr::Identifier { value } = &id.expr {
                let init = generate(&init, instructions, var_table, value.clone());
                var_table.vars.insert(value.clone(), init.clone());
                init
            } else {
                panic!("Id of a variable declaration must be an Identifier")
            }
        },
        Expr::Unary { operand, operator } => {
            let dest = var_table.create_unnamed(node.node_type.clone());
            let operand = generate(&operand, instructions, var_table, dest.name.clone());
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
            let dest = var_table.create_unnamed(node.node_type.clone());
            let fun = var_table.get(&operator.to_string());
            
            match fun.name.as_str() {
                "and" => {
                    let left = generate(&left, instructions, var_table, dest_name.clone());
                    let return_left_label = var_table.create_local_label();
                    let end_label = var_table.create_local_label();
                    let return_right_label = var_table.create_local_label();
                    instructions.push(IREntry { instruction: Instr::CondJump { cond: Box::new(left.clone()), then_label: return_right_label.clone(), else_label: return_left_label.clone() } });
                    instructions.push(IREntry { instruction: Instr::Label(return_left_label) });
                    instructions.push(IREntry::copy(left, dest.clone(), 0));
                    instructions.push(IREntry { instruction: Instr::Jump(end_label.clone()) });
                    instructions.push(IREntry { instruction: Instr::Label(return_right_label) });
                    let right = generate(&right, instructions, var_table, dest_name.clone());
                    instructions.push(IREntry::copy(right, dest.clone(), 0));
                    instructions.push(IREntry { instruction: Instr::Label(end_label) });
                },
                "or" => {
                    let left = generate(&left, instructions, var_table, dest_name.clone());
                    let return_left_label = var_table.create_local_label();
                    let end_label = var_table.create_local_label();
                    let return_right_label = var_table.create_local_label();
                    instructions.push(IREntry { instruction: Instr::CondJump { cond: Box::new(left.clone()), then_label: return_left_label.clone(), else_label: return_right_label.clone() } });
                    instructions.push(IREntry { instruction: Instr::Label(return_left_label) });
                    instructions.push(IREntry::copy(left, dest.clone(), 0));
                    instructions.push(IREntry { instruction: Instr::Jump(end_label.clone()) });
                    instructions.push(IREntry { instruction: Instr::Label(return_right_label) });
                    let right = generate(&right, instructions, var_table, dest_name.clone());
                    instructions.push(IREntry::copy(right, dest.clone(), 0));
                    instructions.push(IREntry { instruction: Instr::Label(end_label) });
                },
                _ => panic!("Unknown logical operation {}", fun.name)
            }

            dest
        },
        Expr::Binary { left, operator, right } => {
            let dest = var_table.create_unnamed(node.node_type.clone());
            let fun = var_table.get(&operator.to_string());
            let left = generate(&left, instructions, var_table, dest.name.clone());
            let right = generate(&right, instructions, var_table, dest.name.clone());
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
            let return_var = generate(&result, instructions, var_table, dest_name.clone());
            instructions.push(IREntry::copy(return_var.clone(), var_table.get(&String::from("_return")), 0));
            instructions.push(IREntry { instruction: Instr::Jump(format!(".L{}_end", var_table.fun_name)) });
            return_var
        },
        Expr::Assignment { left, right } => {
            let (left, pointer_nesting) = generate_assignment_left_side(left, instructions, var_table);
            let right = generate(&right, instructions, var_table, dest_name.clone());
            instructions.push(IREntry::copy(right.clone(), left, pointer_nesting));
            right
        },
        Expr::Block { statements, result } => {
            for stmnt in statements {
                generate(&stmnt, instructions, var_table, dest_name.clone());
            }
            generate(&result, instructions, var_table, dest_name)
        },
        Expr::If { condition, then_branch, else_branch } => {
            let then_label = var_table.create_local_label();
            let end_label = var_table.create_local_label();
            let else_label = if else_branch.is_some() { var_table.create_local_label() } else { end_label.clone() };
            let dest = var_table.create(dest_name.clone(), node.node_type.clone());

            let condition = generate(&condition, instructions, var_table, dest_name.clone());
            instructions.push(IREntry { instruction: Instr::CondJump { 
                cond: Box::new(condition), 
                then_label: then_label.clone(),
                else_label: else_label.clone(),
            }});

            instructions.push(IREntry { instruction: Instr::Label(then_label) });
            let then_branch = generate(&then_branch, instructions, var_table, dest_name.clone());
            instructions.push(IREntry::copy(then_branch, dest.clone(), 0));

            if let Some(else_branch) = else_branch {
                instructions.push(IREntry { instruction: Instr::Jump(end_label.clone()) });
                instructions.push(IREntry { instruction: Instr::Label(else_label) });
                let else_branch = generate(&else_branch, instructions, var_table, dest_name);
                instructions.push(IREntry::copy(else_branch, dest.clone(), 0));
            }

            instructions.push(IREntry { instruction: Instr::Label(end_label) });

            dest
        },
        Expr::While { condition, body } => {
            let while_label = var_table.create_local_label();
            let do_label = var_table.create_local_label();
            let end_label = var_table.create_local_label();
            let dest = var_table.create(dest_name.clone(), node.node_type.clone());

            instructions.push(IREntry { instruction: Instr::Label(while_label.clone()) });

            let condition = generate(&condition, instructions, var_table, dest_name.clone());
            instructions.push(IREntry { instruction: Instr::CondJump { 
                cond: Box::new(condition), 
                then_label: do_label.clone(),
                else_label: end_label.clone(),
            }});

            instructions.push(IREntry { instruction: Instr::Label(do_label) });
            let body = generate(&body, instructions, var_table, dest_name);
            instructions.push(IREntry::copy(body, dest.clone(), 0));
            instructions.push(IREntry { instruction: Instr::Jump(while_label) });

            instructions.push(IREntry { instruction: Instr::Label(end_label) });

            dest
        },
        Expr::StructInstance { struct_name, fields } => todo!()
    }
}

fn generate_assignment_left_side(node: &Box<TypedASTNode>, instructions: &mut Vec<IREntry>, var_table: &mut IRVarTable) -> (IRVar, usize) {
    match &node.expr {
        Expr::Identifier { value } => (var_table.get(&value), 0),
        Expr::Unary { operand, operator: Op::Deref } => {
            let (irvar, nesting) = generate_assignment_left_side(&operand, instructions, var_table);
            (irvar, nesting + 1)
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

fn generate_constructor_call(constructor_type: Type, arguments: &Vec<Box<TypedASTNode>>, instructions: &mut Vec<IREntry>, var_table: &mut IRVarTable) -> IRVar {
    let dest = var_table.create_unnamed(constructor_type);
    let dest_name = dest.name.clone();

    // Copy args to constructor shape
    for arg in arguments {
        let arg_var = generate(&arg, instructions, var_table, dest_name.clone());
        instructions.push(IREntry::copy(arg_var, dest.clone(), 0));
    }
    
    dest
}

fn generate_function_call(fun: IRVar, return_type: Type, arguments: &Vec<Box<TypedASTNode>>, instructions: &mut Vec<IREntry>, var_table: &mut IRVarTable) -> IRVar {
    let dest = var_table.create_unnamed(return_type);
    let mut argument_vars: Vec<Box<IRVar>> = vec![];
    for arg in arguments {
        argument_vars.push(Box::new(generate(&arg, instructions, var_table, dest.name.clone())));
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
        let _ = i("
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

        // println!("{}", _ir.get("main").unwrap().iter().map(|i| i.to_string()).collect::<Vec<String>>().join("\n"))
    }

    #[test]
    fn pointer_nested_assignment() {
        let _ir = i("
            var x = 1;
            var y = &&x;
            **y = 2;
            x
        ");

        // println!("{}", _ir.get("main").unwrap().iter().map(|i| i.to_string()).collect::<Vec<String>>().join("\n"))
    }

    #[test]
    fn logical_expr() {
        let _ir = i("
            true and false
        ");

        // println!("{}", _ir.get("main").unwrap().iter().map(|i| i.to_string()).collect::<Vec<String>>().join("\n"))
    }

    #[test]
    fn heap_alloc() {
        let _ir = i("
            var x = Int(123);
        ");

        println!("{}", _ir.get("main").unwrap().iter().map(|i| i.to_string()).collect::<Vec<String>>().join("\n"))
    }
}
