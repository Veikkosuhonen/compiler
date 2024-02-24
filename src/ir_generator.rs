use std::collections::HashMap;

use crate::{builtin_functions::get_builtin_function_ir_vars, parser::{Expression, Module}, type_checker::{Type, TypedASTNode, TypedUserDefinedFunction}};

#[derive(Clone, Debug)]
pub struct IRVar {
    pub name: String,
    pub var_type: Type
}

impl IRVar {
    fn new(var_idx: usize, var_type: Type) -> IRVar {
        IRVar { name: format!("x{}", var_idx), var_type }
    }

    fn new_param(var_idx: usize, var_type: Type) -> IRVar {
        IRVar { name: format!("p{}", var_idx), var_type }
    }

    fn unit() -> IRVar {
        IRVar { name: String::from("U"), var_type: Type::Unit }
    }

    fn to_string(&self) -> String {
        self.name.to_string()
    }
}

struct IRVarTable {
    vars: HashMap<String, IRVar>,
    var_idx: usize,
    param_idx: usize,
    label_idx: usize,
    fun_name: String,
}

impl IRVarTable {
    fn new(fun_name: String) -> IRVarTable {
        IRVarTable { vars: HashMap::new(), var_idx: 0, param_idx: 0, label_idx: 0, fun_name }
    }

    fn create(&mut self, var_type: Type) -> IRVar {
        let var = IRVar::new(self.var_idx, var_type);
        self.var_idx += 1;
        var
    }

    fn create_param(&mut self, var_type: Type) -> IRVar {
        if self.param_idx >= 5 {
            panic!("Max number of function params is 6")
        }
        let param = IRVar::new_param(self.param_idx, var_type);
        self.param_idx += 1;
        param
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
pub enum Instruction {
    LoadIntConst { value: i32, dest: Box<IRVar> },
    LoadBoolConst { value: bool, dest: Box<IRVar> },
    Copy { source: Box<IRVar>, dest: Box<IRVar> },
    Call { fun: Box<IRVar>, args: Vec<Box<IRVar>>, dest: Box<IRVar> },
    Jump(String),
    CondJump { cond: Box<IRVar>, then_label: String, else_label: String },
    FunctionLabel(String),
    Label(String),
    Return { source: Box<IRVar> },
}

#[derive(Debug)]
pub struct IREntry {
    // location: SourceLocation,
    pub instruction: Instruction
}

impl IREntry {
    pub fn to_string(&self) -> String {
        match &self.instruction {
            Instruction::Return { source } => format!("Return({})", source.to_string()),
            Instruction::Label(label) => format!("Label({})", label.clone()),
            Instruction::FunctionLabel(label) => format!("Function({})", label.clone()),
            Instruction::Jump(label) => format!("Jump({})", label.clone()),
            Instruction::CondJump { cond, else_label, then_label } => format!("CondJump({}, {}, {})", cond.to_string(), then_label.clone(), else_label.clone()),
            Instruction::LoadIntConst { value, dest } => format!("LoadIntConst({}, {})", value, dest.to_string()),
            Instruction::LoadBoolConst { value, dest } => format!("LoadBoolConst({}, {})", value, dest.to_string()),
            Instruction::Copy { source, dest } => format!("Copy({}, {})", source.to_string(), dest.to_string()),
            Instruction::Call { fun, args, dest } => format!("Call({}, [{}], {})", fun.to_string(), args.iter().map(|arg| arg.to_string()).collect::<Vec<String>>().join(","), dest.to_string()),
        }
    }
}

pub fn generate_ir_code(ir: HashMap<String, Vec<IREntry>>) -> String {
    ir.iter().map(|(_, ir)| ir).map(|func_ir| { func_ir.iter().map(|ir| { ir.to_string() }).collect::<Vec<String>>().join("\n") }).collect::<Vec<String>>().join("\n\n")
}

pub fn generate_ir(module: Module<TypedUserDefinedFunction, TypedASTNode>) -> HashMap<String, Vec<IREntry>> {
    let var_table = create_top_level_var_table(&module.functions, &String::from("main"));

    let mut module_functions_ir: HashMap<String, Vec<IREntry>> = HashMap::new();

    let main_instructions = generate_function_ir(String::from("main"), &module.ast, var_table);
    module_functions_ir.insert(String::from("main"), main_instructions);

    for function in &module.functions {
        let mut var_table = create_top_level_var_table(&module.functions, &function.id);
        for (idx, param_name) in function.params.iter().enumerate() {
            let param_type = function.func_type.param_types.get(idx).unwrap();
            let ir_var = var_table.create_param(param_type.clone());
            var_table.vars.insert(param_name.clone(), ir_var);
        }
        let function_ir = generate_function_ir(function.id.clone(), &function.body, var_table);
        module_functions_ir.insert(function.id.to_string(), function_ir);
    }
    
    module_functions_ir
}

fn generate_function_ir(id: String, body: &Box<TypedASTNode>, mut var_table: IRVarTable) -> Vec<IREntry> {
    let mut function_instructions: Vec<IREntry> = vec![];
    function_instructions.push(IREntry { instruction: Instruction::FunctionLabel(id) });
    let return_var = generate(&body, &mut function_instructions, &mut var_table);
    function_instructions.push(IREntry { instruction: Instruction::Return { source: Box::new(return_var) } });
    function_instructions
}

fn create_top_level_var_table(global_functions: &Vec<TypedUserDefinedFunction>, fun_name: &String) -> IRVarTable {
    let mut var_table = IRVarTable::new(fun_name.clone());

    for (name, var) in get_builtin_function_ir_vars() {
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

fn generate(node: &TypedASTNode, instructions: &mut Vec<IREntry>, var_table: &mut IRVarTable) -> IRVar {
    match &node.expr {
        Expression::Unit => IRVar::unit(),
        Expression::IntegerLiteral { value } => {
            let dest = var_table.create(node.node_type.clone());
            instructions.push(IREntry { 
                // location: , 
                instruction: Instruction::LoadIntConst { 
                    value: *value,
                    dest: Box::new(dest.clone()),
                }
            });
            dest
        },
        Expression::BooleanLiteral { value } => {
            let dest = var_table.create(node.node_type.clone());
            instructions.push(IREntry { 
                // location: , 
                instruction: Instruction::LoadBoolConst { 
                    value: *value,
                    dest: Box::new(dest.clone()),
                }
            });
            dest
        },
        Expression::Identifier { value } => {
            var_table.get(value)
        },
        Expression::VariableDeclaration { id, init,.. } => {
            let init = generate(&init, instructions, var_table);
            if let Expression::Identifier { value } = &id.expr {
                var_table.vars.insert(value.clone(), init.clone());
                init
            } else {
                panic!("Id of a variable declaration must be an Identifier")
            }
        },
        Expression::UnaryExpression { operand, operator } => {
            let operand = generate(&operand, instructions, var_table);
            let dest = var_table.create(node.node_type.clone());
            let fun = var_table.get(&operator.to_string());

            instructions.push(IREntry { 
                // location: , 
                instruction: Instruction::Call { 
                    fun: Box::new(fun), 
                    args: vec![Box::new(operand)], 
                    dest: Box::new(dest.clone()),
                }
            });
            dest
        },
        Expression::BinaryExpression { left, operator, right } => {
            let left = generate(&left, instructions, var_table);
            let right = generate(&right, instructions, var_table);
            let dest = var_table.create(node.node_type.clone());
            let fun = var_table.get(&operator.to_string());

            instructions.push(IREntry { 
                // location: , 
                instruction: Instruction::Call { 
                    fun: Box::new(fun), 
                    args: vec![Box::new(left), Box::new(right)], 
                    dest: Box::new(dest.clone()),
                }
            });
            dest
        },
        Expression::CallExpression { callee, arguments } => {
            if let Expression::Identifier { value } = &callee.expr {
                let mut argument_vars: Vec<Box<IRVar>> = vec![];
                for arg in arguments {
                    argument_vars.push(Box::new(generate(&arg, instructions, var_table)));
                }
                let dest = var_table.create(node.node_type.clone());
                let fun = var_table.get(value);

                instructions.push(IREntry { 
                    // location: , 
                    instruction: Instruction::Call { 
                        fun: Box::new(fun), 
                        args: argument_vars, 
                        dest: Box::new(dest.clone()),
                    }
                });
                dest
            } else {
                panic!("Callee must be an identifier");
            }
        },
        Expression::AssignmentExpression { left, right } => {
            let left = generate(&left, instructions, var_table);
            let right = generate(&right, instructions, var_table);
            instructions.push(IREntry {
                instruction: Instruction::Copy { source: Box::new(right.clone()), dest: Box::new(left)  }
            });
            right
        },
        Expression::BlockExpression { statements, result } => {
            for stmnt in statements {
                generate(&stmnt, instructions, var_table);
            }
            generate(&result, instructions, var_table)
        },
        Expression::IfExpression { condition, then_branch, else_branch } => {
            let then_label = var_table.create_local_label();
            let end_label = var_table.create_local_label();
            let else_label = if else_branch.is_some() { var_table.create_local_label() } else { end_label.clone() };
            let dest = var_table.create(node.node_type.clone());

            let condition = generate(&condition, instructions, var_table);
            instructions.push(IREntry { instruction: Instruction::CondJump { 
                cond: Box::new(condition), 
                then_label: then_label.clone(),
                else_label: else_label.clone(),
            }});

            instructions.push(IREntry { instruction: Instruction::Label(then_label) });
            let then_branch = generate(&then_branch, instructions, var_table);
            instructions.push(IREntry { instruction: Instruction::Copy { source: Box::new(then_branch.clone()), dest: Box::new(dest.clone()) } });

            if let Some(else_branch) = else_branch {
                instructions.push(IREntry { instruction: Instruction::Jump(end_label.clone()) });
                instructions.push(IREntry { instruction: Instruction::Label(else_label) });
                let else_branch = generate(&else_branch, instructions, var_table);
                instructions.push(IREntry { instruction: Instruction::Copy { source: Box::new(else_branch.clone()), dest: Box::new(dest.clone()) } });
            }

            instructions.push(IREntry { instruction: Instruction::Label(end_label) });

            dest
        },
        Expression::WhileExpression { condition, body } => {
            let while_label = var_table.create_local_label();
            let do_label = var_table.create_local_label();
            let end_label = var_table.create_local_label();
            let dest = var_table.create(node.node_type.clone());

            instructions.push(IREntry { instruction: Instruction::Label(while_label.clone()) });

            let condition = generate(&condition, instructions, var_table);
            instructions.push(IREntry { instruction: Instruction::CondJump { 
                cond: Box::new(condition), 
                then_label: do_label.clone(),
                else_label: end_label.clone(),
            }});

            instructions.push(IREntry { instruction: Instruction::Label(do_label) });
            let body = generate(&body, instructions, var_table);
            instructions.push(IREntry { instruction: Instruction::Copy { source: Box::new(body.clone()), dest: Box::new(dest.clone()) } });
            instructions.push(IREntry { instruction: Instruction::Jump(while_label) });

            instructions.push(IREntry { instruction: Instruction::Label(end_label) });

            dest
        }
    }
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
        assert!(matches!(&ir.get("main").unwrap()[1].instruction, Instruction::LoadIntConst { value: 789, .. }));
        let ir2 = i("true");
        assert!(matches!(&ir2.get("main").unwrap()[1].instruction, Instruction::LoadBoolConst { value: true, .. }))
    }

    #[test]
    fn var_declaration() {
        let ir = i("{ var x = 789 }");
        println!("{:?}", ir);
        assert!(matches!(&ir.get("main").unwrap()[1].instruction, Instruction::LoadIntConst { value: 789, .. }));
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
}
