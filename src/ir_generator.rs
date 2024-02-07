use std::collections::HashMap;

use crate::{builtin_functions::get_builtin_function_ir_vars, parser::Expression, type_checker::{Type, TypedASTNode}};

#[derive(Clone, Debug)]
pub struct IRVar {
    pub name: String,
    pub var_type: Type
}

impl IRVar {
    fn new(var_idx: usize, var_type: Type) -> IRVar {
        IRVar { name: format!("x{}", var_idx), var_type }
    }

    fn unit() -> IRVar {
        IRVar { name: String::from("U"), var_type: Type::Unit }
    }

    fn to_string(&self) -> String {
        self.name.clone()
    }
}

struct IRVarTable {
    vars: HashMap<String, IRVar>,
    var_idx: usize,
    label_idx: usize,
}

impl IRVarTable {
    fn new() -> IRVarTable {
        IRVarTable { vars: HashMap::new(), var_idx: 0, label_idx: 0 }
    }

    fn create(&mut self, var_type: Type) -> IRVar {
        let var = IRVar::new(self.var_idx, var_type);
        self.var_idx += 1;
        var
    }

    fn get(&self, name: &String) -> IRVar {
        self.vars.get(name).expect(&format!("IRVar '{}' should be defined", name)).clone()
    }

    fn create_label(&mut self) -> String {
        let label = format!("L{}", self.label_idx);
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
    Label(String),
    Return,
}

#[derive(Debug)]
pub struct IREntry {
    // location: SourceLocation,
    instruction: Instruction
}

impl IREntry {
    fn to_string(&self) -> String {
        match &self.instruction {
            Instruction::Return => String::from("Return()"),
            Instruction::Label(label) => format!("\nLabel({})", label.clone()),
            Instruction::Jump(label) => format!("Jump({})", label.clone()),
            Instruction::CondJump { cond, else_label, then_label } => format!("CondJump({}, {}, {})", cond.to_string(), then_label.clone(), else_label.clone()),
            Instruction::LoadIntConst { value, dest } => format!("LoadIntConst({}, {})", value, dest.to_string()),
            Instruction::LoadBoolConst { value, dest } => format!("LoadBoolConst({}, {})", value, dest.to_string()),
            Instruction::Copy { source, dest } => format!("Copy({}, {})", source.to_string(), dest.to_string()),
            Instruction::Call { fun, args, dest } => format!("Call({}, [{}], {})", fun.to_string(), args.iter().map(|arg| arg.to_string()).collect::<Vec<String>>().join(","), dest.to_string()),
        }
    }
}

pub fn generate_ir_code(ir: Vec<IREntry>) -> String {
    ir.iter().map(|ir| ir.to_string()).collect::<Vec<String>>().join("\n")
}

pub fn generate_ir(node: TypedASTNode) -> Vec<IREntry> {
    let mut instructions: Vec<IREntry> = vec![];
    instructions.push(IREntry { instruction: Instruction::Label(String::from("start")) });

    let mut var_table = IRVarTable::new();
    for (name, var) in get_builtin_function_ir_vars() {
        var_table.vars.insert(name, var);
    }

    generate(node, &mut instructions, &mut var_table);
    instructions.push(IREntry { instruction: Instruction::Return });
    instructions
}

fn generate(node: TypedASTNode, instructions: &mut Vec<IREntry>, var_table: &mut IRVarTable) -> IRVar {
    match node.expr {
        Expression::Unit => IRVar::unit(),
        Expression::IntegerLiteral { value } => {
            let dest = var_table.create(node.node_type);
            instructions.push(IREntry { 
                // location: , 
                instruction: Instruction::LoadIntConst { 
                    value,
                    dest: Box::new(dest.clone()),
                }
            });
            dest
        },
        Expression::BooleanLiteral { value } => {
            let dest = var_table.create(node.node_type);
            instructions.push(IREntry { 
                // location: , 
                instruction: Instruction::LoadBoolConst { 
                    value,
                    dest: Box::new(dest.clone()),
                }
            });
            dest
        },
        Expression::Identifier { value } => {
            var_table.get(&value)
        },
        Expression::VariableDeclaration { id, init } => {
            let init = generate(*init, instructions, var_table);
            if let Expression::Identifier { value } = id.expr {
                var_table.vars.insert(value, init.clone());
                init
            } else {
                panic!("Id of a variable declaration must be an Identifier")
            }
        },
        Expression::UnaryExpression { operand, operator } => {
            let operand = generate(*operand, instructions, var_table);
            let dest = var_table.create(node.node_type);
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
            let left = generate(*left, instructions, var_table);
            let right = generate(*right, instructions, var_table);
            let dest = var_table.create(node.node_type);
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
            if let Expression::Identifier { value } = callee.expr {
                let mut argument_vars: Vec<Box<IRVar>> = vec![];
                for arg in arguments {
                    argument_vars.push(Box::new(generate(*arg, instructions, var_table)));
                }
                let dest = var_table.create(node.node_type);
                let fun = var_table.get(&value);

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
            let left = generate(*left, instructions, var_table);
            let right = generate(*right, instructions, var_table);
            instructions.push(IREntry {
                instruction: Instruction::Copy { source: Box::new(right.clone()), dest: Box::new(left)  }
            });
            right
        },
        Expression::BlockExpression { statements, result } => {
            for stmnt in statements {
                generate(*stmnt, instructions, var_table);
            }
            generate(*result, instructions, var_table)
        },
        Expression::IfExpression { condition, then_branch, else_branch } => {
            let then_label = var_table.create_label();
            let end_label = var_table.create_label();
            let else_label = if else_branch.is_some() { var_table.create_label() } else { end_label.clone() };
            let dest = var_table.create(node.node_type);

            let condition = generate(*condition, instructions, var_table);
            instructions.push(IREntry { instruction: Instruction::CondJump { 
                cond: Box::new(condition), 
                then_label: then_label.clone(),
                else_label: else_label.clone(),
            }});

            instructions.push(IREntry { instruction: Instruction::Label(then_label) });
            let then_branch = generate(*then_branch, instructions, var_table);
            instructions.push(IREntry { instruction: Instruction::Copy { source: Box::new(then_branch.clone()), dest: Box::new(dest.clone()) } });

            if let Some(else_branch) = else_branch {
                instructions.push(IREntry { instruction: Instruction::Jump(end_label.clone()) });
                instructions.push(IREntry { instruction: Instruction::Label(else_label) });
                let else_branch = generate(*else_branch, instructions, var_table);
                instructions.push(IREntry { instruction: Instruction::Copy { source: Box::new(else_branch.clone()), dest: Box::new(dest.clone()) } });
            }

            instructions.push(IREntry { instruction: Instruction::Label(end_label) });

            dest
        },
        Expression::WhileExpression { condition, body } => {
            let while_label = var_table.create_label();
            let do_label = var_table.create_label();
            let end_label = var_table.create_label();
            let dest = var_table.create(node.node_type);

            instructions.push(IREntry { instruction: Instruction::Label(while_label.clone()) });

            let condition = generate(*condition, instructions, var_table);
            instructions.push(IREntry { instruction: Instruction::CondJump { 
                cond: Box::new(condition), 
                then_label: do_label.clone(),
                else_label: end_label.clone(),
            }});

            instructions.push(IREntry { instruction: Instruction::Label(do_label) });
            let body = generate(*body, instructions, var_table);
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

    fn i(src: &str) -> Vec<IREntry> {
        let a = parse_source(src.to_string());
        let t = typecheck_program(a);
        generate_ir(t)
    }

    #[test]
    fn literals() {
        let ir = i("789");
        assert!(matches!(&ir[1].instruction, Instruction::LoadIntConst { value: 789, .. }));
        let ir2 = i("true");
        assert!(matches!(&ir2[1].instruction, Instruction::LoadBoolConst { value: true, .. }))
    }

    #[test]
    fn var_declaration() {
        let ir = i("{ var x = 789 }");
        println!("{:?}", ir);
        assert!(matches!(&ir[1].instruction, Instruction::LoadIntConst { value: 789, .. }));
    }
}