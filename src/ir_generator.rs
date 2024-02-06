use std::collections::HashMap;

use crate::{parser::Expression, type_checker::{Type, TypedASTNode}};

#[derive(Clone, Debug)]
pub struct IRVar {
    name: String,
    var_type: Type
}

impl IRVar {
    fn new(var_idx: usize, var_type: Type) -> IRVar {
        IRVar { name: format!("x{}", var_idx), var_type }
    }

    fn unit() -> IRVar {
        IRVar { name: String::from("U"), var_type: Type::Unit }
    }
}

struct IRVarTable {
    vars: HashMap<String, IRVar>,
    var_idx: usize
}

impl IRVarTable {
    fn new() -> IRVarTable {
        IRVarTable { vars: HashMap::new(), var_idx: 0 }
    }

    fn init_var(&mut self, id: String, var_type: Type) -> IRVar {
        let var = self.create(var_type);
        self.vars.insert(id, var.clone());
        var
    }

    fn create(&mut self, var_type: Type) -> IRVar {
        let var = IRVar::new(self.var_idx, var_type);
        self.var_idx += 1;
        var
    }
}

#[derive(Debug)]
pub struct Label {
    name: str,
}

#[derive(Debug)]
pub enum Instruction {
    LoadIntConst { value: i32, dest: Box<IRVar> },
    LoadBoolConst { value: bool, dest: Box<IRVar> },
    Copy { source: Box<IRVar>, dest: Box<IRVar> },
    Call { fun: Box<IRVar>, args: Vec<Box<IRVar>>, dest: Box<IRVar> },
    Jump { label: Box<Label> },
    CondJump { cond: Box<IRVar>, then_label: Box<Label>, else_label: Box<Label> },
    Label(Box<Label>),
}

#[derive(Debug)]
pub struct IREntry {
    // location: SourceLocation,
    instruction: Instruction
}

pub fn generate_ir(node: TypedASTNode) -> Vec<IREntry> {
    let mut instructions: Vec<IREntry> = vec![];
    generate(node, &mut instructions, &mut IRVarTable::new());
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
            var_table.vars.get(&value).unwrap().clone()
        },
        Expression::VariableDeclaration { id, init } => {
            let init = generate(*init, instructions, var_table);
            if let Expression::Identifier { value } = id.expr {
                let dest = var_table.init_var(value, init.var_type.clone());
                instructions.push(IREntry {
                    instruction: Instruction::Copy { source: Box::new(init), dest: Box::new(dest)  }
                });
                IRVar::unit()
            } else {
                panic!("Id of a variable declaration must be an Identifier")
            }
        },
        Expression::UnaryExpression { operand, operator } => {
            /* let operand = generate(*operand, instructions, var_table);
            let dest = var_table.create(node.node_type);
            instructions.push(IREntry { 
                // location: , 
                instruction: Instruction::Call { 
                    fun: Box::new(IRVar { name: Symbol::Operator(operator) }), 
                    args: vec![Box::new(operand)], 
                    dest: Box::new(dest.clone()),
                }
            });
            dest */
            todo!("stuff")
        },
        Expression::BinaryExpression { left, operator, right } => {
            /* let left = generate(*left, instructions, var_idx);
            let right = generate(*right, instructions, var_idx);
            let dest = IRVar::new(var_idx);
            instructions.push(IREntry { 
                // location: , 
                instruction: Instruction::Call { 
                    fun: Box::new(IRVar { name: Symbol::Operator(operator) }), 
                    args: vec![Box::new(left), Box::new(right)], 
                    dest: Box::new(dest.clone()),
                }
            });
            dest */
            todo!("stuff")
        },
        Expression::CallExpression { callee, arguments } => {
            todo!("stuff")
        },
        Expression::AssignmentExpression { left, right } => {
            todo!("stuff")
        },
        Expression::BlockExpression { statements, result } => {
            for stmnt in statements {
                generate(*stmnt, instructions, var_table);
            }
            generate(*result, instructions, var_table)
        },
        Expression::IfExpression { condition, then_branch, else_branch } => {
            // instructions.push(Instruction::CondJump);
            todo!("stuff")
        },
        Expression::WhileExpression { condition, body } => {
            todo!("stuff")
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
        assert!(matches!(&ir[0].instruction, Instruction::LoadIntConst { value: 789, .. }));
        let ir2 = i("true");
        assert!(matches!(&ir2[0].instruction, Instruction::LoadBoolConst { value: true, .. }))
    }

    #[test]
    fn var_declaration() {
        let ir = i("{ var x = 789 }");
        println!("{:?}", ir);
        assert!(matches!(&ir[0].instruction, Instruction::LoadIntConst { value: 789, .. }));
    }
}