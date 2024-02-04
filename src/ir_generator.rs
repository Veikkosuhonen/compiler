use crate::{parser::Expression, tokenizer::SourceLocation};

pub struct IRVar {
    name: str,
}

pub struct Label {
    name: str,
}

pub enum Instruction {
    LoadIntConst { value: i32, dest: Box<IRVar> },
    LoadBoolConst { value: bool, dest: Box<IRVar> },
    Copy { source: Box<IRVar>, dest: Box<IRVar> },
    Call { fun: Box<IRVar>, args: Vec<Box<IRVar>>, dest: Box<IRVar> },
    Jump { label: Box<Label> },
    CondJump { cond: Box<IRVar>, then_label: Box<Label>, else_label: Box<Label> },
    Label(Box<Label>),
}

pub struct IREntry {
    location: SourceLocation,
    instruction: Instruction
}

fn generate(node: Expression, mut instructions: Vec<IREntry>) -> Vec<IREntry> {
    match node {
        Expression::IfExpression { condition, then_branch, else_branch } => {
            // instructions.push(Instruction::CondJump);
        },
        _ => todo!("Stuff")
    }
    instructions
}

pub fn generate_ir(node: Expression) -> Vec<IREntry> {
    let mut instructions: Vec<IREntry> = vec![];
    generate(node, instructions)
}
