use crate::parser::Expression;

pub enum Instruction {
    LoadIntConst,
    CopyInstruction,
    Label,
    Jump,
    CondJump,
}

fn generate(node: Expression, mut instructions: Vec<Instruction>) -> Vec<Instruction> {
    match node {
        Expression::IfExpression { condition, then_branch, else_branch } => {
            instructions.push(Instruction::CondJump);
        },
        _ => todo!("Stuff")
    }
    instructions
}

pub fn generate_ir(node: Expression) -> Vec<Instruction> {
    let mut instructions: Vec<Instruction> = vec![];
    generate(node, instructions)
}
