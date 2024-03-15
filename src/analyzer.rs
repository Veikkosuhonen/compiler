use std::collections::HashMap;

use crate::ir_generator::{IREntry, Instr};

pub fn analyze(_ir: HashMap<String, Vec<IREntry>>) {}

#[derive(Clone)]
struct BasicBlock {
    entries: Vec<(usize, IREntry)>,
    label: String,
    out: Vec<String>
}

fn find_basic_blocks(ir: &Vec<IREntry>) -> Vec<BasicBlock> {
    let mut blocks = vec![];
    let mut current_entries = vec![];
    let mut current_label = String::from("");
    for (idx, entry) in ir.iter().enumerate() {
        
        match &entry.instruction {
            Instr::CondJump { else_label, then_label,.. } => {
                current_entries.push((idx, entry.clone()));
                blocks.push(BasicBlock {
                    entries: current_entries,
                    label: current_label.clone(),
                    out: vec![then_label.to_string(), else_label.to_string()],
                });
                current_entries = vec![];
            },
            Instr::Jump(label) => {
                current_entries.push((idx, entry.clone()));
                blocks.push(BasicBlock {
                    entries: current_entries,
                    label: current_label.clone(),
                    out: vec![label.to_string()],
                });
                current_entries = vec![];
            },
            Instr::Label(name)|Instr::FunctionLabel { name,.. } => {
                if current_entries.len() > 0 {
                    blocks.push(BasicBlock {
                        entries: current_entries,
                        label: current_label.clone(),
                        out: vec![name.to_string()],
                    });
                }
                current_label = name.to_string();
                current_entries = vec![];
                current_entries.push((idx, entry.clone()));
            },
            _ => {
                current_entries.push((idx, entry.clone()));
            },
        }
    }

    if current_entries.len() > 0 {
        blocks.push(BasicBlock {
            entries: current_entries,
            label: current_label,
            out: vec![]
        });
    }

    blocks
}

mod tests {
    use crate::{ir_generator::generate_ir, parse_source, type_checker::typecheck_program};

    use super::*;

    fn i(src: &str) -> HashMap<String, Vec<IREntry>> {
        let a = parse_source(src.to_string());
        let t = typecheck_program(a);
        generate_ir(t)
    }

    #[test]
    fn basic_blocks() {
        let ir = i("
            var x = 1;
            if x > 1 then {
                print_int(x);
            }
            x + 1
        ");

        let bb = find_basic_blocks(ir.get("main").unwrap());

        println!(
            "{}", 
            bb
                .iter()
                .map(|i| format!("Block {:?} --> {:?}\n{}", i.label, i.out, i.entries.iter()
                    .map(|i| i.1.to_string())
                    .collect::<Vec<String>>()
                    .join("\n")
                ))
                .collect::<Vec<String>>()
                .join("\n\n")
        )
    }
}
