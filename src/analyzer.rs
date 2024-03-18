use std::collections::{HashMap, VecDeque};

use crate::{ir_generator::{IREntry, IRVar, Instr}, lang_type::Type, parser::Span};

pub struct Warning {
    pub span: Span,
    pub message: String,
}

pub fn report_useless_writes(_ir: &HashMap<String, Vec<IREntry>>) -> Vec<Warning> {
    let mut warnings = vec![];

    let predefined = get_predefined_vars(&_ir);

    eprintln!(">>> Reaching definitions analysis on IR <<<");
    eprintln!("How to read: <variable> <- [idx's where it was last written to]\n");

    for (f, ir) in _ir.iter() {
        let (ins, _) = get_forward_dataflow(ir, predefined.clone(), rd_transfer, merge);
        eprintln!("\n*** {f} ***");
        eprintln!("{}", ir.iter().enumerate().map(|(idx, i)| 
            format!("{idx} {}     {}", 
                i.to_string(), 
                i.get_reads().iter().map(|r| 
                    format!("{} <- {:?}", 
                        r.to_short_string(), 
                        ins[idx].get(&r.to_short_string())
                        .unwrap_or_else(|| ins[idx].get(&r.parent.clone().unwrap().to_short_string()).unwrap())
                    )
                ).collect::<Vec<String>>().join(", ")
            )
        ).collect::<Vec<String>>().join("\n"));

        // Iterate all writes, and check whether they are read-
        for (idx, entry) in ir.iter().enumerate() {
            if let Some(write) = entry.get_write() {
                // If the type of the write is Unit or Unknown, do not report it in any case. Unknown type is used for main return, and reporting it is buggy and incorrect anyways.
                if write.var_type == Type::Unit || write.var_type == Type::Unknown {
                    continue;
                }

                let mut is_read = false;
                // Where is it read?
                for (jdx, other_entry) in ir.iter().enumerate() {
                    for read in other_entry.get_reads() {
                        let written_to_at = ins[jdx].get(&read.to_short_string())
                            .unwrap_or_else(|| ins[idx].get(&read.parent.clone().unwrap().to_short_string()).unwrap());

                        if written_to_at.contains(&(idx as i32)) {
                            is_read = true;
                        }
                    }
                }

                if !is_read {
                    eprintln!("{:?}", entry);
                    warnings.push(Warning { 
                        span: entry.span.clone(), 
                        message: format!("This write is never read."), 
                    })
                }
            }
        }
    }

    warnings
}

pub fn print_reaching_definitions(_ir: HashMap<String, Vec<IREntry>>) {
    let predefined = get_predefined_vars(&_ir);

    println!(">>> Reaching definitions analysis on IR <<<");
    println!("How to read: <variable> <- [idx's where it was last written to]\n");

    for (f, ir) in _ir.iter() {
        let (ins, _) = get_forward_dataflow(ir, predefined.clone(), rd_transfer, merge);
        println!("\n*** {f} ***");
        println!("{}", ir.iter().enumerate().map(|(idx, i)| 
            format!("{idx} {}     {}", 
                i.to_string(), 
                i.get_reads().iter().map(|r| 
                    format!("{} <- {:?}", 
                        r.to_short_string(), 
                        ins[idx].get(&r.to_short_string())
                        .unwrap_or_else(|| ins[idx].get(&r.parent.clone().unwrap().to_short_string()).unwrap())
                    )
                ).collect::<Vec<String>>().join(", ")
            )
        ).collect::<Vec<String>>().join("\n"));
    }
}

pub fn print_live_vars(_ir: HashMap<String, Vec<IREntry>>) {
    let predefined = get_predefined_vars(&_ir);

    println!(">>> Live variable analysis on IR <<<");
    println!("How to read: <variable> <- [idx's where it is next read]\n");

    for (f, ir) in _ir.iter() {
        let (ins, _) = get_backward_dataflow(ir, predefined.clone(), lv_transfer, merge);
        println!("\n*** {f} ***");
        println!("{}", ir.iter().enumerate().map(|(idx, i)| 
            format!("{idx} {}     {}", 
                i.to_string(), 
                // "Which variables may still be read after this instruction?"
                ins[idx].iter()
                .filter(|(_, lines)| lines.iter().any(|l| *l > idx as i32))
                .map(|(var, lines)| 
                    format!("{} <- {:?}", 
                        var, 
                        lines,
                    )
                ).collect::<Vec<String>>().join(", ")
            )
        ).collect::<Vec<String>>().join("\n"));
    }
}

impl IREntry {
    fn get_write(&self) -> Option<IRVar> {
        match &self.instruction {
            Instr::Call { dest,.. }|Instr::Copy { dest,.. }|Instr::LoadBoolConst { dest,.. }|Instr::LoadIntConst { dest,.. } => Some(*dest.clone()),
            _ => None,
        }
    }

    fn get_reads(&self) -> Vec<IRVar> {
        match &self.instruction {
            Instr::Call { fun, args, .. } => {
                let mut reads = args.iter().map(|a| *a.clone()).collect::<Vec<IRVar>>();
                reads.push(*fun.clone());
                reads
            },
            Instr::CondJump { cond, .. } => vec![*cond.clone()],
            Instr::Copy { source, .. } => vec![*source.clone()],
            _ => vec![],
        }
    }

    fn get_jumps(&self) -> Vec<String> {
        match &self.instruction {
            Instr::CondJump { then_label, else_label,.. } => vec![then_label.clone(), else_label.clone()],
            Instr::Jump(lbl) => vec![lbl.clone()],
            _ => vec![],
        }
    }
}

#[derive(Clone)]
struct BasicBlock {
    // entries: Vec<(usize, IREntry)>,
    label: String,
    out: Vec<String>
}

// 1.
type Set = Vec<i32>;
type State = HashMap<String, Set>;

fn get_forward_dataflow(ir: &Vec<IREntry>, predefined: Vec<String>, 
    transfer: fn(&State, usize, &Vec<IREntry>) -> State,
    merge: fn(&Vec<State>) -> State,
) -> (Vec<State>, Vec<State>) {
    // 2.
    let mut ins: Vec<State> = vec![];
    let mut outs: Vec<State> = vec![];
    // 6.
    let mut predecessors: Vec<Vec<usize>> = vec![];
    let mut successors: Vec<Vec<usize>> = vec![];
    // 7.
    let mut work_queue = VecDeque::new();

    for (idx, entry) in ir.iter().enumerate() {
        // 4.
        ins.push(State::new());
        outs.push(State::new());
        // 7.
        if idx > 0 {
            work_queue.push_back(idx);
        }

        // 6.
        let mut current_predecessors = vec![];

        if let Instr::Label(label) = &entry.instruction {
            ir.iter().enumerate()
            .flat_map(|(jdx, e)| e.get_jumps().iter().map(|jmp| (jdx, jmp.clone())).collect::<Vec<(usize, String)>>())
            .filter(|(_, lbl)| lbl == label)
            .for_each(|(jdx, _)| current_predecessors.push(jdx))
        }
        if idx > 0 {
            current_predecessors.push(idx - 1)
        }
        
        predecessors.push(current_predecessors);

        let mut current_successors = vec![];
        let jumps = entry.get_jumps();
        if jumps.is_empty() && idx < ir.len() - 1 {
            current_successors.push(idx + 1);
        } else {
            for jmp in jumps {
                for (jdx, other) in ir.iter().enumerate() {
                    if let Instr::Label(lbl) = &other.instruction {
                        if *lbl == jmp {
                            current_successors.push(jdx);
                        }
                    }
                }
            }
        }
        successors.push(current_successors);
    }

    let mut zero_out_state = State::new();

    // 3.
    for p in predefined {
        zero_out_state.insert(p, vec![-2]);
    }

    for entry in ir {
        if let Instr::FunctionLabel { params,.. } = &entry.instruction {
            for param in params {
                zero_out_state.insert(param.name.clone(), vec![-2]);
            }
        }

        for var in entry.variables() {
            if !zero_out_state.contains_key(&var.name) {
                zero_out_state.insert(var.name.clone(), vec![-1]);
            }
        }
    };
    
    outs[0] = zero_out_state;

    // 7.
    while let Some(idx) = work_queue.pop_front() {
        let current_predecessors = predecessors[idx].iter().map(|j| outs[*j].clone()).collect::<Vec<State>>();
        ins[idx] = merge(&current_predecessors);
        let prev_out = outs[idx].clone();
        outs[idx] = transfer(&ins[idx], idx, &ir);
        // Changed?
        if prev_out != outs[idx] {
            for jdx in &successors[idx] {
                work_queue.push_back(*jdx);
            }
        }
    }

    (ins, outs)
}


fn get_backward_dataflow(ir: &Vec<IREntry>, predefined: Vec<String>, 
    transfer: fn(&State, usize, &Vec<IREntry>) -> State,
    merge: fn(&Vec<State>) -> State,
) -> (Vec<State>, Vec<State>) {
    // 2.
    let mut ins: Vec<State> = vec![];
    let mut outs: Vec<State> = vec![];
    // 6.
    let mut predecessors: Vec<Vec<usize>> = vec![];
    let mut successors: Vec<Vec<usize>> = vec![];
    // 7.
    let mut work_queue = VecDeque::new();

    for (idx, entry) in ir.iter().enumerate() {
        // 4.
        ins.push(State::new());
        outs.push(State::new());
        // 7.
        if idx > 0 {
            work_queue.push_back(idx);
        }

        // 6.
        let mut current_predecessors = vec![];

        if let Instr::Label(label) = &entry.instruction {
            ir.iter().enumerate()
            .flat_map(|(jdx, e)| e.get_jumps().iter().map(|jmp| (jdx, jmp.clone())).collect::<Vec<(usize, String)>>())
            .filter(|(_, lbl)| lbl == label)
            .for_each(|(jdx, _)| current_predecessors.push(jdx))
        }
        if idx > 0 {
            current_predecessors.push(idx - 1)
        }
        
        predecessors.push(current_predecessors);

        let mut current_successors = vec![];
        let jumps = entry.get_jumps();
        if jumps.is_empty() && idx < ir.len() - 1 {
            current_successors.push(idx + 1);
        } else {
            for jmp in jumps {
                for (jdx, other) in ir.iter().enumerate() {
                    if let Instr::Label(lbl) = &other.instruction {
                        if *lbl == jmp {
                            current_successors.push(jdx);
                        }
                    }
                }
            }
        }
        successors.push(current_successors);
    }

    let mut zero_out_state = State::new();

    // 3.
    for p in predefined {
        zero_out_state.insert(p, vec![-2]);
    }

    for entry in ir {
        if let Instr::FunctionLabel { params,.. } = &entry.instruction {
            for param in params {
                zero_out_state.insert(param.name.clone(), vec![-2]);
            }
        }

        for var in entry.variables() {
            if !zero_out_state.contains_key(&var.name) {
                zero_out_state.insert(var.name.clone(), vec![-1]);
            }
        }
    };
    
    outs[0] = zero_out_state;

    // 7.
    while let Some(idx) = work_queue.pop_back() {
        let current_successors = successors[idx].iter().map(|j| ins[*j].clone()).collect::<Vec<State>>();
        outs[idx] = merge(&current_successors);
        let prev_in = ins[idx].clone();
        ins[idx] = transfer(&outs[idx], idx, &ir);
        // Changed?
        if prev_in != ins[idx] {
            for jdx in &predecessors[idx] {
                work_queue.push_front(*jdx);
            }
        }
    }

    (ins, outs)
}


// 5.
fn rd_transfer(input: &State, i: usize, ir: &Vec<IREntry>) -> State {
    let mut output = input.clone();
    if let Some(var) = ir[i].get_write() {
        output.insert(var.to_short_string(), vec![i as i32]);
    }
    output
}

// 7.
fn merge(states: &Vec<State>) -> State {
    let mut res = State::new();
    for s in states {
        for (k, v) in s.iter() {
            // Merge into res
            let mut v = v.clone();
            if let Some(s) = res.get(k) {
                v.extend(s);
            }
            v.sort();
            v.dedup();
            res.insert(k.clone(), v);
        }
    }
    res
}

fn lv_transfer(input: &State, i: usize, ir: &Vec<IREntry>) -> State {
    let mut output = input.clone();
    for var in ir[i].get_reads() {
        output.insert(var.to_short_string(), vec![i as i32]);
    }
    output
}

pub fn ir_to_flowgraph(ir: HashMap<String, Vec<IREntry>>) -> String {
    format!(
        "digraph {}\n{}\n{}",
        "{",
        ir.iter().map(|(_, ir)| {
            let blocks = find_basic_blocks(ir);
            create_dot_flowgraph(blocks)
        }).collect::<Vec<String>>().join("\n"),
        "}"
    )
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
                    // entries: current_entries,
                    label: current_label.clone(),
                    out: vec![then_label.to_string(), else_label.to_string()],
                });
                current_entries = vec![];
            },
            Instr::Jump(label) => {
                current_entries.push((idx, entry.clone()));
                blocks.push(BasicBlock {
                    // entries: current_entries,
                    label: current_label.clone(),
                    out: vec![label.to_string()],
                });
                current_entries = vec![];
            },
            Instr::Label(name)|Instr::FunctionLabel { name,.. } => {
                if current_entries.len() > 0 {
                    blocks.push(BasicBlock {
                        // entries: current_entries,
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
            // entries: current_entries,
            label: current_label,
            out: vec![]
        });
    }

    blocks
}



fn create_dot_flowgraph(blocks: Vec<BasicBlock>) -> String {
    blocks.iter()
        .map(|b| 
            b.out.iter()
            .map(|o| format!("\"{}\" -> \"{}\";", b.label, o))
            .collect::<Vec<String>>()
            .join("\n")
        )
        .collect::<Vec<String>>().join("\n")
}

fn get_predefined_vars(ir: &HashMap<String, Vec<IREntry>>) -> Vec<String> {
    let mut predefined = vec![];
    for f in ir.keys() {
        predefined.push(f.clone());
    }
    predefined
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
                .map(|i| format!("Block {:?} --> {:?}", i.label, i.out))
                .collect::<Vec<String>>()
                .join("\n\n")
        )
    }
}
