use std::fs::File;
use std::os::unix::fs::PermissionsExt;
use std::thread::JoinHandle;
use std::time::Instant;
use std::{fs, io::Write};
use std::process::{Child, Command, Stdio};

use crate::{asm_generator::generate_asm, ir_generator::generate_ir, parse_source, type_checker::typecheck_program};

pub fn run_tests() {
    let _ = fs::create_dir("./target");
    let mut handles = fs::read_dir("./test_programs/e2e").unwrap()
        .filter_map(|res| {
            res.ok()
        })
        .map(|file| {
            run_test_file(file.path().to_str().unwrap().to_string())
        })
        .collect::<Vec<JoinHandle<_>>>();

    handles.reverse();

    for handle in handles {
        println!("{}", handle.join().unwrap().join(""));
    }
}

fn run_test_file(path: String) -> JoinHandle<Vec<String>> {
    std::thread::spawn(move || {
        let mut lines = vec![];

        let tmp = path.clone();
        let test_id = tmp.split("/").last().unwrap();

        lines.push(format!("\n*** Running test suite {} ***\n", test_id));
        let source = fs::read_to_string(path).expect("Should've been able to read the file");
        let tests = source.split("---").collect::<Vec<&str>>();
        for (i, test_source) in tests.iter().enumerate() {
            lines.push(format!("\n{}/{} ", i + 1, tests.len()));
            lines.append(&mut run_test(test_source, format!("{test_id}_{i}")));
        }
        lines
    })
}

fn run_test(source: &str, id: String) -> Vec<String> {

    let mut outputs: Vec<String> = vec![];

    let mut out = |msg: String| {
        outputs.push(msg);
    };

    let mut inputs: Vec<i32> = vec![];

    let mut expects: Vec<i32> = vec![];

    let mut name: Option<String> = None;

    let program_source = source.split("\n").filter(|line| {
        if line.trim_start().starts_with("input") {
            inputs.push(line.split_whitespace().last().unwrap().parse().expect("Should've been able to parse i32 after 'input'"));
            false
        } else if line.trim_start().starts_with("expect") {
            expects.push(line.split_whitespace().last().unwrap().parse().expect("Should've been able to parse i32 after 'expect'"));
            false
        } else if line.trim_start().starts_with("name") {
            name = Some(line.split_whitespace()
                .collect::<Vec<&str>>()
                .get(1..)
                .expect("Test name to follow 'name'")
                .join(" "));
            false
        } else {
            true
        }
    }).collect::<Vec<&str>>().join("\n");

    if let Some(name) = name {
        out(format!("- {name} "));
    }

    let compilation_start = Instant::now();

    let node = parse_source(program_source.clone());
    let typed_ast = typecheck_program(node);
    let ir = generate_ir(typed_ast);
    let asm = generate_asm(ir);

    out(format!(" - compile in {} ms\n", compilation_start.elapsed().as_millis()));

    if fs::write(format!("./target/{id}.hycs"), program_source).is_err() {
        panic!("Failed to write to temp file ./target/{id}.hycs")
    }

    let interpreter_start = Instant::now();

    // Interpret
    let mut interpret_process = Command::new("./target/debug/compiler")
        .args(["i", format!("./target/{id}.hycs").as_str()])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("Should've been able to interpret program");

    let mut stdin = interpret_process.stdin.take().expect("Should've been able to open stdin to interpreter process");

    let interpreter_input = inputs.clone();
    std::thread::spawn(move || {
        for input in interpreter_input {
            stdin.write(input.to_string().as_bytes()).expect("Should've been able to write to stdin of interpreter process");
        }
    });

    out(format!("-> Interpreted "));
    if let Err(msg) = check_output(interpret_process, expects.clone()) {
        out(format!("---> FAIL - {} ms\n", interpreter_start.elapsed().as_millis()));
        out(msg);
    } else {
        out(format!("---> Pass - {} ms\n", interpreter_start.elapsed().as_millis()));
    }

    // Asm
    if fs::write(format!("./target/{id}.s"), asm).is_err() {
        panic!("Failed to write to temp file ./target/{id}.s")
    }

    let compile_output  = Command::new("gcc")
        .args(["-g", "-no-pie", "-o", format!("./target/{id}").as_str(), format!("./target/{id}.s").as_str()])
        .output()
        .expect("gcc compile should've run");
    
    // println!("{}", String::from_utf8(compile_output.stdout).unwrap());

    if !compile_output.status.success() {
        out(format!("{}\n", String::from_utf8(compile_output.stderr).unwrap()));
        panic!("gcc compile exited with nonzero status")
    }

    // Set executable permission
    if let Ok(file) = File::open(format!("./target/{id}")) {
        if let Ok(meta) = file.metadata() {
            meta.permissions().set_mode(777); // yes we have the tietoturva
        }
    }

    let run_start = Instant::now();

    let mut process = Command::new(format!("./target/{id}"))
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("Should've been able to run compiled program");

    let mut stdin = process.stdin.take().expect("Should've been able to open stdio to child process");
    std::thread::spawn(move || {
        for input in inputs {
            stdin.write(input.to_string().as_bytes()).expect("Should've been able to write to stdin of child process");
        }
    });

    out(format!("-> Compiled    "));
    if let Err(msg) = check_output(process, expects) {
        out(format!("---> FAIL - {} ms\n", interpreter_start.elapsed().as_millis()));
        out(msg);
    } else {
        out(format!("---> Pass - {} ms\n", run_start.elapsed().as_millis()));
    }

    outputs
}

fn check_output(process: Child, expects: Vec<i32>) -> Result<(), String> {
    let output = process.wait_with_output().expect("Should've been able to read process output");

    if !output.status.success() {
        return Err(format!("{}\nprocess exited with status {}", String::from_utf8(output.stderr).unwrap(), output.status.to_string()));
    }

    let outputs = String::from_utf8_lossy(&output.stdout).split("\n").filter_map(|v| {
        // println!("{}", v);
        if let Ok(v) = v.parse::<i32>() {
            Some(v)
        } else {
            None
        }
    }).collect::<Vec<i32>>();

    if outputs.len() != expects.len() {
        return Err(format!("Number of actual outputs {} != number of expected outputs {}\n", outputs.len(), expects.len()));
    }

    for (i, value) in outputs.iter().enumerate() {
        if *value != expects[i] {
            return Err(format!("Output at index {} is incorrect: {} != {}\n", i, *value, expects[i]))
        }
    }

    Ok(())
}