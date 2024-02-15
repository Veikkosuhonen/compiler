use std::fs::File;
use std::os::unix::fs::PermissionsExt;
use std::{fs, io::Write};
use std::process::{Command, Stdio};

use crate::{asm_generator::generate_asm, ir_generator::generate_ir, parse_source, type_checker::typecheck_program};

pub fn run_tests() {
    let _ = fs::create_dir("./target");
    fs::read_dir("./test_programs").unwrap()
        .filter_map(|res| {
            res.ok()
        })
        .for_each(|file| {
            run_test_file(file.path().to_str().unwrap())
        })
}

fn run_test_file(path: &str) {
    println!("Running test suite {}", path);
    let source = fs::read_to_string(path).expect("Should've been able to read the file");
    let tests = source.split("---").collect::<Vec<&str>>();
    for (i, test_source) in tests.iter().enumerate() {
        println!("{}/{}", i + 1, tests.len());
        run_test(test_source)
    }
}

fn run_test(source: &str) {

    let mut inputs: Vec<i32> = vec![];

    let mut expects: Vec<i32> = vec![];

    let program_source = source.split("\n").filter(|line| {
        if line.trim_start().starts_with("input") {
            inputs.push(line.split_whitespace().last().unwrap().parse().expect("Should've been able to parse i32 after 'input'"));
            false
        } else if line.trim_start().starts_with("expect") {
            expects.push(line.split_whitespace().last().unwrap().parse().expect("Should've been able to parse i32 after 'expect'"));
            false
        } else {
            true
        }
    }).collect::<Vec<&str>>().join("\n");

    let node = parse_source(program_source);
    let typed_ast = typecheck_program(node);
    let ir = generate_ir(typed_ast);
    let asm = generate_asm(ir);

    if fs::write("./target/temp_asm.s", asm).is_err() {
        panic!("Failed to write to temp file ./target/temp_asm.s")
    }

    let compile_output  = Command::new("gcc")
        .args(["-g", "-no-pie", "-o", "./target/temp_program", "./target/temp_asm.s"])
        .output()
        .expect("gcc compile should've run");
    
    println!("{}", String::from_utf8(compile_output.stdout).unwrap());

    if !compile_output.status.success() {
        println!("{}", String::from_utf8(compile_output.stderr).unwrap());
        panic!("gcc compile exited with nonzero status")
    }

    // Set executable permission
    if let Ok(file) = File::open("./target/temp_program") {
        if let Ok(meta) = file.metadata() {
            meta.permissions().set_mode(777); // yes we have the tietoturva
        }
    }

    let mut process = Command::new("./target/temp_program")
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

    let output = process.wait_with_output().expect("Should've been able to read process output");

    if !output.status.success() {
        println!("{}", String::from_utf8(output.stderr).unwrap());
        println!("process exited with status {}", output.status.to_string())
    }

    // Check whether output matches expects
    let outputs = String::from_utf8_lossy(&output.stdout).split("\n").filter_map(|v| {
        if let Ok(v) = v.parse::<i32>() {
            Some(v)
        } else {
            None
        }
    }).collect::<Vec<i32>>();

    assert_eq!(outputs.len(), expects.len(), "Number of actual outputs {} != number of expected outputs {}", outputs.len(), expects.len());

    for (i, value) in outputs.iter().enumerate() {
        assert_eq!(*value, expects[i], "Output at index {} is incorrect: {} != {}", i, *value, expects[i]);
    }

    println!("Pass");
}