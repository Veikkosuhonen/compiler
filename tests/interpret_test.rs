use compiler::interpret_file;
use compiler::interpreter::Value;

#[test]
fn basic_test() {
    let res = interpret_file(&"./test_programs/basic_test.hycs".to_string());
    if let Value::Integer(ival) = res {
        assert_eq!(ival, 42);
    } else {
        panic!("Did not return integer");
    }
}

#[test]
fn basic_symbols_test() {
    let res = interpret_file(&"./test_programs/basic_symbols_test.hycs".to_string());
    if let Value::Integer(ival) = res {
        assert_eq!(ival, 42);
    } else {
        panic!("Did not return integer");
    }
}

#[test]
fn scopes_test() {
    let res = interpret_file(&"./test_programs/scopes_test.hycs".to_string());
    if let Value::Integer(ival) = res {
        assert_eq!(ival, 199);
    } else {
        panic!("Did not return integer");
    }
}
