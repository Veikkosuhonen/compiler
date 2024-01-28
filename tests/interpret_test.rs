use compiler::interpret_file;
use compiler::interpreter::Value;

#[test]
fn basic_test() {
    let res = interpret_file(&"./programs/basic_test.chi".to_string());
    if let Value::Integer(ival) = res {
        assert_eq!(ival, 42);
    } else {
        panic!("Did not return integer");
    }
}

