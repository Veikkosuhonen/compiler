use std::rc::Rc;

use crate::sym_table::{SymTable, Symbol};
use crate::parser::{ASTNode, Expression};
use crate::tokenizer::Op;
use crate::builtin_functions::*;


#[derive(Debug)]
#[allow(dead_code)]
pub struct UserDefinedFunction {
    body: Box<Expression<ASTNode>>, // This should only be a BlockExpression
    params: Vec<Box<Expression<ASTNode>>>, // This should only be a vec of Identifiers
}

#[derive(Debug, Clone)]
pub enum Function {
    BuiltIn(BuiltIn),
    UserDefined(Rc<UserDefinedFunction>),
}

#[derive(Debug, Clone, Default)]
pub enum Value {
    Integer(i32),
    Boolean(bool),
    Function(Function),
    #[default] Unit,
}

impl From<Value> for bool {
    fn from(val: Value) -> Self {
        if let Value::Boolean(bval) = val {
            bval
        } else {
            panic!("Tried to convert non-boolean value to bool")
        }
    }
}

impl From<Value> for i32 {
    fn from(val: Value) -> Self {
        if let Value::Integer(ival) = val {
            ival
        } else {
            panic!("Tried to convert non-integer value to i32")
        }
    }
}

fn eval_binary_op(
    left_node: &Box<ASTNode>, 
    right_node: &Box<ASTNode>, 
    operator: &Op, 
    sym_table: &mut Box<SymTable<Value>>
) -> Value {
    let left_val = interpret(&left_node, sym_table);

    if let Value::Function(op_function) = sym_table.get(&mut Symbol::Operator(*operator)) {
        // Lazy eval to enable short circuiting
        let eval_right = || { interpret(&right_node, sym_table) };
        match op_function {
            Function::BuiltIn(builtin) => eval_builtin_binary(builtin, left_val, eval_right),
            Function::UserDefined(_) => panic!("Not yet implemented")
        }
    } else {
        panic!("Undefined operator {:?}", operator)
    }
}

fn eval_unary_op(
    operand: &Box<ASTNode>,
    operator: &Op,
    sym_table: &mut Box<SymTable<Value>>
) -> Value {
    let operand = interpret(&operand, sym_table);
    if let Value::Function(op_function) = sym_table.get(&mut Symbol::Operator(*operator)) {
        match op_function {
            Function::BuiltIn(builtin) => eval_builtin_unary(builtin, operand),
            Function::UserDefined(_) => panic!("Not yet implemented")
        }
    } else {
        panic!("Undefined operator {:?}", operator)
    }
}

fn eval_call_expression(
    callee: &Box<ASTNode>,
    argument_expr: &Vec<Box<ASTNode>>,
    sym_table: &mut Box<SymTable<Value>>
) -> Value {
    if let Expression::Identifier { value: function_id } = &callee.expr {
        let called_function = sym_table.get(&Symbol::Identifier(function_id.to_string()));
        if let Value::Function(called_function) = called_function {
            match called_function {
                Function::BuiltIn(builtin) => {
                    let mut args: Vec<Value> = vec![];
                    for expr in argument_expr {
                        args.push(interpret(&expr, sym_table));
                    }
                    eval_builtin_function(builtin, args)
                },
                Function::UserDefined(_) => panic!("Not yet implemented"),
            }
        } else {
            panic!("Calling undefined function {:?}", function_id);
        }
    } else {
        panic!("Callee of a call expression must be an identifier");
    }
}

fn interpret(node: &ASTNode, sym_table: &mut Box<SymTable<Value>>) -> Value {
    match &node.expr {
        Expression::IntegerLiteral { value } => {
            Value::Integer(*value)
        },
        Expression::BooleanLiteral { value } => {
            Value::Boolean(*value)
        }
        Expression::BinaryExpression { left, operator, right } => {
            eval_binary_op(left, right, operator, sym_table)
        },
        Expression::UnaryExpression { operand, operator } => {
            eval_unary_op(operand, operator, sym_table)
        }
        Expression::IfExpression { condition, then_branch, else_branch } => {
            let condition_result = interpret(&condition, sym_table);
            match condition_result {
                Value::Boolean(condition_val) => {
                    if condition_val {
                        interpret(&then_branch, sym_table)
                    } else if let Some(else_branch) = else_branch {
                        interpret(&else_branch, sym_table)
                    } else {
                        Value::Unit
                    }
                },
                _ => panic!("If expression condition must be a boolean"),
            }
        },
        Expression::WhileExpression { condition, body } => {
            let mut value = Value::Unit;
            while interpret(&condition, sym_table).try_into().expect("While expression condition must return a boolean") {
                value = interpret(&body, sym_table);
            }
            value
        },
        Expression::BlockExpression { statements, result } => {
            sym_table.with_inner(|inner_sym_table| {
                for expr in statements {
                    interpret(&expr, inner_sym_table);
                }
                interpret(&result, inner_sym_table)
            })
        },
        Expression::Identifier { value } => sym_table.get(&Symbol::Identifier(value.clone())),
        Expression::AssignmentExpression { left, right } => {
            if let Expression::Identifier { value: id } = &left.expr {
                let value = interpret(&right, sym_table);
                sym_table.assign(Symbol::Identifier(id.clone()), value)
            } else {
                panic!("Left side of an assignment must be an identifier");
            }
        },
        Expression::VariableDeclaration { id, init } => {
            if let Expression::Identifier { value } = &id.expr {
                let init_value = interpret(&init, sym_table);
                sym_table.symbols.insert(Symbol::Identifier(value.clone()), init_value);
                Value::Unit
            } else {
                panic!("Id of a variable declaration must be an identifier");
            }
        },
        Expression::CallExpression { callee, arguments } => {
            eval_call_expression(callee, arguments, sym_table)
        },
        Expression::Unit => Value::Unit,
    }
}

fn get_toplevel_sym_table() -> Box<SymTable<Value>> {
    let mut sym_table = SymTable::new(None);
    let builtins = get_builtin_function_symbol_value_mappings();
    for (symbol, val) in builtins {
        sym_table.symbols.insert(symbol, val);
    }
    sym_table
}

pub fn interpret_program(node: &ASTNode) -> Value {
    interpret(node, &mut get_toplevel_sym_table())
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::{tokenize, Token};
    use crate::parser::parse;
    use super::*;

    fn i(src: &str) -> Value {
        let tokens: Vec<Token> = tokenize(src);
        let expression = parse(tokens);
        interpret_program(&expression)
    }

    #[test]
    fn test_interpret_integers() {
        let res = i("7 + 3 * 2");
        assert_eq!(13, res.try_into().expect("Not an integer!"));
    }

    #[test]
    fn test_interpret_booleans() {
        let res = i("true and false or true");
        assert_eq!(true, res.try_into().expect("Not a bool!"));
    }

    #[test]
    fn test_unary_ops() {
        let mut res = i("not true");
        assert_eq!(false, res.try_into().expect("Not a bool!"));
        res = i("not not not false");
        assert_eq!(true, res.try_into().expect("Not a bool!"));
        res = i("-42");
        assert_eq!(-42, res.try_into().expect("Not an integer!"));
    }

    #[test]
    fn test_complex_unary() {
        let res = i("2 * -2 + -2");
        assert_eq!(-6, res.try_into().expect("Not an integer!"));
    }

    #[test]
    fn test_interpret_if() {
        let res = i("
        if (true and false and true) 
            then 41");
        assert!(matches!(res, Value::Unit))
    }

    #[test]
    fn test_interpret_if_else() {
        let res = i("
        if (true and false or true) 
            then 42 
            else 0 - 999");
        assert_eq!(42, res.try_into().expect("Not an integer!"));
    }

    #[test]
    fn interpret_while() {
        let res = i("
        {
            var x = 0;
            while (x < 10) do {
                x = x + 1;
            };
            x
        }");
        assert_eq!(10, res.try_into().expect("Not an integer!"));
    }

    #[test]
    fn block_expression() {
        let res = i("
        {
            1 + 1;
            1 + 2;
            11 * 2 + 10 * 2
        }
        ");
        assert_eq!(42, res.try_into().expect("Not an integer!"));
    }

    #[test]
    fn variable_declaration() {
        let res = i("
            {
                var minttu = true;
            }
        ");
        assert!(matches!(res, Value::Unit));
    }

    #[test]
    fn symbol_reference() {
        let res = i("
            {
                var minttu = 50000;
                minttu
            }
        ");
        assert_eq!(50000, res.try_into().expect("Not an integer!"));
    }

    #[test]
    fn outer_symbol_reference() {
        let res = i("
            {
                var minttu = 50000;
                {
                    minttu + 1
                }
            }
        ");
        assert_eq!(50001, res.try_into().expect("Not an integer!"));
    }

    #[test]
    #[should_panic(expected = "Accessing undefined symbol Identifier(\"minttuglitch\")")]
    fn undefined_reference() {
        i("
            {
                var minttu = 50000;
                minttuglitch
            }
        ");
    }

    #[test]
    fn shadowing() {
        let res = i("
            {
                var minttu = 50000;
                {
                    var minttu = 90000;
                    minttu
                }
            }
        ");
        assert_eq!(90000, res.try_into().expect("Not an integer!"));
    }

    #[test]
    fn assign_to_outside_scope() {
        let res = i("
            {
                var minttu = 50000;
                {
                    minttu = 90000;
                };
                minttu
            }
        ");
        assert_eq!(90000, res.try_into().expect("Not an integer!"));
    }

    #[test]
    fn call_builtin_function() {
        let res = i("
            {
                print_bool(false);
            }
        ");
        assert!(matches!(res, Value::Unit));
        let res2 = i("
            {
                var x = 42;
                print_int(x);

                x
            }
        ");
        assert_eq!(42, res2.try_into().expect("Not an integer!"));
    }
}
