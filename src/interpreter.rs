use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::Expression;
use crate::tokenizer::Op;

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Integer(i32),
    Boolean(bool),
    Unit,
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

impl Op {
    fn eval_binary_integer(&self, ival1: i32, right_expr: Expression, sym_table: &Rc<RefCell<SymTable>>) -> Value {
        // Integer ops have no short circuiting so just eval right here
        let ival2: i32 = interpret_sym_table(right_expr, sym_table).try_into().expect("Not Value::Integer");

        match self {
            Op::Add =>       Value::Integer(ival1 + ival2),
            Op::Sub =>       Value::Integer(ival1 - ival2),
            Op::Mul =>       Value::Integer(ival1 * ival2),
            Op::Div =>       Value::Integer(ival1 / ival2),
            Op::Mod =>       Value::Integer(ival1 % ival2),
            Op::Exp =>       Value::Integer(i32::pow(ival1, ival2.try_into().unwrap())),
            Op::Equals =>    Value::Boolean(ival1 == ival2),
            Op::NotEquals => Value::Boolean(ival1 != ival2),
            Op::GT =>        Value::Boolean(ival1 > ival2),
            Op::LT =>        Value::Boolean(ival1 < ival2),
            Op::GTE =>       Value::Boolean(ival1 >= ival2),
            Op::LTE =>       Value::Boolean(ival1 <= ival2),
            _ => panic!("Invalid integer binary operator {:?}", &self)
        }
    }

    fn eval_binary_boolean(&self, bval1: bool, right_expr: Expression, sym_table: &Rc<RefCell<SymTable>>) -> Value {
        match self {
            Op::Equals =>    Value::Boolean(bval1 == interpret_sym_table(right_expr, sym_table).try_into().expect("Not Value::Boolean")),
            Op::NotEquals => Value::Boolean(bval1 != interpret_sym_table(right_expr, sym_table).try_into().expect("Not Value::Boolean")),
            Op::And =>       Value::Boolean(bval1 && interpret_sym_table(right_expr, sym_table).try_into().expect("Not Value::Boolean")),
            Op::Or =>        Value::Boolean(bval1 || interpret_sym_table(right_expr, sym_table).try_into().expect("Not Value::Boolean")),
            _ => panic!("Invalid boolean binary operator {:?}", &self)
        }
    }

    fn eval_unary_integer(&self, ival: i32) -> Value {
        match self {
            Op::Sub => Value::Integer(-ival),
            _ => panic!("invalid integer unary operator {:?}", &self)
        }
    }

    fn eval_unary_boolean(&self, ival: bool) -> Value {
        match self {
            Op::Not => Value::Boolean(!ival),
            _ => panic!("invalid boolean unary operator {:?}", &self)
        }
    }
}

struct SymTable {
    symbols: HashMap<String, Value>,
    parent: Option<Rc<RefCell<SymTable>>>,
}

impl SymTable {
    fn new<'a>(parent: Option<Rc<RefCell<SymTable>>>) -> Rc<RefCell<SymTable>> {
        Rc::new(RefCell::new(SymTable {
            symbols: HashMap::new(),
            parent,
        }))
    }

    fn get(&self, k: &String) -> Value {
        if let Some(val) = self.symbols.get(k) {
            val.clone()
        } else if let Some(parent) = &self.parent {
            parent.borrow().get(k)
        } else {
            panic!("Accessing undefined symbol '{}'", k)
        }
    }

    fn assign(&mut self, k: &String, val: Value) -> Option<(String, Value)> {
        if self.symbols.contains_key(k) {
            self.symbols.insert(k.to_string(), val);
            None
        } else if let Some(parent) = &self.parent {
            parent.borrow_mut().assign(k, val)
        } else {
            panic!("Assigning to undefined symbol '{}'", k);
        }
    }
}

fn interpret_sym_table(node: Expression, sym_table: &Rc<RefCell<SymTable>>) -> Value {
    match node {
        Expression::IntegerLiteral { value } => {
            Value::Integer(value)
        },
        Expression::BooleanLiteral { value } => {
            Value::Boolean(value)
        }
        Expression::BinaryExpression { left, operator, right } => {
            let left_result = interpret_sym_table(*left, sym_table);
            match left_result {
                Value::Integer(ival1) =>  operator.eval_binary_integer(ival1, *right, sym_table),
                Value::Boolean(bval1) => operator.eval_binary_boolean(bval1, *right, sym_table),
                Value::Unit => panic!("Unit used as operand in binary operation"),
            }
        },
        Expression::UnaryExpression { operand, operator } => {
            let operand_result = interpret_sym_table(*operand, sym_table);
            match operand_result {
                Value::Integer(ival) => operator.eval_unary_integer(ival),
                Value::Boolean(bval) => operator.eval_unary_boolean(bval),
                Value::Unit => panic!("Unary operator not permitted for Unit value")
            }
        }
        Expression::IfExpression { condition, then_branch, else_branch } => {
            let condition_result = interpret_sym_table(*condition, sym_table);
            match condition_result {
                Value::Boolean(condition_val) => {
                    if condition_val {
                        interpret_sym_table(*then_branch, sym_table)
                    } else if let Some(else_branch) = else_branch {
                        interpret_sym_table(*else_branch, sym_table)
                    } else {
                        Value::Unit
                    }
                },
                _ => panic!("If expression condition must be a boolean"),
            }
        },
        Expression::BlockExpression { statements, result } => {
            let outer_sym_table = sym_table.clone();
            let inner_sym_table = SymTable::new(Some(outer_sym_table));
            for expr in statements {
                interpret_sym_table(*expr, &inner_sym_table);
            }
            interpret_sym_table(*result, &inner_sym_table)
        },
        Expression::Identifier { value } => sym_table.borrow().get(&value),
        Expression::AssignmentExpression { left, right } => {
            if let Expression::Identifier { value: id } = *left {
                let value = interpret_sym_table(*right, sym_table);
                sym_table.borrow_mut().assign(&id, value);
                // Build up the assignments to outside scope variables, and perform them when scope exited
                value
            } else {
                panic!("Left side of an assignment must be an identifier");
            }
        },
        Expression::VariableDeclaration { id, init } => {
            if let Expression::Identifier { value } = *id {
                let init_value = interpret_sym_table(*init, sym_table);
                sym_table.borrow_mut().symbols.insert(value, init_value);
                Value::Unit
            } else {
                panic!("Id of a variable declaration must be an identifier");
            }
        },
        Expression::Unit => Value::Unit,
        _ => panic!("Unknown expression {:?}", node)
    }
}

pub fn interpret(node: Expression) -> Value {
    interpret_sym_table(node, &SymTable::new(None))
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::{tokenize, Token};
    use crate::parser::parse;
    use super::*;

    fn i(src: &str) -> Value {
        let tokens: Vec<Token> = tokenize(src);
        let expression = parse(tokens);
        interpret(expression)
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
    #[should_panic(expected = "Accessing undefined symbol 'minttuglitch'")]
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
}
