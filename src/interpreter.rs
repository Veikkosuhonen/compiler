use std::collections::HashMap;
use std::mem;
use std::default::Default;
use std::rc::Rc;

use crate::parser::Expression;
use crate::tokenizer::Op;

use self::builtin_functions::BuiltIn;

mod builtin_functions;

#[derive(Debug)]
pub struct UserDefinedFunction {
    body: Box<Expression>, // This should only be a BlockExpression
    params: Vec<Box<Expression>>, // This should only be a vec of Identifiers
}

#[derive(Debug, Clone)]
pub enum Function {
    BuiltIn(BuiltIn),
    UserDefined(Rc<UserDefinedFunction>),
}

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i32),
    Boolean(bool),
    Function(Function),
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
    fn eval_binary_integer(&self, ival1: i32, right_expr: Expression, sym_table: &mut Box<SymTable>) -> Value {
        // Integer ops have no short circuiting so just eval right here
        let ival2: i32 = interpret(right_expr, sym_table).try_into().expect("Not Value::Integer");

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

    fn eval_binary_boolean(&self, bval1: bool, right_expr: Expression, sym_table: &mut Box<SymTable>) -> Value {
        match self {
            Op::Equals =>    Value::Boolean(bval1 == interpret(right_expr, sym_table).try_into().expect("Not Value::Boolean")),
            Op::NotEquals => Value::Boolean(bval1 != interpret(right_expr, sym_table).try_into().expect("Not Value::Boolean")),
            Op::And =>       Value::Boolean(bval1 && interpret(right_expr, sym_table).try_into().expect("Not Value::Boolean")),
            Op::Or =>        Value::Boolean(bval1 || interpret(right_expr, sym_table).try_into().expect("Not Value::Boolean")),
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

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum Symbol {
    Identifier(String),
    Operator(Op)
}

#[derive(Default)]
struct SymTable {
    symbols: HashMap<Symbol, Value>,
    parent: Option<Box<SymTable>>,
}

impl SymTable {
    fn new<'a>(parent: Option<Box<SymTable>>) -> Box<SymTable> {
        Box::new(SymTable {
            symbols: HashMap::new(),
            parent,
        })
    }

    fn get(&self, k: &Symbol) -> Value {
        if let Some(val) = self.symbols.get(k) {
            val.clone()
        } else if let Some(parent) = &self.parent {
            parent.get(k)
        } else {
            panic!("Accessing undefined symbol {:?}", k)
        }
    }

    fn assign(&mut self, k: Symbol, val: Value) -> Value {
        if self.symbols.contains_key(&k) {
            self.symbols.insert(k, val.clone());
            val
        } else if let Some(parent) = &mut self.parent {
            parent.assign(k, val)
        } else {
            panic!("Assigning to undefined symbol '{:?}'", k);
        }
    }

    fn with_inner<T>(self: &mut Box<SymTable>, f: impl FnOnce(&mut Box<SymTable>) -> T) -> T {
        let symtab = mem::replace(self, Default::default());
        let mut inner_symtab = SymTable::new(Some(symtab));
        let result = f(&mut inner_symtab);
        *self = inner_symtab.parent.unwrap();
        result
    }
}

fn interpret(node: Expression, sym_table: &mut Box<SymTable>) -> Value {
    match node {
        Expression::IntegerLiteral { value } => {
            Value::Integer(value)
        },
        Expression::BooleanLiteral { value } => {
            Value::Boolean(value)
        }
        Expression::BinaryExpression { left, operator, right } => {
            let left_result = interpret(*left, sym_table);
            match left_result {
                Value::Integer(ival1) =>  operator.eval_binary_integer(ival1, *right, sym_table),
                Value::Boolean(bval1) => operator.eval_binary_boolean(bval1, *right, sym_table),
                Value::Function(_) => panic!("Function used as operand in binary operation"),
                Value::Unit => panic!("Unit used as operand in binary operation"),
            }
        },
        Expression::UnaryExpression { operand, operator } => {
            let operand_result = interpret(*operand, sym_table);
            match operand_result {
                Value::Integer(ival) => operator.eval_unary_integer(ival),
                Value::Boolean(bval) => operator.eval_unary_boolean(bval),
                Value::Function(_) => panic!("Unary operator not permitted for Function valuen"),
                Value::Unit => panic!("Unary operator not permitted for Unit value")
            }
        }
        Expression::IfExpression { condition, then_branch, else_branch } => {
            let condition_result = interpret(*condition, sym_table);
            match condition_result {
                Value::Boolean(condition_val) => {
                    if condition_val {
                        interpret(*then_branch, sym_table)
                    } else if let Some(else_branch) = else_branch {
                        interpret(*else_branch, sym_table)
                    } else {
                        Value::Unit
                    }
                },
                _ => panic!("If expression condition must be a boolean"),
            }
        },
        Expression::BlockExpression { statements, result } => {
            sym_table.with_inner(|inner_sym_table| {
                for expr in statements {
                    interpret(*expr, inner_sym_table);
                }
                interpret(*result, inner_sym_table)
            })
        },
        Expression::Identifier { value } => sym_table.get(&Symbol::Identifier(value)),
        Expression::AssignmentExpression { left, right } => {
            if let Expression::Identifier { value: id } = *left {
                let value = interpret(*right, sym_table);
                sym_table.assign(Symbol::Identifier(id), value)
            } else {
                panic!("Left side of an assignment must be an identifier");
            }
        },
        Expression::VariableDeclaration { id, init } => {
            if let Expression::Identifier { value } = *id {
                let init_value = interpret(*init, sym_table);
                sym_table.symbols.insert(Symbol::Identifier(value), init_value);
                Value::Unit
            } else {
                panic!("Id of a variable declaration must be an identifier");
            }
        },
        Expression::Unit => Value::Unit,
        _ => panic!("Unknown expression {:?}", node)
    }
}

fn get_toplevel_sym_table() -> Box<SymTable> {
    let mut sym_table = SymTable::new(None);
    let builtins = builtin_functions::get_builtin_function_symbol_mappings();
    for (symbol, val) in builtins {
        sym_table.symbols.insert(symbol, val);
    }
    sym_table
}

pub fn interpret_program(node: Expression) -> Value {
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
        interpret_program(expression)
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
}
