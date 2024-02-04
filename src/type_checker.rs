use crate::builtin_functions::get_builtin_function_symbol_type_mappings;
use crate::sym_table::{SymTable, Symbol};
use crate::parser::Expression;
use crate::tokenizer::Op;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub param_types: Vec<Type>,
    pub return_type: Type,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub enum Type {
    Integer,
    Boolean,
    Function(Box<FunctionType>),
    #[default] Unit,
}

fn typecheck_binary_op(
    left_expr: Box<Expression>, 
    right_expr: Box<Expression>, 
    operator: Op, 
    sym_table: &mut Box<SymTable<Type>>
) -> Type {
    if let Type::Function(op_function) = sym_table.get(&mut Symbol::Operator(operator)) {
        typecheck_call(
            op_function, 
            vec![typecheck(*left_expr, sym_table), typecheck(*right_expr, sym_table)]
        )
    } else {
        panic!("Undefined operator {:?}", operator)
    }
}

fn typecheck_unary_op(
    operand: Box<Expression>,
    operator: Op,
    sym_table: &mut Box<SymTable<Type>>
) -> Type {
    if let Type::Function(op_function) = sym_table.get(&mut Symbol::Operator(operator)) {
        typecheck_call(
            op_function, 
            vec![typecheck(*operand, sym_table)]
        )
    } else {
        panic!("Undefined operator {:?}", operator)
    }
}

fn typecheck_call_expression(
    callee: Box<Expression>,
    argument_expr: Vec<Box<Expression>>,
    sym_table: &mut Box<SymTable<Type>>
) -> Type {
    if let Expression::Identifier { value: function_id } = *callee {
        let called_function = sym_table.get(&Symbol::Identifier(function_id.to_string()));
        if let Type::Function(called_function) = called_function {
            let mut arg_types: Vec<Type> = vec![];
            for expr in argument_expr {
                arg_types.push(typecheck(*expr, sym_table));
            }
            typecheck_call(called_function, arg_types)
        } else {
            panic!("Calling undefined function {:?}", function_id);
        }
    } else {
        panic!("Callee of a call expression must be an identifier");
    }
}

fn typecheck_call(
    callee: Box<FunctionType>,
    argument_types: Vec<Type>
) -> Type {
    if callee.param_types.len() != argument_types.len() {
        panic!("Wrong number of arguments, expected {} but got {}", callee.param_types.len(), argument_types.len())
    }

    for (idx, param_type) in callee.param_types.iter().enumerate() {
        let arg_type = &argument_types[idx];
        if *arg_type != *param_type {
            panic!("Invalid argument type at index {}, expected {:?} but got {:?}", idx, param_type, arg_type)
        }
    }

    callee.return_type
}

fn typecheck(node: Expression, sym_table: &mut Box<SymTable<Type>>) -> Type {
    match node {
        Expression::IntegerLiteral {..} => Type::Integer,
        Expression::BooleanLiteral {..} => Type::Boolean,
        Expression::BinaryExpression { left, operator, right } => {
            typecheck_binary_op(left, right, operator, sym_table)
        },
        Expression::UnaryExpression { operand, operator } => {
            typecheck_unary_op(operand, operator, sym_table)
        }
        Expression::IfExpression { condition, then_branch, else_branch } => {
            let condition_result = typecheck(*condition, sym_table);
            if condition_result != Type::Boolean {
                panic!("If expression condition must be a {:?}, got {:?}", Type::Boolean, condition_result)
            }
            let then_result = typecheck(*then_branch, sym_table);

            if let Some(else_branch_expr) = else_branch {
                let else_result = typecheck(*else_branch_expr, sym_table);
                if then_result != else_result {
                    panic!("Then and else branch return types differ: {:?} != {:?}", then_result, else_result)
                }
            }

            then_result
        },
        Expression::BlockExpression { statements, result } => {
            sym_table.with_inner(|inner_sym_table| {
                for expr in statements {
                    typecheck(*expr, inner_sym_table);
                }
                typecheck(*result, inner_sym_table)
            })
        },
        Expression::Identifier { value } => sym_table.get(&Symbol::Identifier(value)),
        Expression::AssignmentExpression { left, right } => {
            if let Expression::Identifier { value: id } = *left {
                let value = typecheck(*right, sym_table);
                sym_table.assign(Symbol::Identifier(id), value)
            } else {
                panic!("Left side of an assignment must be an identifier");
            }
        },
        Expression::VariableDeclaration { id, init } => {
            if let Expression::Identifier { value } = *id {
                let init_value = typecheck(*init, sym_table);
                sym_table.symbols.insert(Symbol::Identifier(value), init_value);
                Type::Unit
            } else {
                panic!("Id of a variable declaration must be an identifier");
            }
        },
        Expression::CallExpression { callee, arguments } => {
            typecheck_call_expression(callee, arguments, sym_table)
        },
        Expression::Unit => Type::Unit,
    }
}

fn get_toplevel_sym_table() -> Box<SymTable<Type>> {
    let mut sym_table = SymTable::new(None);
    let builtins = get_builtin_function_symbol_type_mappings();
    for (symbol, val) in builtins {
        sym_table.symbols.insert(symbol, val);
    }
    sym_table
}

pub fn typecheck_program(node: Expression) -> Type {
    typecheck(node, &mut get_toplevel_sym_table())
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::{tokenize, Token};
    use crate::parser::parse;
    use super::*;

    fn t(src: &str) -> Type {
        let tokens: Vec<Token> = tokenize(src);
        let expression = parse(tokens);
        typecheck_program(expression)
    }

    #[test]
    fn typecheck_integers() {
        let res = t("7 + 3 * 2");
        assert_eq!(Type::Integer, res);
    }

    #[test]
    #[should_panic(expected="Invalid argument type at index 0, expected Integer but got Boolean")]
    fn simple_type_error() {
        t("false + 3 * 2");
    }

    #[test]
    #[should_panic(expected="Then and else branch return types differ: Unit != Boolean")]
    fn if_expr_type_error() {
        t("
            if (1 > 2) then {
                print_int(1)
            } else {
                false
            }
        ");
    }
}
