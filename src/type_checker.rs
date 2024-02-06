use crate::builtin_functions::get_builtin_function_symbol_type_mappings;
use crate::sym_table::{SymTable, Symbol};
use crate::parser::{ASTNode, Expression};
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

#[derive(Debug)]
pub struct TypedASTNode {
    pub expr: Expression<TypedASTNode>,
    pub node_type: Type,
}

fn typecheck_identifier(
    value: String,
    sym_table: &mut Box<SymTable<Type>>,
) -> TypedASTNode {
    TypedASTNode { expr: Expression::Identifier { value: value.clone() }, node_type: sym_table.get(&Symbol::Identifier(value)) }
}

fn typecheck_if_expression(
    condition: Box<ASTNode>, 
    then_branch: Box<ASTNode>, 
    else_branch: Option<Box<ASTNode>>, 
    sym_table: &mut Box<SymTable<Type>>
) -> TypedASTNode {
    let condition = Box::new(typecheck(*condition, sym_table));
    if condition.node_type != Type::Boolean {
        panic!("If expression condition must be a {:?}, got {:?}", Type::Boolean, condition.node_type)
    }
    let then_branch = Box::new(typecheck(*then_branch, sym_table));
    let node_type = then_branch.node_type.clone();

    let mut else_branch_opt: Option<Box<TypedASTNode>> = None;
    if let Some(else_branch_expr) = else_branch {
        let else_result = typecheck(*else_branch_expr, sym_table);
        if then_branch.node_type != else_result.node_type {
            panic!("Then and else branch return types differ: {:?} != {:?}", then_branch.node_type, else_result.node_type)
        }
        else_branch_opt = Some(Box::new(else_result))
    }

    TypedASTNode {
        expr: Expression::IfExpression { condition, then_branch, else_branch: else_branch_opt },
        node_type,
    }
}

fn typecheck_binary_op(
    left_expr: Box<ASTNode>, 
    right_expr: Box<ASTNode>, 
    operator: Op, 
    sym_table: &mut Box<SymTable<Type>>
) -> TypedASTNode {
    if let Type::Function(op_function) = sym_table.get(&mut Symbol::Operator(operator)) {
        let left  = Box::new(typecheck(*left_expr, sym_table));
        let right = Box::new(typecheck(*right_expr, sym_table));
        let node_type = typecheck_call(
            op_function, 
            vec![&left, &right]
        );
        TypedASTNode { expr: Expression::BinaryExpression { left, operator, right }, node_type }
    } else {
        panic!("Undefined operator {:?}", operator)
    }
}

fn typecheck_unary_op(
    operand: Box<ASTNode>,
    operator: Op,
    sym_table: &mut Box<SymTable<Type>>
) -> TypedASTNode {
    if let Type::Function(op_function) = sym_table.get(&mut Symbol::Operator(operator)) {
        let operand = Box::new(typecheck(*operand, sym_table));
        let node_type = typecheck_call(
            op_function, 
            vec![&operand]
        );
        TypedASTNode { expr: Expression::UnaryExpression { operand, operator }, node_type }
    } else {
        panic!("Undefined operator {:?}", operator)
    }
}

fn typecheck_call_expression(
    callee: Box<ASTNode>,
    argument_expr: Vec<Box<ASTNode>>,
    sym_table: &mut Box<SymTable<Type>>
) -> TypedASTNode {
    if let Expression::Identifier { value: function_id } = callee.expr {
        let called_function = sym_table.get(&Symbol::Identifier(function_id.to_string()));
        if let Type::Function(called_function) = called_function {
            let mut typed_argument_expr: Vec<Box<TypedASTNode>> = vec![];
            for expr in argument_expr {
                let arg = Box::new(typecheck(*expr, sym_table));
                typed_argument_expr.push(arg);
            }
            let node_type = typecheck_call(
                called_function, 
                typed_argument_expr.iter().collect()
            );
            TypedASTNode {
                expr: Expression::CallExpression { 
                    callee: Box::new(typecheck_identifier(function_id, sym_table)),
                    arguments: typed_argument_expr, 
                },
                node_type,
            }
        } else {
            panic!("Calling undefined function {:?}", function_id);
        }
    } else {
        panic!("Callee of a call expression must be an identifier");
    }
}

fn typecheck_call(
    callee: Box<FunctionType>,
    argument_expr: Vec<&Box<TypedASTNode>>
) -> Type {
    if callee.param_types.len() != argument_expr.len() {
        panic!("Wrong number of arguments, expected {} but got {}", callee.param_types.len(), argument_expr.len())
    }

    for (idx, param_type) in callee.param_types.iter().enumerate() {
        let arg = &argument_expr[idx];
        if arg.node_type != *param_type {
            panic!("Invalid argument type at index {}, expected {:?} but got {:?}", idx, param_type, arg.node_type)
        }
    }

    callee.return_type
}

fn typecheck(node: ASTNode, sym_table: &mut Box<SymTable<Type>>) -> TypedASTNode {
    match node.expr {
        Expression::Unit => TypedASTNode { expr: Expression::Unit, node_type: Type::Unit },
        Expression::IntegerLiteral { value } => TypedASTNode { expr: Expression::IntegerLiteral { value }, node_type: Type::Integer },
        Expression::BooleanLiteral { value } => TypedASTNode { expr: Expression::BooleanLiteral { value }, node_type: Type::Boolean },
        Expression::BinaryExpression { left, operator, right } => {
            typecheck_binary_op(left, right, operator, sym_table)
        },
        Expression::UnaryExpression { operand, operator } => {
            typecheck_unary_op(operand, operator, sym_table)
        }
        Expression::IfExpression { condition, then_branch, else_branch } => {
            typecheck_if_expression(condition, then_branch, else_branch, sym_table)
        },
        Expression::BlockExpression { statements, result } => {
            sym_table.with_inner(|inner_sym_table| {
                for expr in statements {
                    typecheck(*expr, inner_sym_table);
                }
                typecheck(*result, inner_sym_table)
            })
        },
        Expression::Identifier { value } => typecheck_identifier(value, sym_table),
        Expression::AssignmentExpression { left, right } => {
            if let Expression::Identifier { value: id } = left.expr {
                let value = typecheck(*right, sym_table);
                sym_table.assign(Symbol::Identifier(id), value.node_type.clone());
                value
            } else {
                panic!("Left side of an assignment must be an identifier");
            }
        },
        Expression::VariableDeclaration { id, init } => {
            if let Expression::Identifier { value } = id.expr {
                let init_value = typecheck(*init, sym_table);
                sym_table.symbols.insert(Symbol::Identifier(value), init_value.node_type);
                TypedASTNode { expr: Expression::Unit, node_type: Type::Unit }
            } else {
                panic!("Id of a variable declaration must be an identifier");
            }
        },
        Expression::CallExpression { callee, arguments } => {
            typecheck_call_expression(callee, arguments, sym_table)
        },
        _ => todo!("Not yet implemented")
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

pub fn typecheck_program(node: ASTNode) -> TypedASTNode {
    typecheck(node, &mut get_toplevel_sym_table())
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::{tokenize, Token};
    use crate::parser::parse;
    use super::*;

    fn t(src: &str) -> TypedASTNode {
        let tokens: Vec<Token> = tokenize(src);
        let expression = parse(tokens);
        typecheck_program(expression)
    }

    #[test]
    fn typecheck_integers() {
        let res = t("7 + 3 * 2");
        assert_eq!(Type::Integer, res.node_type);
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

    #[test]
    fn ast_gets_types() {
        let node = t("
            if (1 > 2) then {
                true
            } else {
                false
            }
        ");

        assert_eq!(node.node_type, Type::Boolean);
        if let Expression::IfExpression { condition, .. } = node.expr {
            assert_eq!(condition.node_type, Type::Boolean);
            if let Expression::BinaryExpression { left, right, .. } = condition.expr {
                assert_eq!(left.node_type, Type::Integer);
                assert_eq!(right.node_type, Type::Integer);
            } else {
                panic!("Fakd")
            }
        } else {
            panic!("Wrong, got {:?}", node.expr)
        }
    }
}
