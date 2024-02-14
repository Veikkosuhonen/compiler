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

pub fn typecheck_program(node: ASTNode) -> TypedASTNode {
    typecheck(node, &mut get_toplevel_sym_table())
}

fn get_toplevel_sym_table() -> Box<SymTable<Type>> {
    let mut sym_table = SymTable::new(None);
    let builtins = get_builtin_function_symbol_type_mappings();
    for (symbol, val) in builtins {
        sym_table.symbols.insert(symbol, val);
    }
    sym_table
}

fn typecheck(node: ASTNode, sym_table: &mut Box<SymTable<Type>>) -> TypedASTNode {
    match node.expr {
        Expression::Unit => TypedASTNode { expr: Expression::Unit, node_type: Type::Unit },
        Expression::IntegerLiteral { value } => TypedASTNode { expr: Expression::IntegerLiteral { value }, node_type: Type::Integer },
        Expression::BooleanLiteral { value } => TypedASTNode { expr: Expression::BooleanLiteral { value }, node_type: Type::Boolean },
        Expression::Identifier { value } => typecheck_identifier(value, sym_table),
        Expression::BinaryExpression { left, operator, right } => {
            typecheck_binary_op(left, right, operator, sym_table)
        },
        Expression::UnaryExpression { operand, operator } => {
            typecheck_unary_op(operand, operator, sym_table)
        }
        Expression::IfExpression { condition, then_branch, else_branch } => {
            typecheck_if_expression(condition, then_branch, else_branch, sym_table)
        },
        Expression::WhileExpression { condition, body } => {
            typecheck_while_expression(condition, body, sym_table)
        },
        Expression::BlockExpression { statements, result } => {
            typecheck_block_expression(statements, result, sym_table)
        },
        Expression::AssignmentExpression { left, right } => {
            typecheck_assignment_expression(left, right, sym_table)
        },
        Expression::VariableDeclaration { id, init } => {
            typecheck_variable_declaration(id, init, sym_table)
        },
        Expression::CallExpression { callee, arguments } => {
            typecheck_call_expression(callee, arguments, sym_table)
        },
    }
}

fn typecheck_identifier(
    value: String,
    sym_table: &mut Box<SymTable<Type>>,
) -> TypedASTNode {
    TypedASTNode { expr: Expression::Identifier { value: value.clone() }, node_type: sym_table.get(&Symbol::Identifier(value)) }
}

fn typecheck_assignment_expression(
    left: Box<ASTNode>, 
    right: Box<ASTNode>, 
    sym_table: &mut Box<SymTable<Type>>
)   -> TypedASTNode {
    if let Expression::Identifier { value: id } = left.expr {
        let right = Box::new( typecheck(*right, sym_table) );
        sym_table.assign(Symbol::Identifier(id.clone()), right.node_type.clone());
        let node_type = right.node_type.clone();
        let left = Box::new(TypedASTNode { expr: Expression::Identifier { value: id }, node_type: node_type.clone() });
        TypedASTNode { 
            expr: Expression::AssignmentExpression { 
                left,
                right 
            }, 
            node_type: node_type.clone()
        }
    } else {
        panic!("Left side of an assignment must be an identifier");
    }
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

fn typecheck_while_expression(
    condition: Box<ASTNode>, 
    body: Box<ASTNode>, 
    sym_table: &mut Box<SymTable<Type>>
) -> TypedASTNode {
    let condition = Box::new(typecheck(*condition, sym_table));
    if condition.node_type != Type::Boolean {
        panic!("If expression condition must be a {:?}, got {:?}", Type::Boolean, condition.node_type)
    }
    let body = Box::new(typecheck(*body, sym_table));
    let node_type = body.node_type.clone();

    TypedASTNode {
        expr: Expression::WhileExpression { condition, body },
        node_type,
    }
}

fn typecheck_block_expression(
    statements: Vec<Box<ASTNode>>,
    result: Box<ASTNode>,
    sym_table: &mut Box<SymTable<Type>>
) -> TypedASTNode {
    sym_table.with_inner(|inner_sym_table| {
        let mut typed_statements: Vec<Box<TypedASTNode>> = vec![];
        for expr in statements {
            typed_statements.push(Box::new(typecheck(*expr, inner_sym_table)));
        }
        let result = Box::new(typecheck(*result, inner_sym_table));
        let node_type = result.node_type.clone();
        TypedASTNode { expr: Expression::BlockExpression { statements: typed_statements, result }, node_type }
    })
}

fn typecheck_variable_declaration(
    id: Box<ASTNode>, 
    init: Box<ASTNode>, 
    sym_table: &mut Box<SymTable<Type>>
)   -> TypedASTNode {
    if let Expression::Identifier { value } = id.expr {
        let init = typecheck(*init, sym_table);
        sym_table.symbols.insert(Symbol::Identifier(value.clone()), init.node_type.clone());
        TypedASTNode { 
            expr: Expression::VariableDeclaration { 
                id: Box::new(TypedASTNode { expr: Expression::Identifier { value }, node_type: init.node_type.clone() }), 
                init: Box::new(init),
            }, 
            node_type: Type::Unit
        }
    } else {
        panic!("Id of a variable declaration must be an identifier");
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
    fn typecheck_block() {
        let res = t("{ var x = 789 }");
        // ASTNode { expr: BlockExpression { statements: [], result: ASTNode { expr: VariableDeclaration { id: ASTNode { expr: Identifier { value: "x" } }, init: ASTNode { expr: IntegerLiteral { value: 789 } } } } } }
        assert_eq!(Type::Unit, res.node_type);
        if let Expression::BlockExpression { result,.. } = res.expr {
            if let Expression::VariableDeclaration { id, init } = result.expr {
                assert_eq!(id.node_type, Type::Integer);
                assert_eq!(init.node_type, Type::Integer);
            } else {
                panic!("Expected VariableDec, got {:?}", result.expr)
            }
        } else {
            panic!("Expected Block, got {:?}", res.expr)
        }
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

    #[test]
    fn while_expr() {
        let node = t("
            while true do {
                100
            }
        ");

        assert_eq!(node.node_type, Type::Integer);
    }

    #[test]
    fn unary_sub() {
        let node = t("
            7 * -7
        ");

        assert_eq!(node.node_type, Type::Integer);
    }
}
