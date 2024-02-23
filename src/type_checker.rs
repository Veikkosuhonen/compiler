use crate::builtin_functions::get_builtin_function_symbol_type_mappings;
use crate::interpreter::UserDefinedFunction;
use crate::sym_table::{SymTable, Symbol};
use crate::parser::{ASTNode, Expression, Module};
use crate::tokenizer::Op;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub param_types: Vec<Type>,
    pub return_type: Type,
}

impl FunctionType {
    fn check_arg_count(&self, argument_expr: &Vec<&Box<TypedASTNode>>) {
        if self.param_types.len() != argument_expr.len() {
            panic!("Wrong number of arguments, expected {} but got {}", self.param_types.len(), argument_expr.len())
        }
    }

    fn typecheck_call(&self, argument_expr: &Vec<&Box<TypedASTNode>>) -> Type {
        self.check_arg_count(&argument_expr);
    
        for (idx, param_type) in self.param_types.iter().enumerate() {
            let arg = &argument_expr[idx];
            if arg.node_type != *param_type {
                panic!("Invalid argument type at index {}, expected {:?} but got {:?}", idx, param_type, arg.node_type)
            }
        }
    
        self.return_type.clone()
    }

    fn typecheck_operator_call(&self, op: Op, argument_expr: &Vec<&Box<TypedASTNode>>) -> Type {
        match op {
            Op::Equals | Op::NotEquals => {
                self.check_arg_count(argument_expr);
                let arg_type = argument_expr.get(0).unwrap();
                let arg_2_type = argument_expr.get(1).unwrap();
                if arg_type.node_type != arg_2_type.node_type {
                    panic!("Argument types for '{}' do not match: {:?} != {:?}", op.to_string(), arg_type, arg_2_type);
                }
                self.return_type.clone()
            },
            _ => self.typecheck_call(argument_expr)
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub enum Type {
    Integer,
    Boolean,
    Function(Box<FunctionType>),
    #[default] Unit,
}

#[derive(Debug, Clone)]
pub struct TypedASTNode {
    pub expr: Expression<TypedASTNode>,
    pub node_type: Type,
}

#[derive(Debug, Clone)]
pub struct TypedParam {
    pub param_type: Type,
    pub id: String,
}

#[derive(Debug, Clone)]
pub struct TypedUserDefinedFunction {
    pub id: String,
    pub body: Box<TypedASTNode>,
    pub params: Vec<String>,
    pub func_type: FunctionType,
}

pub fn typecheck_program(module: Module<UserDefinedFunction, ASTNode>) -> Module<TypedUserDefinedFunction, TypedASTNode> {
    let mut sym_table = get_toplevel_sym_table();
    let mut functions: Vec<TypedUserDefinedFunction> = vec![];

    for func in module.functions {
        let id = Symbol::Identifier(func.id.clone());
        let function_type = get_function_type(&func, &sym_table);
        sym_table.symbols.insert(id, Type::Function(Box::new(function_type.clone())));
        let typed_function = typecheck_function(func, function_type, &mut sym_table);
        functions.push(typed_function);
    }

    let typed_node = typecheck(*module.ast, &mut sym_table);

    Module { functions, ast: Box::new(typed_node) }
}

fn get_toplevel_sym_table() -> Box<SymTable<Type>> {
    let mut sym_table = SymTable::new(None);

    let builtins = get_builtin_function_symbol_type_mappings();
    for (symbol, val) in builtins {
        sym_table.symbols.insert(symbol, val);
    }

    sym_table.symbols.insert(Symbol::Identifier("Int".to_string()), Type::Integer);
    sym_table.symbols.insert(Symbol::Identifier("Bool".to_string()), Type::Boolean);
    sym_table.symbols.insert(Symbol::Identifier("Unit".to_string()), Type::Unit);

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
        Expression::VariableDeclaration { id, init, type_annotation } => {
            typecheck_variable_declaration(id, init, type_annotation, sym_table)
        },
        Expression::CallExpression { callee, arguments } => {
            typecheck_call_expression(callee, arguments, sym_table)
        },
    }
}

fn get_function_type(
    func: &UserDefinedFunction,
    sym_table: &Box<SymTable<Type>>,
) -> FunctionType {
    let params: Vec<(Symbol, Type)> = func.params.iter().map(|param| {
        let sym = Symbol::Identifier(param.name.clone());
        (sym, sym_table.get(&Symbol::Identifier(param.param_type.clone())))
    }).collect();
    let return_type_name = func.return_type.clone().unwrap_or(String::from("Unit"));
    let return_type = sym_table.get(&Symbol::Identifier(return_type_name));
    FunctionType { 
        param_types: params.iter().map(|(_, val)| val.clone()).collect(),
        return_type
    }
}

fn typecheck_function(
    func: UserDefinedFunction,
    func_type: FunctionType,
    sym_table: &mut Box<SymTable<Type>>
) -> TypedUserDefinedFunction {
    let params = func.params.iter().enumerate().map(|(idx, param)| {
        let sym = Symbol::Identifier(param.name.clone());
        (sym, func_type.param_types.get(idx).unwrap().clone())
    }).collect::<Vec<(Symbol, Type)>>();

    let body = sym_table.with_inner_given_args(&params, |inner| {
        typecheck(*func.body, inner)
    });

    TypedUserDefinedFunction {
        id: func.id,
        body: Box::new(body),
        params: func.params.iter().map(|param| param.name.clone()).collect(),
        func_type,
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
        let variable_type = sym_table.get(&Symbol::Identifier(id.clone()));
        let right = Box::new( typecheck(*right, sym_table) );
        if variable_type != right.node_type {
            panic!("Variable type and assignment value types differ: {:?} != {:?}", variable_type, right.node_type)
        }
        let left = Box::new(TypedASTNode { expr: Expression::Identifier { value: id }, node_type: variable_type.clone() });
        TypedASTNode { 
            expr: Expression::AssignmentExpression { 
                left,
                right 
            }, 
            node_type: variable_type.clone()
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
    type_annotation: Option<Box<ASTNode>>,
    sym_table: &mut Box<SymTable<Type>>
)   -> TypedASTNode {
    if let Expression::Identifier { value } = id.expr {
        let init = typecheck(*init, sym_table);
        if let Some(type_annotation) = type_annotation {
            let type_name = match type_annotation.expr {
                Expression::Identifier { value } => value,
                _ => panic!("Type annotation must be an identifier")
            };
            let annotated_type = sym_table.get(&Symbol::Identifier(type_name));
            if init.node_type != annotated_type {
                panic!("Type annotation and init expression types differ: {:?} != {:?}", annotated_type, init.node_type)
            }
        }
        sym_table.symbols.insert(Symbol::Identifier(value.clone()), init.node_type.clone());
        TypedASTNode { 
            expr: Expression::VariableDeclaration { 
                id: Box::new(TypedASTNode { expr: Expression::Identifier { value }, node_type: init.node_type.clone() }), 
                init: Box::new(init),
                type_annotation: None,
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
        let node_type = op_function.typecheck_operator_call(operator, &vec![&left, &right]);
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
        let node_type = op_function.typecheck_operator_call(operator, &vec![&operand]);
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
            let node_type = called_function.typecheck_call(
                &typed_argument_expr.iter().collect()
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

#[cfg(test)]
mod tests {
    use crate::tokenizer::{tokenize, Token};
    use crate::parser::parse;
    use super::*;

    fn t(src: &str) -> TypedASTNode {
        let tokens: Vec<Token> = tokenize(src).expect("Tokenizing to succeed");
        let module = parse(tokens).expect("Parsing to succeed");
        let module = typecheck_program(module);
        *module.ast
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
            if let Expression::BlockExpression { result,.. } = result.expr {
                if let Expression::VariableDeclaration { id, init,.. } = result.expr {
                    assert_eq!(id.node_type, Type::Integer);
                    assert_eq!(init.node_type, Type::Integer);
                } else {
                    panic!("Expected VariableDec, got {:?}", result.expr)
                }
            } else {
                panic!("Expected Block, got {:?}", result.expr)
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
        if let Expression::BlockExpression { result,.. } = node.expr {
            if let Expression::IfExpression { condition, .. } = result.expr {
                assert_eq!(condition.node_type, Type::Boolean);
                if let Expression::BinaryExpression { left, right, .. } = condition.expr {
                    assert_eq!(left.node_type, Type::Integer);
                    assert_eq!(right.node_type, Type::Integer);
                } else {
                    panic!("Fakd")
                }
            } else {
                panic!("Wrong, got {:?}", result.expr)
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

    #[test]
    fn compare_eq_booleans() {
        let node = t("false == true");
        assert_eq!(node.node_type, Type::Boolean);
    }

    #[test]
    fn compare_eq_integer() {
        let node = t("123 == 123");
        assert_eq!(node.node_type, Type::Boolean);
    }

    #[test]
    fn type_annotation() {
        let node = t("var x: Int = 123");
        assert_eq!(node.node_type, Type::Unit);
    }

    #[test]
    #[should_panic(expected="Type annotation and init expression types differ: Integer != Boolean")]
    fn type_annotation_int_error() {
        t("var x: Int = false");
    }

    #[test]
    fn typed_variable_assignment() {
        let node = t("var x: Int = 123; x = 456");
        assert_eq!(node.node_type, Type::Integer);
    }

    #[test]
    #[should_panic(expected="Variable type and assignment value types differ: Integer != Boolean")]
    fn typed_variable_assignment_error() {
        t("var x: Int = 123; x = false");
    }

    #[test]
    fn typecheck_function_call() {
        let node = t("
            fun add(x: Int, y: Int): Int {
                x + y
            }
            add(1, 2)
        ");
        assert_eq!(node.node_type, Type::Integer);
    }

    #[test]
    fn recursion() {
        let node = t("
        fun recurse(x: Int, i: Int): Int {
            if i > 10 then {
                x
            } else {
                recurse(2 * x, i + 1)
            }
        }
        recurse(2, 10)
        ");
        assert_eq!(node.node_type, Type::Integer);
    }
}
