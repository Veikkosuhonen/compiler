use core::fmt;
use std::collections::HashMap;

use crate::builtin_functions::{get_builtin_function_and_operator_types, get_builtin_referrable_types};
use crate::sym_table::{SymTable, Symbol};
use crate::parser::{ASTNode, Expr, Module, Struct, UserDefinedFunction};
use crate::tokenizer::{Op, SourceLocation};

#[derive(Clone, PartialEq, Default)]
pub struct FunctionType {
    pub param_types: Vec<TypedParam>,
    pub return_type: Type,
}

impl fmt::Debug for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fm = f.debug_struct("FunctionType");
        for p in &self.param_types {
            fm.field(&p.name, &p.param_type);
        }
        fm.field("return", &self.return_type);
        fm.finish()
    }
}

impl FunctionType {
    fn check_arg_count(&self, argument_expr_len: usize) {
        if self.param_types.len() != argument_expr_len {
            panic!("Wrong number of arguments, expected {} but got {}", self.param_types.len(), argument_expr_len)
        }
    }

    fn typecheck_unnamed_args_call(&self, argument_expr: &Vec<&Box<TypedASTNode>>) -> Type {
        self.check_arg_count(argument_expr.len());
        let mut constraints: HashMap<String, Type> = HashMap::new();
    
        for (idx, param) in self.param_types.iter().enumerate() {
            let arg = &argument_expr[idx];

            let mut resolution = arg.node_type.satisfy(&param.param_type);
            if let Some((type_id, type_arg)) = resolution.constraint {
                if let Some(required_type) = constraints.get(&type_id) {
                    // The following resolution will either be resolved or a fail, it wont have constraints because we dont allow generic arg to satisfy generic type param.
                    if !type_arg.satisfy(required_type).is_resolved() {
                        panic!("Invalid argument type at index {}, expected {:?} but got {:?}", idx, required_type, type_arg)
                    }
                } else {
                    constraints.insert(type_id, type_arg);
                }
            }

            resolution.constraint = None;
            if !resolution.is_resolved() {
                panic!("Invalid argument type at index {}, expected {:?} but got {:?}", idx, param.param_type, arg.node_type)
            }
        }

        self.return_type.resolve(&mut constraints)
    }

    fn typecheck_named_args_call(&self, argument_expr: &Vec<(String, Box<TypedASTNode>)>) -> Type {
        self.check_arg_count(argument_expr.len());
        let mut constraints: HashMap<String, Type> = HashMap::new();
    
        for arg in argument_expr {
            let param = self.param_types.iter().find(|param| param.name == arg.0).expect(format!("No such parameter: {}", arg.0).as_str());
            let arg_name = &arg.0;
            let arg = &arg.1;
        
            let mut resolution = arg.node_type.satisfy(&param.param_type);
            if let Some((type_id, type_arg)) = resolution.constraint {
                if let Some(required_type) = constraints.get(&type_id) {
                    // The following resolution will either be resolved or a fail, it wont have constraints because we dont allow generic arg to satisfy generic type param.
                    if !type_arg.satisfy(required_type).is_resolved() {
                        panic!("Invalid argument type for param {}, expected {:?} but got {:?}", arg_name, required_type, type_arg)
                    }
                } else {
                    constraints.insert(type_id, type_arg);
                }
            }

            resolution.constraint = None;
            if !resolution.is_resolved() {
                panic!("Invalid argument type for param {}, expected {:?} but got {:?}", arg_name, param.param_type, arg.node_type)
            }
        }

        self.return_type.resolve(&mut constraints)
    }

    pub fn unnamed_params(param_types: Vec<Type>, return_type: Type) -> FunctionType {
        FunctionType {
            param_types: param_types.iter().map(|t| TypedParam { name: "".to_string(), param_type: t.clone() }).collect(),
            return_type
        }
    }
}

#[derive(Clone, Default, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Function(Box<FunctionType>),
    Struct(TypedStruct),
    Pointer(Box<Type>),
    Generic(String),
    Typeref(Box<Type>),
    Constructor(Box<Type>),
    Unknown,
    #[default] Unit,
}

impl Type {
    pub fn get_constructor_type(&self) -> FunctionType {
        let return_type = Type::Typeref(Box::new(self.clone()));
        let param_types = match self {
            Type::Struct(struct_type) => struct_type.fields.clone(),
            _ => vec![TypedParam { name: "val".to_string(), param_type: self.clone() }],
        };
        FunctionType { param_types, return_type }
    }

    pub fn size(&self) -> usize {
        match self {
            Type::Int => 1,
            Type::Bool => 1,
            Type::Struct(stype) => stype.fields.len(),
            Type::Pointer(_) => 1,
            Type::Unit => 1,
            _ => todo!("Size of {:?}", self)
        }
    }

    pub fn get_fields(&self) -> Vec<TypedParam> {
        match self {
            Type::Struct(struct_type) => struct_type.fields.clone(),
            _ => vec![TypedParam { name: String::from("value"), param_type: self.clone() }]
        }
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int =>            f.debug_tuple("Int").finish(),
            Type::Bool =>           f.debug_tuple("Bool").finish(),
            Type::Function(func) =>    f.write_fmt(format_args!("{:?} -> {:?}", func.param_types, func.return_type)),
            Type::Struct(s) =>      f.write_fmt(format_args!("{} {:?}", s.id, s.fields)),
            Type::Pointer(p) =>     f.write_fmt(format_args!("Pointer<{:?}>", p)),
            Type::Generic(g) =>     f.write_str(g),
            Type::Typeref(t) =>     f.write_fmt(format_args!("Typeref<{:?}>", t)),
            Type::Constructor(c) => f.write_fmt(format_args!("Constructor<{:?}>", c)),
            Type::Unknown =>        f.debug_tuple("Unknown").finish(),
            Type::Unit =>           f.debug_tuple("Unit").finish(),
        }
    }
}

#[derive(Debug)]
pub struct TypeResolution {
    satisfied: bool,
    constraint: Option<(String, Type)>,
}

impl TypeResolution {
    pub fn failed() -> TypeResolution {
        TypeResolution { satisfied: false, constraint: None }
    }
    pub fn satisfied() -> TypeResolution {
        TypeResolution { satisfied: true, constraint: None }
    }
    pub fn constrained(constraint: Option<(String, Type)>) -> TypeResolution {
        TypeResolution { satisfied: true, constraint, }
    }
    pub fn is_resolved(&self) -> bool {
        self.satisfied && self.constraint.is_none()
    }
}

impl Type {
    pub fn satisfy(&self, other: &Self) -> TypeResolution {
        // println!("{:?}\n satisfy\n {:?}", self, other);
        let res = match other {
            // Any non-generic type satisfies a generic type, but it produces a constraint
            Type::Generic(type_id) => match self {
                Type::Generic(_) => TypeResolution::failed(),
                _ => TypeResolution::constrained(Some((type_id.clone(), self.clone()))),
            }
            // Pointer value type must be satisfied
            Type::Pointer(other_pointer_type) => match self {
                Type::Pointer(self_pointer_type) => self_pointer_type.satisfy(&other_pointer_type),
                _ => TypeResolution::failed(),
            },
            // Constructor value type must be satisfied
            Type::Constructor(other_constructor_type) => match self {
                Type::Constructor(self_constructor_type) => self_constructor_type.satisfy(&other_constructor_type),
                _ => TypeResolution::failed(),
            },
            // Everything satisfies the Unknown type
            Type::Unknown => TypeResolution::satisfied(),
            _ => if *self == *other { TypeResolution::satisfied() } else { TypeResolution::failed() },
        };
        // println!("{:?}", res);
        res
    }

    pub fn resolve(&self, constraints: &mut HashMap<String, Type>) -> Type {
        match self {
            // Generic type must resolve from constraints
            Type::Generic(type_id) => constraints.remove(type_id).expect(format!("Type resolution failed for {:?}", self).as_str()),
            // Pointer value must be resolved
            Type::Pointer(pointer_type) => Type::Pointer(Box::new(pointer_type.resolve(constraints))),
            Type::Constructor(constructor_type) => Type::Constructor(Box::new(constructor_type.resolve(constraints))),
            _ => self.clone(),
        }
    }

    pub fn generic(type_id: &str) -> Type {
        Type::Generic(type_id.to_string())
    }

    pub fn get_callable_type(&self) -> FunctionType {
        match self {
            Type::Function(ftype) => *ftype.clone(),
            Type::Typeref(referred_type) => match referred_type.as_ref() {
                Type::Struct(struct_type) => FunctionType {
                    param_types: struct_type.fields.clone(),
                    return_type: Type::Constructor(Box::new(*referred_type.clone())),
                },
                _ => FunctionType::unnamed_params(
                    vec![*referred_type.clone()],
                    Type::Constructor(Box::new(*referred_type.clone()))
                ),
            },
            _ => panic!("{:?} is not callable", self)
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedASTNode {
    pub expr: Expr<TypedASTNode>,
    pub node_type: Type,
}

impl TypedASTNode {
    pub fn new(expr: Expr<TypedASTNode>, node_type: Type, _start: SourceLocation, _end: SourceLocation) -> TypedASTNode {
        TypedASTNode { expr, node_type }
    }
}

#[derive(Clone, PartialEq)]
pub struct TypedParam {
    pub name: String,
    pub param_type: Type,
}

impl fmt::Debug for TypedParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}: {:?}", self.name, self.param_type))
    }
}

#[derive(Clone)]
pub struct TypedUserDefinedFunction {
    pub id: String,
    pub body: Box<TypedASTNode>,
    pub params: Vec<String>,
    pub func_type: FunctionType,
}

impl fmt::Debug for TypedUserDefinedFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TypedUserDefinedFunction")
            .field("id", &self.id)
            .finish()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedStruct {
    pub id: String,
    pub fields: Vec<TypedParam>,
}

impl TypedStruct {
    pub fn get_member(&self, value: &String) -> (usize, Type) {
        let (offset, param) = self.fields.iter().enumerate().find(|(_, param)| param.name == *value).unwrap();
        (offset, param.param_type.clone())
    }
}

impl Module<TypedUserDefinedFunction, TypedStruct> {
    pub fn main(&self) -> &TypedUserDefinedFunction {
        self.functions.iter().find(|func| func.id == "main").expect("Main does not exist")
    }

    /// Test helper
    pub fn last(&self) -> &Box<TypedASTNode> {
        match &self.main().body.expr {
            Expr::Block { statements, result } => {
                if matches!(result.expr, Expr::Unit) {
                    return statements.last().unwrap();
                }
                result
            },
            _ => panic!("Main body should always be a block")
        }
    }
}

pub fn typecheck_program(module: Module<UserDefinedFunction, Struct>) -> Module<TypedUserDefinedFunction, TypedStruct> {
    let mut sym_table = get_toplevel_sym_table();
    let mut functions: Vec<TypedUserDefinedFunction> = vec![];
    let mut structs: Vec<TypedStruct> = vec![];

    for struct_def in &module.structs {
        let id = Symbol::Identifier(struct_def.id.clone());
        let struct_type = get_struct_type(&struct_def, &mut sym_table);
        sym_table.symbols.insert(id, Type::Typeref(Box::new(Type::Struct(struct_type.clone()))));
    }

    for func in &module.functions {
        let id = Symbol::Identifier(func.id.clone());
        let function_type = get_function_type(&func, &mut sym_table);
        sym_table.symbols.insert(id, Type::Function(Box::new(function_type.clone())));
    }

    for struct_def in module.structs {
        let struct_type_ref = sym_table.get(&Symbol::Identifier(struct_def.id.clone()));
        if let Type::Typeref(struct_type) = struct_type_ref {
            if let Type::Struct(struct_type) = *struct_type {
                let typed_struct = TypedStruct {
                    id: struct_def.id, 
                    fields: struct_type.fields, 
                };
                structs.push(typed_struct);
            } else {
                panic!("{}'s declared type is not a struct", struct_def.id)
            }
        } else {
            panic!("{} has no defined type", struct_def.id)
        }
    }

    for func in module.functions {
        let function_type = sym_table.get(&Symbol::Identifier(func.id.clone()));
        if let Type::Function(function_type) = function_type {
            let typed_function = typecheck_function(func, *function_type, &mut sym_table);
            functions.push(typed_function);
        } else {
            panic!("{}'s declared type is not a function", func.id)
        }
    }

    Module { functions, structs }
}

fn get_toplevel_sym_table() -> Box<SymTable<Symbol,Type>> {
    let mut sym_table = SymTable::new(None);

    let builtins = get_builtin_function_and_operator_types();
    for (symbol, val) in builtins {
        sym_table.symbols.insert(symbol, val);
    }

    for (symbol, ref_type) in get_builtin_referrable_types() {
        sym_table.symbols.insert(symbol, ref_type);
    }

    sym_table
}

fn typecheck(node: ASTNode, sym_table: &mut Box<SymTable<Symbol,Type>>) -> TypedASTNode {
    match node.expr {
        Expr::Unit => TypedASTNode { expr: Expr::Unit, node_type: Type::Unit },
        Expr::IntegerLiteral { value } => TypedASTNode { expr: Expr::IntegerLiteral { value }, node_type: Type::Int },
        Expr::BooleanLiteral { value } => TypedASTNode { expr: Expr::BooleanLiteral { value }, node_type: Type::Bool },
        Expr::Identifier { value } => typecheck_identifier(value, sym_table),
        Expr::Logical { left, operator, right } => {
            typecheck_logical_op(left, right, operator, sym_table)
        },
        Expr::Binary { left, operator, right } => {
            typecheck_binary_op(left, right, operator, sym_table)
        },
        Expr::Unary { operand, operator } => {
            typecheck_unary_op(operand, operator, sym_table)
        },
        Expr::If { condition, then_branch, else_branch } => {
            typecheck_if_expression(condition, then_branch, else_branch, sym_table)
        },
        Expr::While { condition, body } => {
            typecheck_while_expression(condition, body, sym_table)
        },
        Expr::Block { statements, result } => {
            typecheck_block_expression(statements, result, sym_table)
        },
        Expr::Assignment { left, right } => {
            typecheck_assignment_expression(left, right, sym_table)
        },
        Expr::VariableDeclaration { id, init, type_annotation } => {
            typecheck_variable_declaration(id, init, type_annotation, sym_table)
        },
        Expr::Call { callee, arguments } => {
            typecheck_call_expression(callee, arguments, sym_table)
        },
        Expr::Return { result } => {
            let result = typecheck(*result, sym_table);
            if !result.node_type.satisfy(&sym_table.expected_returns).is_resolved() {
                panic!("Wrong return type in return expression: annotated {:?} but found {:?}", sym_table.expected_returns, result.node_type)
            }
            sym_table.returns = Some(result.node_type.clone());
            TypedASTNode { expr: Expr::Return { result: Box::new(result) }, node_type: Type::Unit }
        },
        Expr::StructInstance { struct_name, fields } => {
            typecheck_struct_instance(struct_name, fields, sym_table)
        },
        Expr::Member { parent, name } => {
            let parent = typecheck(*parent, sym_table);
            let struct_type = match &parent.node_type {
                Type::Pointer(pointer_type) => match pointer_type.as_ref() {
                    Type::Struct(struct_type) => struct_type,
                    _ => panic!("Left side pointer of a member expression must point to a struct")
                }
                _ => panic!("Left side of a member expression must be a pointer to a struct"),
            };
            
            let member_type = struct_type.fields
                .iter()
                .find(|param| param.name == name)
                .expect(format!("Struct does not have a member '{name}'").as_str())
                .param_type.clone();
            
            TypedASTNode {
                expr: Expr::Member { parent: Box::new(parent), name },
                node_type: member_type,
            }
        },
        Expr::Break => TypedASTNode { expr: Expr::Break, node_type: Type::Unit },
        Expr::Continue => TypedASTNode { expr: Expr::Continue, node_type: Type::Unit }
    }
}

fn get_function_type(
    func: &UserDefinedFunction,
    sym_table: &mut Box<SymTable<Symbol,Type>>,
) -> FunctionType {
    let param_types: Vec<TypedParam> = func.params.iter().map(|param| {
        TypedParam { name: param.name.clone(), param_type: typecheck_type_annotation_referred_type(&param.param_type, sym_table) }
    }).collect();
    let return_type_name = &func.return_type;
    let return_type = typecheck_type_annotation_referred_type(&return_type_name, sym_table);

    FunctionType { 
        param_types,
        return_type
    }
}

fn get_struct_type(
    struct_def: &Struct,
    sym_table: &mut Box<SymTable<Symbol,Type>>,
) -> TypedStruct {
    let fields: Vec<TypedParam> = struct_def.fields.iter().map(|field| {
        TypedParam {
            name: field.name.clone(),
            param_type: typecheck_type_annotation_referred_type(&field.param_type, sym_table)
        }
    }).collect();

    TypedStruct { fields, id: struct_def.id.clone() }
}

fn typecheck_function(
    func: UserDefinedFunction,
    func_type: FunctionType,
    sym_table: &mut Box<SymTable<Symbol,Type>>
) -> TypedUserDefinedFunction {
    let params = func.params.iter().enumerate().map(|(idx, param)| {
        let sym = Symbol::Identifier(param.name.clone());
        (sym, func_type.param_types.get(idx).unwrap().param_type.clone())
    }).collect::<Vec<(Symbol, Type)>>();

    let body = sym_table.function_scope(&params, |inner| {
        inner.expected_returns = func_type.return_type.clone();
        let body = typecheck(*func.body, inner);
        let actual_return_type = inner.returns.clone().unwrap_or(body.node_type.clone());

        // If the function has the special Unknown type, do no return value typechecking
        if !actual_return_type.satisfy(&inner.expected_returns).is_resolved() {
            panic!("Wrong return type for {}: annotated {:?} but found {:?}", func.id, inner.expected_returns, body.node_type)
        }

        body
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
    sym_table: &mut Box<SymTable<Symbol,Type>>,
) -> TypedASTNode {
    TypedASTNode { expr: Expr::Identifier { value: value.clone() }, node_type: sym_table.get(&Symbol::Identifier(value)) }
}

fn typecheck_assignable(
    dest: Box<ASTNode>, 
    sym_table: &mut Box<SymTable<Symbol,Type>>,
) -> TypedASTNode {
    match dest.expr {
        Expr::Identifier { value } => typecheck_identifier(value, sym_table),
        Expr::Unary { operand, operator: Op::Deref } => {
            if let Type::Function(deref_func) = sym_table.get(&mut Symbol::Operator(Op::Deref)) {
                let operand = Box::new(typecheck_assignable(operand, sym_table));
                let node_type = deref_func.typecheck_unnamed_args_call(&vec![&operand]);
                TypedASTNode {
                    expr: Expr::Unary { operand, operator: Op::Deref },
                    node_type,
                }
            } else {
                unreachable!()
            }
        },
        Expr::Member { .. } => typecheck(*dest, sym_table),
        _ => panic!("Left side of an assignment can only be an Identifier or a Deref unary expression")
    }
}

fn typecheck_assignment_expression(
    left: Box<ASTNode>, 
    right: Box<ASTNode>, 
    sym_table: &mut Box<SymTable<Symbol,Type>>
)   -> TypedASTNode {
    let left = typecheck_assignable(left, sym_table);
    let expected_type = left.node_type.clone();
    let right = Box::new( typecheck(*right, sym_table) );
    if expected_type != right.node_type {
        panic!("Variable type and assignment value types differ: {:?} != {:?}", expected_type, right.node_type)
    }

    TypedASTNode { 
        expr: Expr::Assignment { 
            left: Box::new(left),
            right 
        }, 
        node_type: expected_type
    }
}

fn typecheck_if_expression(
    condition: Box<ASTNode>, 
    then_branch: Box<ASTNode>, 
    else_branch: Option<Box<ASTNode>>, 
    sym_table: &mut Box<SymTable<Symbol,Type>>
) -> TypedASTNode {
    let condition = Box::new(typecheck(*condition, sym_table));
    if condition.node_type != Type::Bool {
        panic!("If expression condition must be a {:?}, got {:?}", Type::Bool, condition.node_type)
    }
    let then_branch = Box::new(typecheck(*then_branch, sym_table));
    let mut node_type = Type::Unit;

    let mut else_branch_opt: Option<Box<TypedASTNode>> = None;
    if let Some(else_branch_expr) = else_branch {
        let else_result = typecheck(*else_branch_expr, sym_table);
        if then_branch.node_type != else_result.node_type {
            panic!("Then and else branch return types differ: {:?} != {:?}", then_branch.node_type, else_result.node_type)
        }
        node_type = else_result.node_type.clone();
        else_branch_opt = Some(Box::new(else_result));
    }

    TypedASTNode {
        expr: Expr::If { condition, then_branch, else_branch: else_branch_opt },
        node_type,
    }
}

fn typecheck_while_expression(
    condition: Box<ASTNode>, 
    body: Box<ASTNode>, 
    sym_table: &mut Box<SymTable<Symbol,Type>>
) -> TypedASTNode {
    let condition = Box::new(typecheck(*condition, sym_table));
    if condition.node_type != Type::Bool {
        panic!("If expression condition must be a {:?}, got {:?}", Type::Bool, condition.node_type)
    }
    let body = Box::new(typecheck(*body, sym_table));
    // let node_type = body.node_type.clone();

    TypedASTNode {
        expr: Expr::While { condition, body },
        node_type: Type::Unit,
    }
}

fn typecheck_block_expression(
    statements: Vec<Box<ASTNode>>,
    result: Box<ASTNode>,
    sym_table: &mut Box<SymTable<Symbol,Type>>
) -> TypedASTNode {
    sym_table.block_scope(|inner_sym_table| {
        let mut typed_statements: Vec<Box<TypedASTNode>> = vec![];
        for expr in statements {
            typed_statements.push(Box::new(typecheck(*expr, inner_sym_table)));
        }
        let result = Box::new(typecheck(*result, inner_sym_table));
        let node_type = result.node_type.clone();
        TypedASTNode { expr: Expr::Block { statements: typed_statements, result }, node_type }
    })
}

/// Typecheck and get the type that a type annotation refers to.
/// The type annotation expression must have the type Type::Typeref(T),
/// then T is returned.
/// Unlike the typecheck functions for other AST nodes,
/// here we only return the type instead of a TypedASTNode,
/// since a type annotation does not produce any IR.
fn typecheck_type_annotation_referred_type(type_annotation: &Box<ASTNode>, sym_table: &mut Box<SymTable<Symbol,Type>>) -> Type {
    let typeref = match &type_annotation.expr {
        Expr::Identifier { value } => {
            let sym = Symbol::Identifier(value.clone());
            sym_table.get(&sym).clone()
        },
        Expr::Unary { operand, operator: Op::Deref } => {
            let inner_type = typecheck_type_annotation_referred_type(operand, sym_table);
            // Slightly ugly activity here: inner_type T is the type that the inner type annotation expr
            // refers to, so the type of this annotation is a Typeref to the type Pointer<T>
            Type::Typeref(Box::new(Type::Pointer(Box::new(inner_type))))
        },
        _ => panic!("Type annotation must be an identifier or a deref expression, found {:?}", type_annotation.expr),
    };

    match typeref {
        Type::Typeref(referred_type) => *referred_type,
        _ => panic!("Type annotation type should be Type::Typeref(T), found {:?}", typeref)
    }
}

fn typecheck_variable_declaration(
    id: Box<ASTNode>, 
    init: Box<ASTNode>,
    type_annotation: Option<Box<ASTNode>>,
    sym_table: &mut Box<SymTable<Symbol,Type>>
)   -> TypedASTNode {
    if let Expr::Identifier { value } = id.expr {
        let init = typecheck(*init, sym_table);
        if let Some(type_annotation) = type_annotation {
            let annotated_type = typecheck_type_annotation_referred_type(&type_annotation, sym_table);
            if init.node_type != annotated_type {
                panic!("Type annotation and init expression types differ: {:?} != {:?}", annotated_type, init.node_type)
            }
        }
        sym_table.symbols.insert(Symbol::Identifier(value.clone()), init.node_type.clone());
        TypedASTNode { 
            expr: Expr::VariableDeclaration { 
                id: Box::new(TypedASTNode { expr: Expr::Identifier { value }, node_type: init.node_type.clone() }), 
                init: Box::new(init),
                type_annotation: None,
            }, 
            node_type: Type::Unit
        }
    } else {
        panic!("Id of a variable declaration must be an identifier");
    }
}

fn typecheck_logical_op(
    left_expr: Box<ASTNode>, 
    right_expr: Box<ASTNode>, 
    operator: Op, 
    sym_table: &mut Box<SymTable<Symbol,Type>>
) -> TypedASTNode {
    if let Type::Function(op_function) = sym_table.get(&mut Symbol::Operator(operator)) {
        let left  = Box::new(typecheck(*left_expr, sym_table));
        let right = Box::new(typecheck(*right_expr, sym_table));
        let node_type = op_function.typecheck_unnamed_args_call(&vec![&left, &right]);
        TypedASTNode { expr: Expr::Logical { left, operator, right }, node_type }
    } else {
        panic!("Undefined operator {:?}", operator)
    }
}


fn typecheck_binary_op(
    left_expr: Box<ASTNode>, 
    right_expr: Box<ASTNode>, 
    operator: Op, 
    sym_table: &mut Box<SymTable<Symbol,Type>>
) -> TypedASTNode {
    if let Type::Function(op_function) = sym_table.get(&mut Symbol::Operator(operator)) {
        let left  = Box::new(typecheck(*left_expr, sym_table));
        let right = Box::new(typecheck(*right_expr, sym_table));
        let node_type = op_function.typecheck_unnamed_args_call(&vec![&left, &right]);
        TypedASTNode { expr: Expr::Binary { left, operator, right }, node_type }
    } else {
        panic!("Undefined operator {:?}", operator)
    }
}

fn typecheck_unary_op(
    operand: Box<ASTNode>,
    operator: Op,
    sym_table: &mut Box<SymTable<Symbol,Type>>
) -> TypedASTNode {
    if let Type::Function(op_function) = sym_table.get(&mut Symbol::Operator(operator)) {
        let operand = Box::new(typecheck(*operand, sym_table));
        let node_type = op_function.typecheck_unnamed_args_call(&vec![&operand]);
        TypedASTNode { expr: Expr::Unary { operand, operator }, node_type }
    } else {
        panic!("Undefined operator {:?}", operator)
    }
}

fn typecheck_call_expression(
    callee: Box<ASTNode>,
    argument_expr: Vec<Box<ASTNode>>,
    sym_table: &mut Box<SymTable<Symbol,Type>>
) -> TypedASTNode {
    let callee = typecheck(*callee, sym_table);
    let call_type = callee.node_type.get_callable_type();

    let mut typed_argument_expr: Vec<Box<TypedASTNode>> = vec![];
    for expr in argument_expr {
        let arg = Box::new(typecheck(*expr, sym_table));
        typed_argument_expr.push(arg);
    }
    let node_type = call_type.typecheck_unnamed_args_call(&typed_argument_expr.iter().collect());
    TypedASTNode {
        expr: Expr::Call { 
            callee: Box::new(callee),
            arguments: typed_argument_expr, 
        },
        node_type,
    }
}

fn typecheck_struct_instance(
    id: String,
    fields: Vec<(String, Box<ASTNode>)>,
    sym_table: &mut Box<SymTable<Symbol,Type>>
) -> TypedASTNode {
    let struct_type = sym_table.get(&Symbol::Identifier(id.clone()));
    let constructor_type = struct_type.get_callable_type();

    let mut typed_named_argument_expr: Vec<(String, Box<TypedASTNode>)> = vec![];

    for (name, expr) in fields {
        let arg = typecheck(*expr, sym_table);
        typed_named_argument_expr.push((name.clone(), Box::new(arg.clone())));
    }

    let node_type = constructor_type.typecheck_named_args_call(&typed_named_argument_expr);
    TypedASTNode {
        expr: Expr::StructInstance { struct_name: id, fields: typed_named_argument_expr },
        node_type,
    }
    
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::{tokenize, Token};
    use crate::parser::parse;
    use super::*;

    fn t(src: &str) -> Module<TypedUserDefinedFunction, TypedStruct> {
        let tokens: Vec<Token> = tokenize(src).expect("Tokenizing to succeed");
        let module = parse(tokens).expect("Parsing to succeed");
        typecheck_program(module)
    }

    #[test]
    fn typecheck_integers() {
        let res = t("7 + 3 * 2;");
        assert_eq!(Type::Int, res.last().node_type);
    }

    #[test]
    fn typecheck_block() {
        let res = t("{ var x = 789 }");
        let res = &res.main().body;
        // ASTNode { expr: BlockExpression { statements: [], result: ASTNode { expr: VariableDeclaration { id: ASTNode { expr: Identifier { value: "x" } }, init: ASTNode { expr: IntegerLiteral { value: 789 } } } } } }
        assert_eq!(Type::Unit, res.node_type);

        if let Expr::Block { result,.. } = &res.expr {
            if let Expr::Block { result,.. } = &result.expr {
                if let Expr::VariableDeclaration { id, init,.. } = &result.expr {
                    assert_eq!(id.node_type, Type::Int);
                    assert_eq!(init.node_type, Type::Int);
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
    #[should_panic(expected="Invalid argument type at index 0, expected Int but got Bool")]
    fn simple_type_error() {
        t("false + 3 * 2");
    }

    #[test]
    #[should_panic(expected="Then and else branch return types differ: Unit != Bool")]
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
    fn if_without_else_is_unit() {
        let module = t("
            if (1 > 2) then {
                1
            };
        ");
        assert_eq!(module.last().node_type, Type::Unit)
    }

    #[test]
    fn if_else_type() {
        let module = t("
            if (1 > 2) then {
                1
            } else {
                2
            };
        ");
        assert_eq!(module.last().node_type, Type::Int)
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
        let node = &node.last();

        assert_eq!(node.node_type, Type::Bool);
        if let Expr::If { condition, .. } = &node.expr {
            assert_eq!(condition.node_type, Type::Bool);
            if let Expr::Binary { left, right, .. } = &condition.expr {
                assert_eq!(left.node_type, Type::Int);
                assert_eq!(right.node_type, Type::Int);
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
        let node = &node.main().body;

        assert_eq!(node.node_type, Type::Unit);
    }

    #[test]
    fn unary_sub() {
        let node = t("
            7 * -7
        ");
        let node = &node.main().body;

        assert_eq!(node.node_type, Type::Int);
    }

    #[test]
    fn compare_eq_booleans() {
        let node = t("false == true");
        let node = &node.main().body;
        assert_eq!(node.node_type, Type::Bool);
    }

    #[test]
    #[should_panic(expected="Invalid argument type at index 1, expected Int but got Bool")]
    fn compare_eq_invalid_types() {
        t("1 == true");
    }

    #[test]
    fn compare_eq_integer() {
        let node = t("123 == 123");
        let node = &node.main().body;
        assert_eq!(node.node_type, Type::Bool);
    }

    #[test]
    fn type_annotation() {
        let node = t("var x: Int = 123");
        let node = &node.main().body;
        assert_eq!(node.node_type, Type::Unit);
    }

    #[test]
    #[should_panic(expected="Type annotation and init expression types differ: Int != Bool")]
    fn type_annotation_int_error() {
        t("var x: Int = false");
    }

    #[test]
    fn typed_variable_assignment() {
        let node = t("var x: Int = 123; x = 456");
        assert_eq!(node.main().body.node_type, Type::Int);
    }

    #[test]
    #[should_panic(expected="Variable type and assignment value types differ: Int != Bool")]
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
        assert_eq!(node.main().body.node_type, Type::Int);
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
        assert_eq!(node.main().body.node_type, Type::Int);
    }

    #[test]
    fn global_function_references() {
        let node = &t("
        f1();

        fun f1() {
            f2();
        }
        
        fun f2() {
            f1();
        }
        ");
        assert_eq!(node.main().body.node_type, Type::Unit);
    }

    #[test]
    fn expression_with_return_is_unit() {
        let res = t("
        var x = {
            return 1
        };
        x
        ");
        assert_eq!(res.main().body.node_type, Type::Unit)
    }

    #[test]
    fn return_expression_type() {
        let module = &t("
            return 1;
        ");
        if let Expr::Block { result,.. } = &module.main().body.expr {
            assert_eq!(result.node_type, Type::Unit);
        }
    }

    #[test]
    fn return_type_ok() {
        t("
            fun f(): Int {
                return 1;
            }
        ");
    }

    #[test]
    fn address_of_operator_is_generic() {
        let module = &t("
            var pointer = &1;
            pointer
        ");
        if let Expr::Block { result,.. } = &module.main().body.expr {
            if let Type::Pointer(ptype) = &result.node_type {
                assert_eq!(**ptype, Type::Int);
            }
        }

        let module = &t("
            var pointer = &false;
            pointer
        ");
        if let Expr::Block { result,.. } = &module.main().body.expr {
            if let Type::Pointer(ptype) = &result.node_type {
                assert_eq!(**ptype, Type::Bool);
            }
        }
    }

    #[test]
    fn nested_pointer_deref() {
        let module = &t("
            var pointer: Int*** = &&&1;
            ***pointer
        ");
        if let Expr::Block { result,.. } = &module.main().body.expr {
            assert!(matches!(result.node_type, Type::Int))
        }
    }

    #[test]
    fn assign_to_pointer_deref() {
        t("
            var x = 1;
            var y = &x;
            *y = 2;
        ");
    }

    #[test]
    #[should_panic(expected = "Type annotation and init expression types differ: Pointer<Pointer<Pointer<Int>>> != Pointer<Pointer<Pointer<Pointer<Int>>>>")]
    fn nested_pointer_typecheck_fail() {
        t("
            var pointer: Int*** = &&&&1;
        ");
    }

    #[test]
    fn constructor_type() {
        let m = t("Int(123)");
        if let Expr::Block { result,.. } = &m.main().body.expr {
            assert!(matches!(result.node_type, Type::Constructor(_)))
        } else {
            panic!("Wrong!")
        }
    }

    #[test]
    fn new_operator() {
        let m = t("new Int(123)");
        if let Expr::Block { result,.. } = &m.main().body.expr {
            assert!(matches!(result.node_type, Type::Pointer(_)))
        } else {
            panic!("Wrong!")
        }
    }

    #[test]
    fn delete_operator() {
        let m = t("
            var x = new Int(123);
            delete x
        ");
        if let Expr::Block { result,.. } = &m.main().body.expr {
            assert!(matches!(result.node_type, Type::Unit))
        } else {
            panic!("Wrong!")
        }
    }

    #[test]
    #[should_panic(expected = "Invalid argument type at index 0, expected Constructor<T> but got Int")]
    fn new_only_accepts_constructor() {
        t("new 1");
    }

    #[test]
    fn logical_expr() {
        let m = t("
            true and false
        ");

        if let Expr::Block { result,.. } = &m.main().body.expr {
            assert!(matches!(result.expr, Expr::Logical {.. }));
        } else {
            panic!("Wrong!")
        }
    }

    #[test]
    fn struct_instance() {
        let _n = t("
            struct Point {
                x: Int,
                y: Int
            }
        ");

        let s = _n.structs.first().unwrap();
        assert_eq!(s.id, "Point");
        assert_eq!(s.fields.len(), 2);
        assert_eq!(s.fields.first().unwrap().name, "x");
        assert_eq!(s.fields.first().unwrap().param_type, Type::Int);
    }

    #[test]
    fn struct_instance_type() {
        let m = t("
            struct Point {
                x: Int,
                y: Int
            }
            Point { x: 1, y: 2 }
        ");
        if let Expr::Block { result,.. } = &m.main().body.expr {
            assert!(matches!(result.node_type, Type::Constructor(_)))
        } else {
            panic!("Wrong!")
        }
    }

    #[test]
    fn struct_param_type() {
        let m = t("
            struct Point {
                x: Int,
                y: Int
            }
            fun f(p: Point*): Point* {
                return p
            }
            f(new Point { x: 1, y: 2 })
        ");
        if let Expr::Block { .. } = &m.main().body.expr {
            // assert!(matches!(result.node_type, Type::Constructor(_)))
        } else {
            panic!("Wrong!")
        }
    }

    #[test]
    fn member_access() {
        let m = t("
            struct Vector { x: Int, y: Int, z: Int }
            struct Particle {
                pos: Vector*,
                vel: Vector*
            }
            var p = new Particle { 
                pos: new Vector { x: 0, y: 0, z: 0 },
                vel: new Vector { x: 0, y: -1, z: 0 }
            }
            p.pos.z
        ");
        if let Expr::Block { result,.. } = &m.main().body.expr {
            assert!(matches!(result.node_type, Type::Int))
        } else {
            panic!("Wrong!")
        }
    }

    #[test]
    fn break_continue() {
        let _m = t("
            while true do {
                break;
                continue;
                if true then {
                    break;
                    continue;
                }
            }
        ");
    }
}
