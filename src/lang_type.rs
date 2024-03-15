use core::fmt;
use std::collections::HashMap;

use crate::type_checker::TypedStruct;


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
            Type::Function(_) => 0, // What? We never actually come to allocate functions, but they still can appear as an IR variable when creating function pointers.
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
    pub satisfied: bool,
    pub constraint: Option<(String, Type)>,
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
            Type::Pointer(pointer_type) => {
                // A function pointer can be called.
                if let Type::Function(_) = pointer_type.as_ref() {
                    return pointer_type.get_callable_type()
                }
                panic!("Pointer to {:?} is not callable", pointer_type)
            },
            _ => panic!("{:?} is not callable", self)
        }
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