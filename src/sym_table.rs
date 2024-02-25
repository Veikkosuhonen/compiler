use std::{collections::HashMap, mem};

use crate::tokenizer::Op;

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum Symbol {
    Identifier(String),
    Operator(Op)
}

impl Symbol {
    pub fn to_string(&self) -> String {
        match &self {
            Symbol::Identifier(name) => name.clone(),
            Symbol::Operator(op) => op.to_string(),
        }
    }
}

#[derive(Default)]
pub struct SymTable<T> {
    pub symbols: HashMap<Symbol, T>,
    pub parent: Option<Box<SymTable<T>>>,
    pub returns: Option<T>,
    pub expected_returns: T,
}

impl<T:Clone + Default> SymTable<T> {
    pub fn new<'a>(parent: Option<Box<SymTable<T>>>) -> Box<SymTable<T>> {
        Box::new(SymTable {
            symbols: HashMap::new(),
            parent,
            returns: None,
            expected_returns: T::default(),
        })
    }

    pub fn get(&self, k: &Symbol) -> T {
        if let Some(val) = self.symbols.get(k) {
            val.clone()
        } else if let Some(parent) = &self.parent {
            parent.get(k)
        } else {
            panic!("Accessing undefined symbol {:?}", k)
        }
    }

    pub fn assign(&mut self, k: Symbol, val: T) -> T {
        if self.symbols.contains_key(&k) {
            self.symbols.insert(k, val.clone());
            val
        } else if let Some(parent) = &mut self.parent {
            parent.assign(k, val)
        } else {
            panic!("Assigning to undefined symbol '{:?}'", k);
        }
    }

    pub fn block_scope<Y>(self: &mut Box<SymTable<T>>, f: impl FnOnce(&mut Box<SymTable<T>>) -> Y) -> Y {
        let expected_returns = self.expected_returns.clone();
        let symtab = mem::replace(self, Default::default());
        let mut inner_symtab = SymTable::new(Some(symtab));
        inner_symtab.expected_returns = expected_returns;
        let result = f(&mut inner_symtab);
        *self = inner_symtab.parent.unwrap();
        if inner_symtab.returns.is_some() {
            self.returns = inner_symtab.returns;
        }
        result
    }

    pub fn function_scope<Y>(self: &mut Box<SymTable<T>>, args: &Vec<(Symbol, T)>, f: impl FnOnce(&mut Box<SymTable<T>>) -> Y) -> Y {
        let symtab = mem::replace(self, Default::default());
        let mut inner_symtab = SymTable::new(Some(symtab));
        for (arg_symbol, arg_val) in args {
            inner_symtab.symbols.insert(arg_symbol.clone(), arg_val.clone());
        }
        let result = f(&mut inner_symtab);
        *self = inner_symtab.parent.unwrap();
        result
    }
}