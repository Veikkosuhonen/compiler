use std::{collections::HashMap, fmt::Debug, hash::Hash, mem};

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

impl Default for Symbol {
    fn default() -> Self {
        Symbol::Identifier(String::from(""))
    }
}

#[derive(Default)]
pub struct SymTable<S,T> {
    pub symbols: HashMap<S, T>,
    pub parent: Option<Box<SymTable<S,T>>>,
    pub returns: Option<T>,
    pub expected_returns: T,
}

impl<
    S:PartialEq + Hash + Eq + Debug + Default + Clone, 
    T:Clone + Default
> SymTable<S,T> {
    pub fn new<'a>(parent: Option<Box<SymTable<S,T>>>) -> Box<SymTable<S,T>> {
        Box::new(SymTable {
            symbols: HashMap::new(),
            parent,
            returns: None,
            expected_returns: T::default(),
        })
    }

    pub fn get(&self, k: &S) -> T {
        if let Some(val) = self.symbols.get(k) {
            val.clone()
        } else if let Some(parent) = &self.parent {
            parent.get(k)
        } else {
            panic!("Accessing undefined symbol {:?}", k)
        }
    }

    pub fn get_ref(&self, k: &S) -> &T {
        if let Some(val) = self.symbols.get(k) {
            val
        } else if let Some(parent) = &self.parent {
            parent.get_ref(k)
        } else {
            panic!("Accessing undefined symbol {:?}", k)
        }
    }

    pub fn declare(&mut self, k: S, val: T) -> T {
        self.symbols.insert(k, val.clone());
        val
    }

    pub fn assign(&mut self, k: S, val: T) -> T {
        if self.symbols.contains_key(&k) {
            self.symbols.insert(k, val.clone());
            val
        } else if let Some(parent) = &mut self.parent {
            parent.assign(k, val)
        } else {
            panic!("Assigning to undefined symbol '{:?}'", k);
        }
    }

    pub fn block_scope<Y>(self: &mut Box<SymTable<S,T>>, f: impl FnOnce(&mut Box<SymTable<S,T>>) -> Y) -> Y {
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

    pub fn function_scope<Y>(self: &mut Box<SymTable<S,T>>, args: &Vec<(S, T)>, f: impl FnOnce(&mut Box<SymTable<S,T>>) -> Y) -> Y {
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