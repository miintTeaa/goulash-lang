use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

use crate::ast::Ident;

#[derive(Debug)]
pub struct Scope<T> {
    variables: HashMap<Ident, T>,
}

impl<T> Scope<T> {
    pub fn new() -> Self {
        Self {
            variables: HashMap::default(),
        }
    }

    pub fn declare(&mut self, s: Ident, val: T) {
        self.variables.insert(s, val);
    }

    pub fn delete(&mut self, ident: &Ident) -> Option<(Ident, T)> {
        self.variables.remove_entry(&ident)
    }

    pub fn get(&self, ident: &Ident) -> Option<&T> {
        self.variables.get(&ident)
    }

    pub fn get_mut(&mut self, ident: &Ident) -> Option<&mut T> {
        self.variables.get_mut(&ident)
    }
}

#[derive(Debug)]
pub struct ScopeStack<T> {
    scopes: Vec<Scope<T>>,
}

impl<T> ScopeStack<T> {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new()],
        }
    }

    pub fn declare(&mut self, s: Ident, val: T) {
        self.scopes
            .last_mut()
            .expect("should have scope here")
            .declare(s, val)
    }

    pub fn find(&self, ident: &Ident) -> Option<&T> {
        for scope in self.scopes.iter().rev() {
            match scope.get(ident) {
                Some(v) => return Some(v),
                None => (),
            }
        }
        None
    }

    pub fn find_mut(&mut self, ident: &Ident) -> Option<&mut T> {
        for scope in self.scopes.iter_mut().rev() {
            match scope.get_mut(ident) {
                Some(v) => return Some(v),
                None => (),
            }
        }
        None
    }

    pub fn find_with_scope(&self, ident: &Ident) -> Option<(&T, usize)> {
        for (scope_id, scope) in self.scopes.iter().enumerate().rev() {
            match scope.get(ident) {
                Some(v) => return Some((v, scope_id)),
                None => (),
            }
        }
        None
    }

    pub fn find_with_scope_mut(&mut self, ident: &Ident) -> Option<(&mut T, usize)> {
        for (scope_id, scope) in self.scopes.iter_mut().enumerate().rev() {
            match scope.get_mut(ident) {
                Some(v) => return Some((v, scope_id)),
                None => (),
            }
        }
        None
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }
}

impl<T> Index<usize> for ScopeStack<T> {
    type Output = Scope<T>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.scopes[index]
    }
}

impl<T> IndexMut<usize> for ScopeStack<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.scopes[index]
    }
}
