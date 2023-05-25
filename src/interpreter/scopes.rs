use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

#[derive(Debug)]
pub struct Scope<T> {
    variables: HashMap<String, T>,
}

impl<T> Scope<T> {
    pub fn new() -> Self {
        Self {
            variables: HashMap::default(),
        }
    }

    pub fn declare(&mut self, s: String, val: T) {
        self.variables.insert(s, val);
    }

    pub fn delete(&mut self, ident: &str) {
        self.variables.remove_entry(ident);
    }

    pub fn get(&self, ident: &str) -> Option<&T> {
        self.variables.get(ident)
    }

    pub fn get_mut(&mut self, ident: &str) -> Option<&mut T> {
        self.variables.get_mut(ident)
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

    pub fn declare(&mut self, s: String, val: T) {
        self.scopes
            .last_mut()
            .expect("should have scope here")
            .declare(s, val)
    }

    pub fn find(&self, ident: &str) -> Option<&T> {
        for scope in self.scopes.iter().rev() {
            match scope.get(ident) {
                Some(v) => return Some(v),
                None => (),
            }
        }
        None
    }

    #[allow(unused)]
    pub fn find_mut(&mut self, ident: &str) -> Option<&mut T> {
        for scope in self.scopes.iter_mut().rev() {
            match scope.get_mut(ident) {
                Some(v) => return Some(v),
                None => (),
            }
        }
        None
    }

    #[allow(unused)]
    pub fn find_with_scope(&self, ident: &str) -> Option<(&T, usize)> {
        for (scope_id, scope) in self.scopes.iter().enumerate().rev() {
            match scope.get(ident) {
                Some(v) => return Some((v, scope_id)),
                None => (),
            }
        }
        None
    }

    pub fn find_with_scope_mut(&mut self, ident: &str) -> Option<(&mut T, usize)> {
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
