use std::collections::HashMap;

#[derive(Debug)]
struct Scope<T> {
    variables: HashMap<String, T>,
}

impl<T> Scope<T> {
    pub fn new() -> Self {
        Self {
            variables: HashMap::default(),
        }
    }

    pub fn decl(&mut self, s: String, val: T) {
        self.variables.insert(s, val);
    }

    pub fn del(&mut self, ident: &str) {
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

    pub fn decl(&mut self, s: String, val: T) -> bool {
        match self.get(&s) {
            Some(_) => false,
            None => {
                self.scopes
                    .last_mut()
                    .expect("should have scope here")
                    .decl(s, val);
                true
            }
        }
    }

    pub fn del(&mut self, ident: &str) {
        match self.get(&ident) {
            Some(_) => self
                .scopes
                .last_mut()
                .expect("should have scope here")
                .del(ident),
            None => (),
        }
    }

    pub fn get(&self, s: &str) -> Option<&T> {
        for scope in self.scopes.iter().rev() {
            match scope.get(s) {
                Some(v) => return Some(v),
                None => (),
            }
        }
        None
    }

    pub fn get_with_scope(&self, s: &str) -> Option<(&T, usize)> {
        for (scope_id, scope) in self.scopes.iter().enumerate().rev() {
            match scope.get(s) {
                Some(v) => return Some((v, scope_id)),
                None => (),
            }
        }
        None
    }

    pub fn decl_in_scope(&mut self, s: String, val: T, id: usize) {
        self.scopes[id].decl(s, val)
    }

    pub fn del_from_scope(&mut self, s: &str, id: usize) {
        self.scopes[id].del(s)
    }

    pub fn set_in_scope(&mut self, s: &str, val: T, id: usize) -> bool {
        match self.scopes[id].get_mut(s) {
            Some(v) => {
                *v = val;
                true
            }
            None => false,
        }
    }

    pub fn set(&mut self, s: &str, val: T) -> bool {
        for scope in self.scopes.iter_mut().rev() {
            match scope.get_mut(s) {
                Some(v) => {
                    *v = val;
                    return true;
                }
                None => (),
            }
        }
        false
    }
}
