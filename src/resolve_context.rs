use ecow::EcoString;

use crate::hir::Identifier;
use crate::scope::{Scope, VarId};

pub struct ResolveContext {
    pub scope_stack: Vec<Scope>,
    pub variables: Vec<VariableInfo>,
}

impl ResolveContext {
    pub fn new() -> Self {
        ResolveContext {
            scope_stack: vec![Scope::new()],
            variables: Vec::new(),
        }
    }
    pub fn define(&mut self, ident: &Identifier) -> VarId {
        let scope = self.scope_stack.last_mut().unwrap();
        let id = VarId(self.variables.len());
        self.variables.push(VariableInfo {
            name: ident.name.clone(),
        });
        scope.define(ident, id);
        id
    }
    pub fn resolve(&mut self, ident: &Identifier) -> Option<VarId> {
        self.scope_stack.iter().rev().find_map(|scope| scope.resolve(ident))
    }
    pub fn push_scope(&mut self) {
        self.scope_stack.push(Scope::new());
    }
    pub fn pop_scope(&mut self) {
        self.scope_stack.pop();
    }
    pub fn get_var(&self, var_id: VarId) -> &VariableInfo {
        &self.variables[var_id.0]
    }
}

pub struct VariableInfo {
    pub name: EcoString,
}
