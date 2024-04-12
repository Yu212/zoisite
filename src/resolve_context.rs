use ecow::EcoString;

use crate::hir::Identifier;
use crate::scope::{FnId, Scope, VarId};

pub struct ResolveContext {
    pub scope_stack: Vec<Scope>,
    pub variables: Vec<VariableInfo>,
    pub functions: Vec<FunctionInfo>,
}

impl ResolveContext {
    pub fn new() -> Self {
        ResolveContext {
            scope_stack: vec![Scope::new()],
            variables: Vec::new(),
            functions: Vec::new(),
        }
    }
    pub fn define_var(&mut self, ident: &Identifier) -> VarId {
        let scope = self.scope_stack.last_mut().unwrap();
        let id = VarId(self.variables.len());
        self.variables.push(VariableInfo {
            name: ident.name.clone(),
        });
        scope.define_var(ident, id);
        id
    }
    pub fn define_fn(&mut self, name: EcoString) -> FnId {
        let scope = self.scope_stack.last_mut().unwrap();
        let id = FnId(self.functions.len());
        self.functions.push(FunctionInfo {
            name: name.clone(),
        });
        scope.define_fn(name, id);
        id
    }
    pub fn resolve_var(&mut self, ident: &Identifier) -> Option<VarId> {
        self.scope_stack.iter().rev().find_map(|scope| scope.resolve_var(ident))
    }
    pub fn resolve_fn(&mut self, ident: &Identifier) -> Option<FnId> {
        self.scope_stack.iter().rev().find_map(|scope| scope.resolve_fn(ident))
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
    pub fn get_fn(&self, fn_id: FnId) -> &FunctionInfo {
        &self.functions[fn_id.0]
    }
}

pub struct VariableInfo {
    pub name: EcoString,
}

pub struct FunctionInfo {
    pub name: EcoString,
}
