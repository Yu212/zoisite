use ecow::EcoString;

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
    pub fn define_var(&mut self, name: EcoString) -> VarId {
        let scope = self.scope_stack.last_mut().unwrap();
        let id = VarId(self.variables.len());
        self.variables.push(VariableInfo {
            name: name.clone(),
            id,
        });
        scope.define_var(name, id);
        id
    }
    pub fn define_fn(&mut self, name: EcoString, num_args: usize) -> FnId {
        let scope = self.scope_stack.last_mut().unwrap();
        let id = FnId(self.functions.len());
        self.functions.push(FunctionInfo {
            name: name.clone(),
            id,
            num_args,
        });
        scope.define_fn(name, num_args, id);
        id
    }
    pub fn resolve_var(&mut self, name: &EcoString) -> Option<&VariableInfo> {
        let var_id = self.scope_stack.iter().rev().find_map(|scope| scope.resolve_var(name));
        var_id.map(|var_id| self.get_var(var_id))
    }
    pub fn resolve_fn(&mut self, name: &EcoString, num_args: usize) -> Option<&FunctionInfo> {
        let fn_id = self.scope_stack.iter().rev().find_map(|scope| scope.resolve_fn(name, num_args));
        fn_id.map(|fn_id| self.get_fn(fn_id))
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
    pub id: VarId,
}

pub struct FunctionInfo {
    pub name: EcoString,
    pub id: FnId,
    pub num_args: usize,
}
