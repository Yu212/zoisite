use std::collections::HashMap;

use ecow::EcoString;

use crate::hir::Identifier;

pub struct Scope {
    variables: HashMap<EcoString, VarId>,
    functions: HashMap<EcoString, FnId>,
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }
    pub fn define_var(&mut self, ident: &Identifier, var_id: VarId) {
        self.variables.insert(ident.name.clone(), var_id);
    }
    pub fn define_fn(&mut self, name: EcoString, fn_id: FnId) {
        self.functions.insert(name, fn_id);
    }
    pub fn resolve_var(&self, ident: &Identifier) -> Option<VarId> {
        self.variables.get(&ident.name).copied()
    }
    pub fn resolve_fn(&self, ident: &Identifier) -> Option<FnId> {
        self.functions.get(&ident.name).copied()
    }
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct VarId(pub usize);

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct FnId(pub usize);
