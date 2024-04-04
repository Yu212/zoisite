use std::collections::HashMap;

use ecow::EcoString;

use crate::hir::Identifier;

pub struct Scope {
    variables: HashMap<EcoString, VarId>,
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            variables: HashMap::new(),
        }
    }
    pub fn define(&mut self, ident: &Identifier, var_id: VarId) {
        self.variables.insert(ident.name.clone(), var_id);
    }
    pub fn resolve(&self, ident: &Identifier) -> Option<VarId> {
        self.variables.get(&ident.name).copied()
    }
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct VarId(pub usize);
