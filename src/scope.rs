use std::collections::HashMap;

use ecow::EcoString;
use crate::resolve_context::Place;

pub struct Scope {
    variables: HashMap<EcoString, VarId>,
    functions: HashMap<(EcoString, usize), FnId>,
    pub place: Place,
}

impl Scope {
    pub fn new(place: Place) -> Self {
        Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
            place,
        }
    }
    pub fn define_var(&mut self, name: EcoString, var_id: VarId) {
        self.variables.insert(name, var_id);
    }
    pub fn define_fn(&mut self, name: EcoString, num_args: usize, fn_id: FnId) {
        self.functions.insert((name, num_args), fn_id);
    }
    pub fn resolve_var(&self, name: &EcoString) -> Option<VarId> {
        self.variables.get(name).copied()
    }
    pub fn resolve_fn(&self, name: &EcoString, num_args: usize) -> Option<FnId> {
        self.functions.get(&(name.clone(), num_args)).copied()
    }
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct VarId(pub usize);

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct FnId(pub usize);
