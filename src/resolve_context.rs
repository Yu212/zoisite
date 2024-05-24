use std::iter;

use ecow::EcoString;

use crate::scope::{FnId, Scope, VarId};

pub struct ResolveContext {
    pub global_scope: Scope,
    pub scope_stack: Vec<Scope>,
    pub variables: Vec<VariableInfo>,
    pub functions: Vec<FunctionInfo>,
    pub places: Vec<Place>,
}

impl ResolveContext {
    pub fn new() -> Self {
        let global_place = Place(0);
        let root_place = Place(1);
        ResolveContext {
            global_scope: Scope::new(global_place),
            scope_stack: vec![Scope::new(root_place)],
            variables: Vec::new(),
            functions: Vec::new(),
            places: vec![global_place, root_place],
        }
    }
    pub fn define_builtins(&mut self) {
        self.define_global_fn(EcoString::from("print"), 1);
        self.define_global_fn(EcoString::from("input"), 0);
    }
    pub fn define_var(&mut self, name: EcoString) -> VarId {
        let scope = self.scope_stack.last_mut().unwrap();
        let id = VarId(self.variables.len());
        self.variables.push(VariableInfo {
            name: name.clone(),
            id,
            place: scope.place,
        });
        scope.define_var(name, id);
        id
    }
    pub fn define_fn(&mut self, name: EcoString, num_args: usize) -> FnId {
        let scope = self.scope_stack.last_mut().unwrap();
        let place = scope.place;
        let id = FnId(self.functions.len());
        self.functions.push(FunctionInfo {
            name: name.clone(),
            id,
            num_args,
            place,
        });
        scope.define_fn(name, num_args, id);
        id
    }
    pub fn define_global_fn(&mut self, name: EcoString, num_args: usize) -> FnId {
        let place = self.global_scope.place;
        let id = FnId(self.functions.len());
        self.functions.push(FunctionInfo {
            name: name.clone(),
            id,
            num_args,
            place,
        });
        self.global_scope.define_fn(name, num_args, id);
        id
    }
    pub fn resolve_var(&mut self, name: &EcoString) -> Option<VarId> {
        let place = self.scope_stack.last_mut().unwrap().place;
        self.scope_stack.iter().rev()
            .filter(|scope| place == scope.place)
            .chain(iter::once(&self.global_scope))
            .find_map(|scope| scope.resolve_var(name))
    }
    pub fn resolve_fn(&mut self, name: &EcoString, num_args: usize) -> Option<FnId> {
        let place = self.scope_stack.last_mut().unwrap().place;
        self.scope_stack.iter().rev()
            .filter(|scope| place == scope.place)
            .chain(iter::once(&self.global_scope))
            .find_map(|scope| scope.resolve_fn(name, num_args))
    }
    pub fn push_scope(&mut self, update_place: bool) {
        let scope = self.scope_stack.last_mut().unwrap();
        let place = if update_place {
            let new_place = Place(self.places.len());
            self.places.push(new_place);
            new_place
        } else { scope.place };
        self.scope_stack.push(Scope::new(place));
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
    pub place: Place,
}

pub struct FunctionInfo {
    pub name: EcoString,
    pub id: FnId,
    pub num_args: usize,
    pub place: Place,
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct Place(usize);
