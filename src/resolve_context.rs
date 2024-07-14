use std::iter;

use ecow::EcoString;

use crate::r#type::Type;
use crate::scope::{FnId, Scope, VarId};

pub struct ResolveContext {
    pub global_scope: Scope,
    pub scope_stack: Vec<Scope>,
    pub variables: Vec<VariableInfo>,
    pub functions: Vec<FuncInfo>,
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
        self.define_global_fn(EcoString::from("print"), vec![EcoString::from("n")], vec![Type::Int], Type::Unit);
        self.define_global_fn(EcoString::from("input"), vec![], vec![], Type::Unit);
    }
    pub fn define_var(&mut self, name: EcoString, ty: Type) -> VarId {
        let scope = self.scope_stack.last_mut().unwrap();
        let id = VarId(self.variables.len());
        self.variables.push(VariableInfo {
            name: name.clone(),
            id,
            place: scope.place,
            ty,
        });
        scope.define_var(name, id);
        id
    }
    pub fn define_fn(&mut self, name: EcoString, params: Vec<VarId>, return_ty: Type) -> FuncInfo {
        let idx = self.scope_stack.len() - 2;
        let scope = self.scope_stack.get_mut(idx).unwrap();
        let place = scope.place;
        let id = FnId(self.functions.len());
        scope.define_fn(name.clone(), params.len(), id);
        let params_ty = params.iter().map(|&var_id| self.get_var(var_id).ty).collect();
        let fun_info = FuncInfo {
            name,
            id,
            params,
            place,
            params_ty,
            return_ty,
        };
        self.functions.push(fun_info.clone());
        fun_info
    }
    pub fn define_global_fn(&mut self, name: EcoString, params: Vec<EcoString>, params_ty: Vec<Type>, return_ty: Type) -> FuncInfo {
        self.push_scope(true);
        let params: Vec<_> = params.iter().zip(params_ty.clone())
            .map(|(name, ty)| self.define_var(name.clone(), ty))
            .collect();
        self.pop_scope();
        let place = self.global_scope.place;
        let id = FnId(self.functions.len());
        self.global_scope.define_fn(name.clone(), params.len(), id);
        let fun_info = FuncInfo {
            name,
            id,
            params,
            place,
            params_ty,
            return_ty,
        };
        self.functions.push(fun_info.clone());
        fun_info
    }
    pub fn resolve_ty(&mut self, name: &EcoString) -> Type {
        match name.as_str() {
            "int" => Type::Int,
            "bool" => Type::Bool,
            "unit" => Type::Unit,
            _ => Type::Invalid,
        }
    }
    pub fn resolve_var(&mut self, name: &EcoString) -> Option<VarId> {
        let place = self.scope_stack.last_mut().unwrap().place;
        self.scope_stack.iter().rev()
            .filter(|scope| place == scope.place)
            .chain(iter::once(&self.global_scope))
            .find_map(|scope| scope.resolve_var(name))
    }
    pub fn resolve_fn(&mut self, name: &EcoString, num_params: usize) -> Option<FnId> {
        self.scope_stack.iter().rev()
            .chain(iter::once(&self.global_scope))
            .find_map(|scope| scope.resolve_fn(name, num_params))
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
    pub fn get_fn(&self, fn_id: FnId) -> &FuncInfo {
        &self.functions[fn_id.0]
    }
}

pub struct VariableInfo {
    pub name: EcoString,
    pub id: VarId,
    pub place: Place,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct FuncInfo {
    pub name: EcoString,
    pub id: FnId,
    pub params: Vec<VarId>,
    pub place: Place,
    pub params_ty: Vec<Type>,
    pub return_ty: Type,
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct Place(usize);
