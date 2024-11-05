use std::cell::RefCell;
use std::iter;

use crate::hir::Identifier;
use crate::r#type::Type;
use crate::scope::{FnId, Scope, VarId};
use ecow::EcoString;

pub struct ResolveContext {
    pub global_scope: Scope,
    pub scope_stack: Vec<Scope>,
    pub variables: Vec<VariableInfo>,
    pub functions: Vec<FuncInfo>,
    pub places: Vec<Place>,
    next_ty_var_id: usize,
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
            next_ty_var_id: 0,
        }
    }
    pub fn define_builtins(&mut self) {
        self.define_global_fn(EcoString::from("printInt"), vec![EcoString::from("n")], vec![Type::Int], Type::Unit);
        self.define_global_fn(EcoString::from("printStr"), vec![EcoString::from("n")], vec![Type::Str], Type::Unit);
        self.define_global_fn(EcoString::from("inputInt"), vec![], vec![], Type::Int);
        self.define_global_fn(EcoString::from("inputStr"), vec![EcoString::from("len")], vec![Type::Int], Type::Str);
        self.define_global_fn(EcoString::from("chr"), vec![EcoString::from("n")], vec![Type::Int], Type::Char);
        self.define_global_fn(EcoString::from("ord"), vec![EcoString::from("ch")], vec![Type::Char], Type::Int);
        self.define_global_fn(EcoString::from("str"), vec![EcoString::from("n")], vec![Type::Int], Type::Str);
    }
    pub fn define_var(&mut self, name: EcoString, ident: Option<Identifier>, ty_hint: Option<Type>) -> VarId {
        let scope = self.scope_stack.last_mut().unwrap();
        let id = VarId(self.variables.len());
        self.variables.push(VariableInfo {
            name: name.clone(),
            ident,
            id,
            place: scope.place,
            ty: RefCell::new(Type::Invalid),
            ty_hint,
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
        let params_ty = params.iter().map(|&var_id| self.get_var(var_id).ty_hint.clone().unwrap()).collect();
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
            .map(|(name, ty)| self.define_var(name.clone(), None, Some(ty)))
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
            "str" => Type::Str,
            "char" => Type::Char,
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
    pub fn new_ty_var(&mut self) -> Type {
        let id = self.next_ty_var_id;
        self.next_ty_var_id += 1;
        Type::TyVar(id)
    }
}

pub struct VariableInfo {
    pub ident: Option<Identifier>,
    pub name: EcoString,
    pub id: VarId,
    pub place: Place,
    pub ty: RefCell<Type>,
    pub ty_hint: Option<Type>,
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
