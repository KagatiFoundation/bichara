/*
MIT License

Copyright (c) 2023 Kagati Foundation

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#![allow(clippy::new_without_default)]

use std::collections::HashMap;
use crate::types::LitTypeVariant;

use super::{StorageClass, Symbol, SymbolTrait, Symtable};

/// Limit of local variables in a function.
pub const LOCAL_LIMIT: usize = 1024;

/// Inavlid function ID.
pub const INVALID_FUNC_ID: usize = 0xFFFFFFFF;

/// Represents a function parameter in the symbol table.
#[derive(Clone, Debug)]
pub struct FuncParam {
    /// The type of the function parameter.
    pub lit_type: LitTypeVariant,

    /// The name of the function parameter.
    pub name: String,

    pub offset: i32
}

impl SymbolTrait for FuncParam {
    fn uninit() -> Self {
        Self {
            lit_type: LitTypeVariant::None,
            name: "".to_string(),
            offset: -1
        }
    }

    fn name(&self) -> String {
        self.name.clone()
    }

    fn is_unused(&self) -> bool {
        false
    }
}

#[derive(Clone, Debug)]
pub struct LocalSymbol {
    pub name: String,
    pub sym_type: LitTypeVariant,
    pub offset: usize,
}

#[derive(Clone, Debug)]
pub struct FunctionInfo {
    pub name: String,
    pub func_id: usize,
    pub stack_size: i32,
    pub return_type: LitTypeVariant,
    /// Contains information about the variables defined locally in 'this' function
    pub local_syms: Symtable<Symbol>,
    pub storage_class: StorageClass,
    pub params: Symtable<FuncParam>,
}

impl FunctionInfo {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        name: String, 
        func_id: usize, 
        stack_size: i32, 
        return_type: LitTypeVariant,
        storage_class: StorageClass,
        params: Symtable<FuncParam>,
        locals: Symtable<Symbol>,
    ) -> Self {
        Self {
            name, 
            func_id,
            stack_size, 
            return_type, 
            local_syms: locals,
            storage_class,
            params,
        }
    }

    pub fn has_param(&self, param_name: &str) -> bool {
        self.params.find_symbol(param_name).is_some()
    }
}

#[derive(Debug)]
pub struct FunctionInfoTable {
    functions: HashMap<String, FunctionInfo>,
}

impl FunctionInfoTable {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new()
        }
    }
    
    pub fn add(&mut self, func_info: FunctionInfo) {
        self.functions.insert(func_info.name.clone(), func_info);
    }

    pub fn get(&self, name: &str) -> Option<&FunctionInfo> {
        self.functions.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut FunctionInfo> {
        self.functions.get_mut(name)
    }
}