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

use super::StorageClass;

/// Limit of local variables in a function.
pub const LOCAL_LIMIT: usize = 1024;

#[derive(Clone)]
pub struct LocalSymbol {
    pub name: String,
    pub sym_type: LitTypeVariant,
    pub offset: usize,
}

#[derive(Clone)]
pub struct LocalSymtable {
    pub syms: Vec<LocalSymbol>,
    pub counter: usize,
}

impl LocalSymtable {
    pub fn new() -> Self {
        Self {
            syms: vec![],
            counter: 0
        }
    }

    /// Add a new symbol
    pub fn add(&mut self, symbol: LocalSymbol) -> usize {
        assert!(self.syms.len() < LOCAL_LIMIT, "Local variable count has exceeded the limit of '{}'", LOCAL_LIMIT);
        self.syms.push(symbol);
        self.syms.len() - 1
    }

    /// Get the position of symbol with the provided name if it exists
    pub fn position(&self, name: &str) -> Option<usize> {
        assert!(name.is_empty(), "Name can't be an empty string");
        self.syms.iter().position(|item| item.name == name)
    }

    /// Get the symbol with the provided name if it exists
    pub fn get(&self, name: &str) -> Option<&LocalSymbol> {
        if let Some(pos) = self.position(name) {
            return self.syms.get(pos);
        }
        None
    }

    pub fn remove(&mut self, name: &str) -> Option<LocalSymbol> {
        if let Some(pos) = self.position(name) {
            return Some(self.syms.remove(pos));
        }
        None
    }
}

#[derive(Clone)]
pub struct FunctionInfo {
    pub name: String,
    pub func_id: usize,
    pub stack_size: i32,
    pub return_type: LitTypeVariant,
    /// Contains information about the variables defined locally in 'this' function
    pub local_syms: LocalSymtable,
    pub storage_class: StorageClass
}

impl FunctionInfo {
    pub fn new(
        name: String, 
        func_id: usize, 
        stack_size: i32, 
        return_type: LitTypeVariant,
        storage_class: StorageClass
    ) -> Self {
        Self {
            name, 
            func_id,
            stack_size, 
            return_type, 
            local_syms: LocalSymtable::new(),
            storage_class
        }
    }
}

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
}