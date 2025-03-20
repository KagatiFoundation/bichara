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

use kagc_ast::SourceFile;
use kagc_symbol::{FunctionInfo, FunctionInfoTable, Symbol, Symtable, INVALID_FUNC_ID};

use crate::errors::CtxError;

#[derive(Debug, Eq, PartialEq)]
pub enum CompilerScope {
    GLOBAL,
    FUNCTION
}

#[derive(Debug)]
pub struct CompilerCtx<'ctx> {
    /// Symbol table that is passed around each compilation unit
    /// during the whole compilation process.
    pub sym_table: &'ctx mut Symtable<Symbol>,

    /// Functions information table.
    pub func_table: &'ctx mut FunctionInfoTable,

    /// Next label ID.
    pub label_id: usize,

    /// Source file that is currently being processed.
    pub current_file: Option<&'ctx SourceFile>,

    pub current_function: usize,

    pub scope: CompilerScope
}

impl<'ctx> CompilerCtx<'ctx> {
    pub fn new(symt: &'ctx mut Symtable<Symbol>, func_table: &'ctx mut FunctionInfoTable) -> Self {
        Self {
            sym_table: symt,
            func_table,
            label_id: 0,
            current_file: None,
            current_function: INVALID_FUNC_ID,
            scope: CompilerScope::GLOBAL
        }
    }

    pub fn incr_label_count(&mut self) {
        self.label_id += 1;
    }

    pub fn get_curr_func(&self) -> Option<&FunctionInfo> {
        if let Some(symbol) = self.sym_table.get_symbol(self.current_function) {
            if let Some(func_info) = self.func_table.get(&symbol.name) {
                return Some(func_info);
            }
        }
        None
    }

    pub fn get_curr_func_mut(&mut self) -> Option<&mut FunctionInfo> {
        if let Some(symbol) = self.sym_table.get_symbol(self.current_function) {
            if let Some(func_info) = self.func_table.get_mut(&symbol.name) {
                return Some(func_info);
            }
        }
        None
    }

    pub fn switch_to_func_scope(&mut self, func_id: usize) {
        self.current_function = func_id;
        self.scope = CompilerScope::FUNCTION;
    }

    pub fn switch_to_global_scope(&mut self) {
        self.current_function = INVALID_FUNC_ID;
        self.scope = CompilerScope::GLOBAL;
    }

    /// Searches for a symbol either in the local scope of the current function 
    /// or in the global context.
    /// 
    /// # Returns
    /// * `Ok(Symbol)` - The symbol found at the specified index.
    /// * `Err(CodeGenErr::UndefinedSymbol)` - If neither local nor global context is 
    ///   available for the lookup.
    pub fn find_sym(&self, name: &str) -> Result<&Symbol, CtxError> {
        if let Some(func_info) = self.get_curr_func() {
            if let Some(sym_pos) = func_info.local_syms.find_symbol(name) {
                if let Some(sym) = func_info.local_syms.get_symbol(sym_pos) {
                    return Ok(sym);
                }
            }
        }
        return self.find_sym_in_table(name, self.sym_table);
    }

    fn find_sym_in_table<'a>(&'a self, name: &str, table: &'a Symtable<Symbol>) -> Result<&Symbol, CtxError> {
        if let Some(sym_pos) = table.find_symbol(name) {
            return if let Some(sym) = table.get_symbol(sym_pos) {
                Ok(sym)
            } else {
                Err(CtxError::UndefinedSymbol)
            };
        }
        Err(CtxError::UndefinedSymbol)
    }

    pub fn find_sym_mut(&mut self, name: &str) -> Result<&mut Symbol, CtxError> {
        if let Some(local_sym_pos) = {
            if let Some(func_info) = self.get_curr_func_mut() {
                func_info.local_syms.find_symbol(name)
            } else {
                None
            }
        } {
            if let Some(func_mut) = self.get_curr_func_mut() {
                if let Some(mut_sym) = func_mut.local_syms.get_symbol_mut(local_sym_pos) {
                    return Ok(mut_sym);
                }
            }
        } else if let Some(sym_pos) = self.sym_table.find_symbol(name) {
            if let Some(sym) = self.sym_table.get_symbol_mut(sym_pos) {
                return Ok(sym);
            } else {
                return Err(CtxError::UndefinedSymbol);
            }
        }
        Err(CtxError::UndefinedSymbol)
    }

    pub fn get_func_name(&self, index: usize) -> Option<String> {
        let func_name: String = self.sym_table.get_symbol(index).unwrap().name.clone();
        self.func_table.get(&func_name).map(|_| func_name)
    }
}