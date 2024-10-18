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

use crate::{ast::SourceFile, FunctionInfoTable, Symbol, Symtable};

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
}

impl<'ctx> CompilerCtx<'ctx> {
    pub fn new(symt: &'ctx mut Symtable<Symbol>, func_table: &'ctx mut FunctionInfoTable) -> Self {
        Self {
            sym_table: symt,
            func_table,
            label_id: 0,
            current_file: None,
        }
    }

    pub fn incr_label_count(&mut self) {
        self.label_id += 1;
    }
}
