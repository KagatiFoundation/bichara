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

use crate::types::{LitType, LitTypeVariant};

// The types of symbol names inside the symbol table
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum SymbolType {
    Variable,
    Function,
    Array,
    Constant, // for now, this is used to represent only the string literals
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum StorageClass {
    GLOBAL, // globally visible symbol
    LOCAL, // locally visible symbol
    PARAM, // locally visible function parameter
}

#[derive(Clone, PartialEq, Debug)]
pub struct Symbol {
    pub name: String,
    pub lit_type: LitTypeVariant, // What kind of value this symbol is
    pub sym_type: SymbolType,     // What type of symbol this is. i.e. Variable or Function
    pub size: usize,              // number of elements in the symbol
    pub class: StorageClass,
    pub local_offset: i32, // for locals, offset from the stack base pointer

    /// Default value this symbol has. This is only set when 
    /// the identifier is a global one.
    pub default_value: Option<LitType>
}

impl Symbol {
    pub fn new(name: String, lit: LitTypeVariant, sym_type: SymbolType, class: StorageClass) -> Self {
        Self {
            name,
            lit_type: lit,
            sym_type,
            size: 1,
            class,
            local_offset: 0,
            default_value: None
        }
    }

    /// Get an uninitialized(default) symbol
    pub fn uninit() -> Self {
        Self {
            name: String::from(""),
            lit_type: LitTypeVariant::None,
            sym_type: SymbolType::Variable, // we don't have a None type
            size: 0,                        // oooooh, scary
            class: StorageClass::GLOBAL,
            local_offset: 0,
            default_value: None
        }
    }
}