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

use crate::enums::*;
use crate::types::*;
use std::slice::Iter;

#[derive(Clone)]
pub struct Symbol {
    pub name: String,
    pub lit_type: LitType, // What kind of value this symbol is
    pub sym_type: SymbolType // What type of symbol this is. i.e. Variable or Function
}

impl Symbol {
    pub fn new(name: String, lit: LitType, sym_type: SymbolType) -> Self {
        Self { name, lit_type: lit, sym_type }
    }
}

#[derive(Clone)]
pub struct Symtable {
    syms: Vec<Symbol>, // 
    counter: usize, // next free slot in the table
}

impl Symtable {
    pub fn iter(&self) -> Iter<'_, Symbol> {
        self.syms.iter()
    }
}

// Maximum number of symbols in program
pub const NSYMBOLS: usize = 1024;

impl Symtable {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self { Self { syms: vec![], counter: 0 } }

    pub fn find_symbol(&self, name: &str) -> usize {
        for (idx, n) in self.syms.iter().enumerate() {
            if name == n.name { return idx; }
        }
        0xFFFFFFFF
    }

    fn next(&mut self) -> usize {
        self.counter += 1;
        if self.counter >= NSYMBOLS {
            panic!("too many global symbols");
        }
        self.counter
    }

    pub fn add_symbol(&mut self, sym: Symbol) -> usize {
        let mut pos: usize = self.find_symbol(&sym.name);
        if pos != 0xFFFFFFFF {
            return pos;
        }
        pos = self.next();
        self.syms.push(sym);
        pos - 1
    }

    pub fn get_symbol(&self, idx: usize) -> &Symbol {
        if idx >= self.syms.len() {
            panic!("index '{}' out of bounds for range '{}'", idx, self.syms.len());
        }
        self.syms.get(idx).unwrap()
    }
}