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
        }
    }
}

#[derive(Clone, Debug)]
pub struct Symtable {
    syms: Vec<Symbol>, //
    counter: usize,    // next free slot in the table
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
    pub fn new() -> Self {
        Self {
            syms: vec![Symbol::uninit(); NSYMBOLS],
            counter: 0,
        }
    }

    pub fn find_symbol(&self, name: &str) -> Option<usize> {
        for (idx, n) in self.syms.iter().enumerate() {
            if name == n.name {
                return Some(idx);
            }
        }
        None
    }

    fn next(&mut self) -> usize {
        assert!(self.counter < NSYMBOLS, "Symbol table is full!");
        self.counter += 1;
        self.counter
    }

    pub fn add_symbol(&mut self, sym: Symbol) -> Option<usize> {
        if self.find_symbol(&sym.name).is_some() { 
            return None;
        }
        let act_pos: usize = self.next();
        self.syms.push(sym);
        Some(act_pos - 1)
    }

    pub fn insert(&mut self, pos: usize, sym: Symbol) -> Option<usize> {
        assert!(
            pos < NSYMBOLS,
            "value '{}' out of bounds for range '{}'",
            pos,
            self.counter
        );
        self.syms[pos] = sym;
        Some(pos)
    }

    // TODO: Convert return type to Option<&Symbol>
    pub fn get_symbol(&self, idx: usize) -> Option<&Symbol> {
        assert!(
            idx < NSYMBOLS,
            "value '{}' out of bounds for range '{}'",
            idx,
            self.counter
        );
        self.syms.get(idx)
    }

    pub fn remove_symbol(&mut self, index: usize) -> Symbol {
        assert!(self.counter < NSYMBOLS);
        self.syms.remove(index)
    }
}

#[cfg(test)]
mod tests {
    use super::{Symbol, SymbolType, Symtable, NSYMBOLS};

    #[test]
    fn test_symbol_addition() {
        let mut table: Symtable = Symtable::new();
        matches!(
            table.add_symbol(Symbol::new(
                String::from("number"),
                super::LitTypeVariant::I32,
                SymbolType::Variable,
                crate::symtable::StorageClass::GLOBAL
            )),
            Option::Some(0)
        );
        assert_eq!(table.syms.len(), 1);
        assert_eq!(
            table.add_symbol(Symbol::new(
                String::from("number2"),
                super::LitTypeVariant::I32,
                SymbolType::Variable,
                crate::symtable::StorageClass::GLOBAL
            )),
            Option::Some(1)
        );
        assert_eq!(
            table.add_symbol(Symbol::new(
                String::from("number3"),
                super::LitTypeVariant::I32,
                SymbolType::Variable,
                crate::symtable::StorageClass::GLOBAL
            )),
            Option::Some(2)
        );
    }

    // This test insures that no more than 1024 symbols are defined in program.
    #[test]
    #[should_panic(expected = "assertion failed")]
    fn test_more_than_1024_symbols_creates_panic_situation() {
        let mut table: Symtable = Symtable::new();
        table.counter = NSYMBOLS;
        table.add_symbol(Symbol::new(
            String::from("number"),
            super::LitTypeVariant::I32,
            SymbolType::Variable,
            crate::symtable::StorageClass::GLOBAL
        ));
    }

    #[test]
    fn test_find_symbol_index_from_its_name() {
        let mut table: Symtable = Symtable::new();
        table.add_symbol(Symbol::new(
            String::from("number2"),
            super::LitTypeVariant::I32,
            SymbolType::Variable,
            crate::symtable::StorageClass::GLOBAL
        ));
        table.add_symbol(Symbol::new(
            String::from("number3"),
            super::LitTypeVariant::I32,
            SymbolType::Variable,
            crate::symtable::StorageClass::GLOBAL
        ));
        table.add_symbol(Symbol::new(
            String::from("number4"),
            super::LitTypeVariant::I32,
            SymbolType::Variable,
            crate::symtable::StorageClass::GLOBAL
        ));
        assert_eq!(table.find_symbol("number2"), Option::Some(0));
        assert_eq!(table.find_symbol("number3"), Option::Some(1));
        assert_eq!(table.find_symbol("number4"), Option::Some(2));
    }

    #[test]
    fn test_symbol_removal() {
        let mut table: Symtable = Symtable::new();
        table.add_symbol(Symbol::new(
            String::from("number2"),
            super::LitTypeVariant::I32,
            SymbolType::Variable,
            crate::symtable::StorageClass::GLOBAL
        ));
        table.add_symbol(Symbol::new(
            String::from("number3"),
            super::LitTypeVariant::I32,
            SymbolType::Variable,
            crate::symtable::StorageClass::GLOBAL
        ));
        table.add_symbol(Symbol::new(
            String::from("number4"),
            super::LitTypeVariant::I32,
            SymbolType::Variable,
            crate::symtable::StorageClass::GLOBAL
        ));
        assert_eq!(
            table.remove_symbol(0),
            Symbol::new(
                String::from("number2"),
                crate::symtable::LitTypeVariant::I32,
                SymbolType::Variable,
                crate::symtable::StorageClass::GLOBAL
            )
        );
    }
}
