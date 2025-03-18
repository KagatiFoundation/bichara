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

use std::slice::Iter;
use crate::sym::SymbolTrait;

// Maximum number of symbols in program
pub const NSYMBOLS: usize = 1024;

#[derive(Clone, Debug)]
pub struct Symtable<T: SymbolTrait + Clone> {
    syms: Vec<T>, //
    counter: usize,    // next free slot in the table
}

impl<T: SymbolTrait + Clone> Symtable<T> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            syms: vec![],
            counter: 0,
        }
    }

    pub fn find_symbol(&self, name: &str) -> Option<usize> {
        self.syms.iter().position(|sym| sym.name() == name)
    }

    fn next(&mut self) -> usize {
        assert!(self.counter < NSYMBOLS, "Symbol table is full!");
        self.counter += 1;
        self.counter
    }

    pub fn add_symbol(&mut self, sym: T) -> Option<usize> {
        let act_pos: usize = self.next();
        if self.find_symbol(&sym.name()).is_some() { 
            return None;
        }
        self.syms.push(sym);
        Some(act_pos - 1)
    }

    pub fn insert(&mut self, pos: usize, sym: T) -> Option<usize> {
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
    pub fn get_symbol(&self, idx: usize) -> Option<&T> {
        assert!(
            idx < NSYMBOLS,
            "value '{}' out of bounds for range '{}'",
            idx,
            self.counter
        );
        self.syms.get(idx)
    }

    pub fn get_symbol_mut(&mut self, idx: usize) -> Option<&mut T> {
        assert!(
            idx < NSYMBOLS,
            "value '{}' out of bounds for range '{}'",
            idx,
            self.counter
        );
        self.syms.get_mut(idx)
    }

    pub fn get_or_fail(&self, idx: usize) -> &T {
        if let Some(sym) = self.get_symbol(idx) {
            sym
        } else {
            panic!("Symbol at index '{}' not found in provided symbol table", idx);
        }
    }

   pub fn get_mut_or_fail(&mut self, idx: usize) -> &mut T {
        if let Some(sym) = self.get_symbol_mut(idx) {
            sym
        } else {
            panic!("Symbol at index '{}' not found in provided symbol table", idx);
        }
    }

    pub fn remove_symbol(&mut self, index: usize) -> T {
        assert!(self.counter < NSYMBOLS);
        self.syms.remove(index)
    }

    pub fn iter(&self) -> Iter<'_, T> {
        self.syms.iter()
    }

    pub fn count(&self) -> usize {
        self.syms.len()
    }
}

#[cfg(test)]
mod tests {
    use kagc_types::LitTypeVariant;

    use crate::{sym::{StorageClass, Symbol, SymbolType}, symbol_table::Symtable};

    #[test]
    fn test_symbol_addition() {
        let mut table: Symtable<Symbol> = Symtable::new();
        matches!(
            table.insert(0, Symbol::new(
                String::from("number"),
                LitTypeVariant::I32,
                SymbolType::Variable,
                StorageClass::GLOBAL
            )),
            Option::Some(0)
        );
        assert_eq!(
            table.insert(1, Symbol::new(
                String::from("number2"),
                LitTypeVariant::I32,
                SymbolType::Variable,
                StorageClass::GLOBAL
            )),
            Option::Some(1)
        );
        assert_eq!(
            table.insert(2, Symbol::new(
                String::from("number3"),
                LitTypeVariant::I32,
                SymbolType::Variable,
                StorageClass::GLOBAL
            )),
            Option::Some(2)
        );
    }

    #[test]
    fn test_find_symbol_index_from_its_name() {
        let mut table: Symtable<Symbol> = Symtable::new();
        table.insert(0, Symbol::new(
            String::from("number2"),
            LitTypeVariant::I32,
            SymbolType::Variable,
            StorageClass::GLOBAL
        ));
        table.insert(1, Symbol::new(
            String::from("number3"),
            LitTypeVariant::I32,
            SymbolType::Variable,
            StorageClass::GLOBAL
        ));
        table.insert(2, Symbol::new(
            String::from("number4"),
            LitTypeVariant::I32,
            SymbolType::Variable,
            StorageClass::GLOBAL
        ));
        assert_eq!(table.find_symbol("number2"), Option::Some(0));
        assert_eq!(table.find_symbol("number3"), Option::Some(1));
        assert_eq!(table.find_symbol("number4"), Option::Some(2));
    }

    #[test]
    fn test_symbol_removal() {
        let mut table: Symtable<Symbol> = Symtable::new();
        table.insert(0, Symbol::new(
            String::from("number2"),
            LitTypeVariant::I32,
            SymbolType::Variable,
            StorageClass::GLOBAL
        ));
        table.insert(1, Symbol::new(
            String::from("number3"),
            LitTypeVariant::I32,
            SymbolType::Variable,
            StorageClass::GLOBAL
        ));
        table.insert(2, Symbol::new(
            String::from("number4"),
            LitTypeVariant::I32,
            SymbolType::Variable,
            StorageClass::GLOBAL
        ));
        assert_eq!(
            table.remove_symbol(0),
            Symbol::new(
                String::from("number2"),
                LitTypeVariant::I32,
                SymbolType::Variable,
                StorageClass::GLOBAL
            )
        );
    }
}