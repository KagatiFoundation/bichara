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

use std::{cell::RefCell, rc::Rc};

pub mod ast;
pub mod enums;
pub mod error;
pub mod parser;
pub mod register;
pub mod symtable;
pub mod tokenizer;
pub mod types;
pub mod utils;
pub mod function;
pub mod ast2;
pub mod symbol;
pub mod code_gen;

/*
global char n;
def main() -> char {
    n = 4;
    return n;
}
*/

fn main() {
    static mut LABEL_ID: usize = 0;
    let mut tokener: tokenizer::Tokenizer = tokenizer::Tokenizer::new("global integer a = 23;");
    let reg_manager: Rc<RefCell<register::RegisterManager>> =
        Rc::new(RefCell::new(register::RegisterManager::new()));
    let sym_table: Rc<RefCell<symtable::Symtable>> =
        Rc::new(RefCell::new(symtable::Symtable::new()));
    let func_table: Rc<RefCell<function::FunctionInfoTable>> = Rc::new(RefCell::new(function::FunctionInfoTable::new()));
    let mut p: parser::Parser = parser::Parser::new(tokener.start_scan(), Rc::clone(&sym_table), Rc::clone(&func_table), unsafe { &mut LABEL_ID });
    let mut traverser: ast::ASTTraverser =
        ast::ASTTraverser::new(Rc::clone(&reg_manager), Rc::clone(&sym_table), Rc::clone(&func_table), unsafe { &mut LABEL_ID });
    p.start(&mut traverser);
}
