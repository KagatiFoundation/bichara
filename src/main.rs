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

pub mod tokenizer;
pub mod error;
pub mod parser;
pub mod utils;
pub mod ast;
pub mod symtable;
pub mod enums;
pub mod register;
pub mod types;

/* 
global char n; 
def main() -> char { 
    n = 4; 
    return n; 
}
*/

fn main() { 
    let mut tokener: tokenizer::Tokenizer = tokenizer::Tokenizer::new("global integer *a; global integer *b; b = &a;");
    let tokens: Vec<tokenizer::Token> = tokener.start_scan();
    let reg_manager: Rc<RefCell<register::RegisterManager>> = Rc::new(RefCell::new(register::RegisterManager::new()));
    let sym_table: Rc<RefCell<symtable::Symtable>> = Rc::new(RefCell::new(symtable::Symtable::new()));
    let mut p: parser::Parser = parser::Parser::new(&tokens, Rc::clone(&sym_table));
    let mut traverser: ast::ASTTraverser = ast::ASTTraverser::new(Rc::clone(&reg_manager), Rc::clone(&sym_table));
    p.start(&mut traverser);
}