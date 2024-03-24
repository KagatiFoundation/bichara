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

pub mod enums;
pub mod error;
pub mod parser;
pub mod register;
pub mod symtable;
pub mod tokenizer;
pub mod types;
pub mod utils;
pub mod function;
pub mod ast;
pub mod symbol;
pub mod code_gen;

use std::cell::RefCell;
use function::FunctionInfoTable;
use parser::Parser;
use symtable::Symtable;
use ast::{
    ASTOperation, 
    AST
};
use code_gen::{
    Aarch64CodeGen,
    CodeGen,
    RegManager
};

/*
global char n;
def main() -> char {
    n = 4;
    return n;
}
*/

fn main() {
    static mut LABEL_ID: usize = 0;
    let mut tokener: tokenizer::Tokenizer = tokenizer::Tokenizer::new("global integer number; def main() -> void { number = 34; local integer b = 23; return; }");
    let mut symt: Symtable = Symtable::new();
    let mut funct: FunctionInfoTable = FunctionInfoTable::new();
    let mut pars: Parser = Parser::new(tokener.start_scan(), &mut symt, &mut funct, unsafe {
        &mut LABEL_ID
    });
    let nodes: Vec<AST> = pars.parse();
    let rm: RefCell<RegManager> = RefCell::new(RegManager::new({
        let mut regs: Vec<String> = vec![];
        for i in 0..8 {
            regs.push(format!("x{}", i));
        }
        regs
    }));
    let mut cg: Aarch64CodeGen = Aarch64CodeGen::new(rm, &mut symt, &mut funct, unsafe {
        &mut LABEL_ID
    });
    for node in &nodes {
        cg.gen_code_from_ast(node, 0xFFFFFFFF, ASTOperation::AST_NONE);
    }
}
