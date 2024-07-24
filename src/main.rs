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

pub mod ast;
pub mod code_gen;
pub mod context;
pub mod error;
pub mod parser;
pub mod symbol;
pub mod tokenizer;
pub mod types;
pub mod utils;

use std::{cell::RefCell, rc::Rc};

use ast::SourceFile;
use code_gen::{Aarch64CodeGen, RegManager};
use context::CompilerCtx;
use parser::*;
use symbol::*;
use tokenizer::Tokenizer;

fn main() {
    let tokener = Rc::new(RefCell::new(Tokenizer::new()));
    let parsr = Rc::new(RefCell::new(Parser::new()));
    let mut symt: Symtable = Symtable::new();
    let mut funct: FunctionInfoTable = FunctionInfoTable::new();
    let mut file1: SourceFile =
        SourceFile::new("/Users/rigelstar/Desktop/KagatiFoundation/bichara/examples/input1.bic");
    let mut source_files: Vec<&mut SourceFile> = vec![&mut file1];
    let ctx = Rc::new(RefCell::new(CompilerCtx::new(&mut symt, &mut funct)));
    let rm: RefCell<RegManager> = RefCell::new(RegManager::new({
        let mut regs: Vec<String> = vec![];
        for i in 0..8 {
            regs.push(format!("x{}", i));
        }
        regs
    }));
    let mut cg = Aarch64CodeGen::new(rm);
    for sf in &mut source_files {
        let read_res: Result<i32, std::io::Error> = sf.read();
        if let Err(e) = read_res {
            panic!("Error reading a source file: {:?}", e);
        }
    }
    for sf in &mut source_files {
        sf.tokenize(Rc::clone(&tokener));
    }
    {
        let mut parser_borrow = parsr.borrow_mut();
        for sf in &mut source_files {
            let tokens: Vec<tokenizer::Token> = sf.tokens.clone().unwrap();
            ctx.borrow_mut().current_file = Some(sf);
            let parse_result: Vec<ast::AST> = parser_borrow.parse_with_ctx(Rc::clone(&ctx), tokens);
            if !parser_borrow.has_parsing_errors() {
                cg.gen_with_ctx(Rc::clone(&ctx), parse_result);
            }
        }
        std::mem::drop(parser_borrow);
    }
}
