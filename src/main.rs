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

use crate::ast::ASTTraverser;

pub mod tokenizer;
pub mod error;
pub mod parser;
pub mod utils;
pub mod ast;
pub mod symtable;
pub mod enums;

fn main() {
    let mut tokener: tokenizer::Tokenizer = tokenizer::Tokenizer::new("5+5");
    let tokens: Vec<tokenizer::Token> = tokener.start_scan();
    let mut p: parser::Parser = parser::Parser::new(tokens);
    let nodes: Vec<ast::ASTNode> = p.parse_stmts();
    let mut ast_traverser: ASTTraverser = ASTTraverser::new(symtable::Symtable::new());
    for node in &nodes {
        ast_traverser.traverse(node);
    }
}