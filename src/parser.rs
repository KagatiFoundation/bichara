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

use std::vec;

use crate::tokenizer::*;
use crate::symtable::*; 
use crate::enums::*;
use crate::ast::ASTNode;

// Actual parser
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    pub sym_table: Symtable, // symbol table for global identifiers
}

impl Parser {
    #[inline]
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0, sym_table: Symtable::new() }
    }

    pub fn parse_stmts(&mut self) -> Vec<ASTNode> {
        let mut result: Vec<ASTNode> = vec![];
        for token in self.tokens.clone() {
            if token.kind == TokenKind::KW_INT {
                self.parse_var_decl();
            } 
            else if token.kind == TokenKind::T_IDENTIFIER {
                if let Some(assign) = self.parse_assignment() {
                    result.push(assign);
                }
            }
            else if token.kind == TokenKind::T_EOF { break; }
        }
        result
    }

    fn parse_assignment(&mut self) -> Option<ASTNode> {
        // expect 'id'
        if let Some(id) = self.peek() {
            if id.kind != TokenKind::T_IDENTIFIER {
                panic!("expected id");
            }
            let id_idx: usize = self.sym_table.find(&id.lexeme);
            if id_idx == 0xFFFFFFFF {
                panic!("variable does not exist");
            }
            let right: ASTNode = ASTNode::make_leaf(ASTNodeKind::AST_LVIDENT, LitType::Integer(id_idx as i32));
            self.current += 1; // skip 'id'
            // expect '='
            if let Some(eq) = self.peek() {
                if eq.kind != TokenKind::T_EQUAL {
                    panic!("expected =");
                }
                self.current += 1; // skip '='
            }
            let left: ASTNode  = self.parse_addition();
            let result: Option<ASTNode> = Some(ASTNode::new(ASTNodeKind::AST_ASSIGN, left, right, LitType::Integer(-1)));
            // self.current += 1; // skip value
            // expect ';'
            if let Some(semi) = self.peek() {
                if semi.kind != TokenKind::T_SEMICOLON {
                    panic!("expected ;");
                }
                self.current += 1; // skip ';'
            }
            return result;
        }
        None
    }

    fn parse_var_decl(&mut self) {
        if let Some(top) = self.peek() {
            if top.kind != TokenKind::KW_INT {
                panic!("expected the token to be KW_INT");
            }
            self.current += 1; // skip 'int' keyword
            // we now expect the next token to be a identifier kind
            if let Some(id) = self.peek() {
                if id.kind != TokenKind::T_IDENTIFIER {
                    panic!("expected the token to be T_IDENTIFIER");
                }
                self.current += 1; // skip id
                // expect semicolon next
                if let Some(semi) = self.peek() {
                    if semi.kind != TokenKind::T_SEMICOLON {
                        panic!("expected the token to be T_SEMICOLON");
                    }
                    self.current += 1; // skip ';'
                    self.sym_table.add(&id.lexeme);
                    println!("\tcommon\t{} 8:8\n", id.lexeme);
                }
            }
        }
    }

    fn parse_addition(&mut self) -> ASTNode {
        let mut left: ASTNode = self.parse_multiplicative();
        let mut right: ASTNode;
        if let Some(top) = self.peek() {
            let mut kind: TokenKind = top.kind;
            loop {
                self.skip();
                right = self.parse_multiplicative();
                left = ASTNode::new(Parser::get_arith_op(kind), left, right, LitType::Integer(0));
                if let Some(now) = self.peek() { kind = now.kind; } 
                else { break; }
            }
        }
        left
    }

    fn parse_multiplicative(&mut self) -> ASTNode {
        let mut left: ASTNode = self.parse_primary();
        let mut right: ASTNode;
        if let Some(top) = self.peek() {
            let mut kind: TokenKind = top.kind;
            while kind == TokenKind::T_STAR || kind == TokenKind::T_SLASH {
                self.skip();
                right = self.parse_primary();
                left = ASTNode::new(Parser::get_arith_op(kind), left, right, LitType::Integer(0));
                if let Some(now) = self.peek() { kind = now.kind; } 
                else { break; }
            }
        }
        left
    }

    fn parse_primary(&mut self) -> ASTNode {
        if let Some(top) = self.peek() {
            self.skip(); // skip the primary token
            match top.kind {
                TokenKind::T_INT_NUM => {
                    let int_val: i32 = top.lexeme.parse().unwrap();
                    return ASTNode::make_leaf(ASTNodeKind::AST_INTLIT, LitType::Integer(int_val));
                },
                _ => {
                    panic!("syntax error on line {}!", top.pos.line);
                }
            }
        }
        panic!("unexpected EOF!");
    }

    fn get_arith_op(tk: TokenKind) -> ASTNodeKind {
        match tk {
            TokenKind::T_PLUS => ASTNodeKind::AST_ADD,
            TokenKind::T_MINUS => ASTNodeKind::AST_SUBTRACT,
            TokenKind::T_STAR => ASTNodeKind::AST_MULTIPLY,
            TokenKind::T_SLASH => ASTNodeKind::AST_DIVIDE,
            _ => {
                panic!("unknown arithmetic operator");
            }
        }
    }

    #[inline]
    fn skip(&mut self) {
        self.current += 1;
    }

    #[inline]
    fn peek(&self) -> Option<Token> {
        if !self.is_at_end() { Some(self.tokens[self.current].clone()) } else { None }
    }

    #[inline]
    fn is_at_end(&self) -> bool {
        if self.current < self.tokens.len() && self.tokens[self.current].kind == TokenKind::T_EOF { return true; }
        self.current >= self.tokens.len()
    }
}

#[cfg(test)]
mod tests {
    
}