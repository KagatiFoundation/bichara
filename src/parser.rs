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
use crate::ast::*;

extern crate lazy_static;
use lazy_static::lazy_static;

lazy_static! {
    static ref PRECENDENCE: Vec<u8> = vec![0];
}

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
        if let Some(binary) = self.parse_binary() {
            result.push(binary);
        }
        result
    }

    pub fn parse_binary(&mut self) -> Option<ASTNode> {
        let left: Option<ASTNode> = self.parse_primary();
        if self.tokens[self.current].kind == TokenKind::T_EOF {
            return left;
        }
        let kind: ASTNodeKind = self.parse_arith_operator(); 
        let right: Option<ASTNode> = self.parse_binary();
        Some(ASTNode::new(kind, left.unwrap(), right.unwrap(), LitType::Integer(0)))
    }

    fn parse_primary(&mut self) -> Option<ASTNode> {
        let current_token: &Token = &self.tokens[self.current];
        self.current += 1;
        match current_token.kind {
            TokenKind::T_INT_NUM => Some(ASTNode::make_leaf(ASTNodeKind::AST_INTLIT, LitType::Integer(current_token.lexeme.parse::<i32>().unwrap()))),
            TokenKind::T_FLOAT_NUM => Some(ASTNode::make_leaf(ASTNodeKind::AST_INTLIT, LitType::Float(current_token.lexeme.parse::<f32>().unwrap()))),
            _ => {
                println!("{:?}", current_token);
                panic!("please provide an integer number");
            }
        }
    }

    fn parse_arith_operator(&mut self) -> ASTNodeKind {
        let token_type: TokenKind = self.tokens[self.current].kind;
        let result: ASTNodeKind = match token_type {
            TokenKind::T_PLUS => ASTNodeKind::AST_ADD,
            TokenKind::T_MINUS => ASTNodeKind::AST_SUBTRACT,
            TokenKind::T_STAR => ASTNodeKind::AST_MULTIPLY,
            TokenKind::T_SLASH => ASTNodeKind::AST_DIVIDE,
            _ => panic!("Please provide an arithmetic operator. {:?} is not an arithmetic operator", token_type)
        };
        self.current += 1;
        result
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
    use super::*; 

    // test depth 1 binary tree
    #[test]
    fn test_depth_one_bin_tree() {
        let mut tokener: Tokenizer = Tokenizer::new("5+5");
        let tokens: Vec<Token> = tokener.start_scan();
        let mut p: Parser = Parser::new(tokens);
        let nodes: Vec<ASTNode> = p.parse_stmts();
        assert_eq!(nodes.len(), 1);
        assert_eq!(nodes[0].operation, ASTNodeKind::AST_ADD);
        assert_eq!(nodes[0].left.as_ref().unwrap().value, LitType::Integer(5));
        assert_eq!(nodes[0].right.as_ref().unwrap().value, LitType::Integer(5));
    }
}