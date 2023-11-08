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

use crate::tokenizer::{Token, TokenKind};

#[derive(Debug, Clone)]
pub enum LitType {
    Integer(i32),
    Long(i64),
    Short(i16),
    Char(u8),
    String(String),
    Double(f64),
    Float(f32),
}

pub struct ASTNode {
    pub operation: TokenKind,
    pub left: Option<Box<ASTNode>>,
    pub right: Option<Box<ASTNode>>,
    pub value: LitType
}

impl ASTNode {
    pub fn new(op: TokenKind, left: ASTNode, right: ASTNode, value: LitType) -> Self {
        Self { operation: op, left: Some(Box::new(left)), right: Some(Box::new(right)), value }
    }

    pub fn make_leaf(op: TokenKind, value: LitType) -> Self {
        Self { operation: op, left: None, right: None, value }
    }
}

// Actual parser
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    #[inline]
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0, }
    }

    pub fn parse_addition(&mut self) -> ASTNode {
        let mut left: ASTNode = self.parse_multiplicative();
        let mut right: ASTNode;
        if let Some(top) = self.peek() {
            let mut kind: TokenKind = top.kind;
            loop {
                self.skip();
                right = self.parse_multiplicative();
                left = ASTNode::new(Parser::is_arith_op(kind), left, right, LitType::Integer(0));
                if let Some(now) = self.peek() { kind = now.kind; } 
                else { break; }
            }
        }
        left
    }

    pub fn parse_multiplicative(&mut self) -> ASTNode {
        let mut left: ASTNode = self.parse_primary();
        let mut right: ASTNode;
        if let Some(top) = self.peek() {
            let mut kind: TokenKind = top.kind;
            while kind == TokenKind::T_STAR || kind == TokenKind::T_SLASH {
                self.skip();
                right = self.parse_primary();
                left = ASTNode::new(Parser::is_arith_op(kind), left, right, LitType::Integer(0));
                if let Some(now) = self.peek() { kind = now.kind; } 
                else { break; }
            }
        }
        left
    }

    pub fn parse_primary(&mut self) -> ASTNode {
        if let Some(top) = self.peek() {
            self.skip(); // skip the primary token
            match top.kind {
                TokenKind::T_INT_NUM => {
                    let int_val: i32 = top.lexeme.parse().unwrap();
                    return ASTNode::make_leaf(TokenKind::T_INT_NUM, LitType::Integer(int_val));
                },
                TokenKind::T_FLOAT_NUM => {
                    let float_val: f32 = top.lexeme.parse().unwrap();
                    return ASTNode::make_leaf(TokenKind::T_FLOAT_NUM, LitType::Float(float_val));
                },
                _ => {
                    panic!("syntax error on line {}!", top.pos.line);
                }
            }
        }
        panic!("unexpected EOF!");
    }

    pub fn is_arith_op(tk: TokenKind) -> TokenKind {
        match tk {
            TokenKind::T_PLUS | TokenKind::T_STAR | TokenKind::T_MINUS | TokenKind::T_SLASH => tk,
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
    use super::*;
    use crate::tokenizer::*;
}