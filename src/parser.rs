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

#[derive(Debug)]
pub enum LitType {
    Integer(i32),
    Long(i64),
    Short(i16),
    Char(u8),
    String(String),
    Double(f64),
    Float(f32),
}

pub trait Expr {}

pub struct LitExpr {
    value: LitType
}

pub struct BinExpr {
    left: Box<dyn Expr>,
    right: Box<dyn Expr>,
    op: String,
}

impl Expr for LitExpr{}

impl Expr for BinExpr{}

pub enum ExprNode {
    LiteralExpression(LitExpr),   
    BinaryExpression(BinExpr), 
}

pub trait Stmt{}

pub struct VarDeclStmt {
    name: String,
    value: Box<dyn Expr>,
}

pub enum StmtNode {
    VariableDeclration(VarDeclStmt),
}

pub enum ASTNode {
    ExpressionNode(ExprNode),
    StatementNode(StmtNode)
}

// Actual parser
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
        }
    }

    pub fn start_parser(&mut self) -> Vec<StmtNode> {
        let mut stmts: Vec<StmtNode> = vec![];
        stmts
    }

    pub fn parse_stmt(&mut self) -> Option<StmtNode> {
        if let Some(now) = self.peek() {
            match now.kind {
                TokenKind::KW_INT => {
                    self.skip(); // skip the keyword 'int'
                    return self.parse_int_decl();
                },
                TokenKind::T_EOF => return None,
                _ => return None,
            }
        }
        None
    }

    fn parse_int_decl(&mut self) -> Option<StmtNode> {
        None
    }

    fn tokenkind_matches_any(&self, kind: TokenKind, kinds: Vec<TokenKind>) -> bool {
        for k in kinds {
            if k == kind { return true; }
        }
        return false;
    }

    fn skip(&mut self) {
        self.current += 1;
    }

    fn peek(&self) -> Option<&Token> {
        if !self.is_at_end() {
            return self.tokens.get(self.current);
        } else {
            return None;
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }
}