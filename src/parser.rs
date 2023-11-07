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

pub trait Expr {}

#[derive(Clone, Debug)]
pub struct LitExpr {
    value: LitType
}

#[derive(Clone, Debug)]
pub struct BinExpr {
    left: Box<ExprNode>,
    right: Box<ExprNode>,
    op: String,
}

impl Expr for LitExpr{}

impl Expr for BinExpr{}

#[derive(Clone, Debug)]
pub enum ExprNode {
    LiteralExpression(LitExpr),   
    BinaryExpression(BinExpr), 
}

pub trait Stmt{}

pub struct VarDeclStmt {
    name: String,
    value: Box<dyn Expr>,
}

enum Declaration {
    VarDeclaration,
    FunctionDeclaration
}

pub struct StmtNode {
    decl: Declaration
}

pub enum ASTNode {
    ExpressionNode(ExprNode),
    StatementNode(StmtNode)
}

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedToken,
    ParenNotClosed,
    BraceNotClosed,
    UnexpectedEOF
}

#[derive(Debug)]
pub enum ParseResult {
    Success(ExprNode),
    Error(ErrorKind, Option<Token>), // usize for token index
}

// Actual parser
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    #[inline]
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
        }
    }

    pub fn start_parser(&mut self) -> Vec<ExprNode> {
        let mut exprs: Vec<ExprNode> = vec![];
        while !self.is_at_end() {
            match self.parse_expr() {
                ParseResult::Success(expr_node) => {
                    exprs.push(expr_node);
                },
                ParseResult::Error(_, _) => todo!(),
            }
        }
        exprs
    }

    pub fn parse_expr(&mut self) -> ParseResult {
        if self.is_at_end() { ParseResult::Error(ErrorKind::UnexpectedEOF, self.peek()) } 
        else { self.parse_addition() }
    }

    pub fn parse_addition(&mut self) -> ParseResult {
        let left_side_res: ParseResult = self.parse_primary();
        match left_side_res {
            ParseResult::Success(ref left_side_expr) => {
                if let Some(top) = self.peek() {
                    if top.kind == TokenKind::T_PLUS || top.kind == TokenKind::T_MINUS {
                        let operator: String = top.lexeme.clone();
                        self.current += 1; // skip the operator
                        let right_side_result: ParseResult = self.parse_addition();
                        match right_side_result {
                            ParseResult::Success(right_side_expr) => {
                                return ParseResult::Success(
                                    ExprNode::BinaryExpression(
                                        BinExpr { 
                                            left: Box::new(left_side_expr.clone()), 
                                            right: Box::new(right_side_expr), 
                                            op: operator, 
                                    })
                                );
                            },
                            ParseResult::Error(_, _) => return right_side_result,
                        }
                    }
                }
            },
            ParseResult::Error(_, _) => return left_side_res,
        }
        left_side_res
    }

    pub fn parse_primary(&mut self) -> ParseResult {
        if let Some(top) = self.peek() {
            self.skip(); // skip primary token
            match top.kind {
                TokenKind::T_INT_NUM => {
                    let int_val: i32 = top.lexeme.parse().unwrap();
                    ParseResult::Success(self.create_literal_expr(LitType::Integer(int_val)))
                },
                TokenKind::T_FLOAT_NUM => {
                    let float_val: f32 = top.lexeme.parse().unwrap();
                    ParseResult::Success(self.create_literal_expr(LitType::Float(float_val)))
                }
                _ => ParseResult::Error(ErrorKind::UnexpectedToken, Some(top)),
            }
        } else {ParseResult::Error(ErrorKind::UnexpectedEOF, None)}
    }

    #[inline]
    pub fn create_literal_expr(&self, val: LitType) -> ExprNode {
        ExprNode::LiteralExpression(LitExpr { value: val })
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

    #[test]
    fn test_should_create_bin_expr() {
        let mut t: Tokenizer = Tokenizer::new("4 + 5"); 
        let toks: Vec<Token> = t.start_scan();
        let mut p: Parser = Parser::new(toks);
        let e: ParseResult = p.parse_expr();
        assert!(matches!(e, ParseResult::Success(ExprNode::BinaryExpression(_))));
    }

    #[test]
    fn test_should_result_in_invalid_bin_expr() {
        let mut t: Tokenizer = Tokenizer::new("4 + 5 +"); 
        let toks: Vec<Token> = t.start_scan();
        let mut p: Parser = Parser::new(toks);
        let e: ParseResult = p.parse_expr();
        assert!(matches!(e, ParseResult::Error(ErrorKind::UnexpectedEOF, None)));
    }
    
    #[test]
    fn test_should_result_in_invalid_bin_expr2() {
        let mut t: Tokenizer = Tokenizer::new("+ 2 + 4"); 
        let toks: Vec<Token> = t.start_scan();
        let mut p: Parser = Parser::new(toks);
        let e: ParseResult = p.parse_expr();
        assert!(matches!(e, ParseResult::Error(ErrorKind::UnexpectedToken, Some(
            Token{kind: TokenKind::T_PLUS, lexeme: _, pos: TokenPos{line: 1, column: 1}}
        ))));
    }
    
    #[test]
    fn test_should_result_in_invalid_bin_expr3() {
        let mut t: Tokenizer = Tokenizer::new("4 + 5 + + 4"); 
        let toks: Vec<Token> = t.start_scan();
        let mut p: Parser = Parser::new(toks);
        let e: ParseResult = p.parse_expr();
        assert!(matches!(e, ParseResult::Error(ErrorKind::UnexpectedToken, Some(
            Token { kind: TokenKind::T_PLUS, lexeme: _, pos: TokenPos { line: 1, column: 9 } }
        ))));
    }
}