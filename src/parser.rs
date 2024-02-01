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

use core::panic;
use std::cell::RefCell;
use std::rc::Rc;

use crate::tokenizer::*;
use crate::symtable::*; 
use crate::enums::*;
use crate::ast::*;

extern crate lazy_static;
use lazy_static::lazy_static;

lazy_static! {
    static ref TOKEN_PRECENDENCE: Vec<u16> = {
        let mut items: Vec<u16> = vec![0; TokenKind::T_NONE as usize];
        items.insert(10, 10); // T_PLUS
        items.insert(11, 10); // T_MINUS
        items.insert(12, 20); // T_STAR
        items.insert(13, 20); // T_SLASH
        items.insert(14, 30); // T_LTEQ
        items.insert(15, 30); // T_NEQ
        items.insert(16, 40); // T_LTEQ
        items.insert(17, 40); // T_GTEP
        items.insert(18, 40); // T_GTHAN
        items.insert(19, 40); // T_LTHAN
        items
    };
}

// Actual parser
pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    current: usize,
    current_token: &'a Token,
    sym_table: Rc<RefCell<Symtable>>, // symbol table for global identifiers
}

impl<'a> Parser<'a> {
    #[inline]
    pub fn new(tokens: &'a Vec<Token>, sym_table: Rc<RefCell<Symtable>>) -> Self {
        Self { tokens, current: 0, current_token: &tokens[0], sym_table }
    }

    pub fn start(&mut self, traverser: &mut ASTTraverser) {
        // first, parse the globals
        self.parse_globals();
        // main code starts from here
        println!(".text\n.global _main\n_main:");
        loop {
            if self.current_token.kind == TokenKind::T_EOF { break; }
            if let Some(stmt) = &self.parse_stmt() {
                traverser.traverse(stmt);
            }
        }
        println!("mov x0, 0\nmov x16, 1\nsvc 0x80");
    }

    fn parse_stmt(&mut self) -> Option<ASTNode> {
        #[allow(unused_assignments)]
        let mut tree: Option<ASTNode> = None;
        let mut left: Option<ASTNode> = None;
        match self.current_token.kind {
            TokenKind::KW_GLOBAL => {
                // ignore globals as they are already parsed
                self.jump_past(TokenKind::T_SEMICOLON);
                tree = None;
            },
            TokenKind::KW_LOCAL => {
                self.parse_local_variable_decl_stmt();
                tree = Some(ASTNode::make_leaf(ASTNodeKind::AST_INTLIT, LitType::Integer(12)));
            },
            TokenKind::T_IDENTIFIER => {
                tree = self.parse_assignment_stmt();
            },
            TokenKind::KW_IF => {
                tree = self.parse_if_stmt();
            },
            TokenKind::T_LBRACE => {
                self.jump_past(TokenKind::T_RBRACE);
                tree = left.clone();
            },
            TokenKind::T_EOF => tree = None,
            _ => panic!("Syntax error: {:?}", self.current_token)
        };
        if let Some(_tree) = &tree {
            if left.is_none() {
                left = tree.clone();
            } else {
                left = Some(ASTNode::new(ASTNodeKind::AST_GLUE, left.unwrap(), tree.clone().unwrap(), LitType::Integer(0)));
            }
        }
        tree
    }

    // This function parses tokens starting from 'KW_INT' to the next 'T_SEMICOLON'.
    fn parse_globals(&mut self) {
        println!(".data");
        loop {
            match self.current_token.kind {
                TokenKind::KW_GLOBAL => self.parse_global_variable_decl_stmt(),
                TokenKind::T_EOF => break,
                _ => self.skip_to_next_token()
            }
        }
        // reinit the parser to its starting state
        self.current_token = &self.tokens[0];
        self.current = 0;
    }

    fn parse_if_stmt(&mut self) -> Option<ASTNode> {
        _ = self.token_match(TokenKind::KW_IF); // match and ignore 'if' keyword
        _ = self.token_match(TokenKind::T_LPAREN); // match and ignore '('
        let cond_ast: Option<ASTNode> = self.parse_equality();
        if let Some(_icast) = &cond_ast {
            if (_icast.operation < ASTNodeKind::AST_EQEQ) || (_icast.operation > ASTNodeKind::AST_LTHAN) { // if operation kind is not "relational operation"
                panic!("'{:?}' is not allowed in if's condition.", _icast.operation);
            }
        }
        _ = self.token_match(TokenKind::T_RPAREN); // match and ignore ')'
        let if_true_ast: Option<ASTNode> = self.parse_stmt();
        let mut if_false_ast: Option<ASTNode> = None;
        if self.current_token.kind == TokenKind::KW_ELSE {
            self.skip_to_next_token(); // skip 'else'
            if_false_ast = self.parse_stmt();
        }
        Some(ASTNode::make_with_mid(ASTNodeKind::AST_IF, cond_ast.unwrap(), if_true_ast.clone().unwrap(), if_true_ast.clone().unwrap(), LitType::Integer(0)))
    }

    fn parse_assignment_stmt(&mut self) -> Option<ASTNode> {
        let id_token: Token = self.token_match(TokenKind::T_IDENTIFIER).clone();
        if self.sym_table.borrow().find(&id_token.lexeme) == 0xFFFFFFFF { // if the symbol has not been defined
            panic!("Assigning to an undefined symbol '{}'", id_token.lexeme);
        }
        _ = self.token_match(TokenKind::T_EQUAL);
        let bin_expr_node: Option<ASTNode> = self.parse_equality();
        _ = self.token_match(TokenKind::T_SEMICOLON);
        let lvalueid: ASTNode = ASTNode::make_leaf(ASTNodeKind::AST_LVIDENT, LitType::String(id_token.lexeme));
        Some(ASTNode::new(ASTNodeKind::AST_ASSIGN, bin_expr_node.unwrap(), lvalueid, LitType::Integer(0)))
    }

    fn parse_global_variable_decl_stmt(&mut self) {
        _ = self.token_match(TokenKind::KW_GLOBAL);
        _ = self.token_match(TokenKind::KW_INT);
        let id_token: Token = self.token_match(TokenKind::T_IDENTIFIER).clone();
        _ = self.token_match(TokenKind::T_SEMICOLON);
        self.sym_table.borrow_mut().add(&id_token.lexeme); // track the symbol that has been defined
        println!("{}: .word 0 // int {};", id_token.lexeme, id_token.lexeme);
    }
    
    fn parse_local_variable_decl_stmt(&mut self) {
        _ = self.token_match(TokenKind::KW_LOCAL);
        _ = self.token_match(TokenKind::KW_INT);
        let _id_token: Token = self.token_match(TokenKind::T_IDENTIFIER).clone();
        _ = self.token_match(TokenKind::T_SEMICOLON);
        // self.sym_table.borrow_mut().add(&id_token.lexeme); // track the symbol that has been defined
        println!("[LOCAL VAR DECL]");
    }

    fn parse_equality(&mut self) -> Option<ASTNode> {
        let left: Option<ASTNode> = self.parse_comparision();
        self.try_parsing_binary(left, vec![TokenKind::T_EQEQ, TokenKind::T_NEQ])
    }

    fn parse_comparision(&mut self) -> Option<ASTNode> {
        let left: Option<ASTNode> = self.parse_addition();
        self.try_parsing_binary(left, vec![TokenKind::T_GTHAN, TokenKind::T_LTHAN, TokenKind::T_GTEQ, TokenKind::T_LTEQ])
    }

    fn parse_addition(&mut self) -> Option<ASTNode> {
        let left: Option<ASTNode> = self.parse_factor();
        self.try_parsing_binary(left, vec![TokenKind::T_PLUS, TokenKind::T_MINUS])
    }

    fn parse_factor(&mut self) -> Option<ASTNode> {
        let left: Option<ASTNode> = self.parse_primary();
        self.try_parsing_binary(left, vec![TokenKind::T_SLASH, TokenKind::T_STAR])
    }

    fn try_parsing_binary(&mut self, left_side_tree: Option<ASTNode>, tokens: Vec<TokenKind>) -> Option<ASTNode> {
        if let Some(left) = left_side_tree.clone() {
            let current_token_kind: TokenKind = self.current_token.kind;
            let mut ok: bool = false;
            for token in tokens {
                if token == current_token_kind {
                    ok = true;
                    break;
                }
            }
            if ok {
                self.skip_to_next_token(); // skip the operator
                if let Some(right) = self.parse_equality() {
                    return Some(ASTNode::new(ASTNodeKind::from_token_kind(current_token_kind), left, right, LitType::Integer(0)));
                } else {
                    panic!("Something unexpected happended with this token: '{:?}'", self.current_token);
                }
            } else { return left_side_tree; }
        }
        left_side_tree
    }

    fn parse_primary(&mut self) -> Option<ASTNode> {
        let current_token: Token = self.current_token.clone();
        self.skip_to_next_token();
        match current_token.kind {
            TokenKind::T_INT_NUM => Some(ASTNode::make_leaf(ASTNodeKind::AST_INTLIT, LitType::Integer(current_token.lexeme.parse::<i32>().unwrap()))),
            TokenKind::T_FLOAT_NUM => Some(ASTNode::make_leaf(ASTNodeKind::AST_INTLIT, LitType::Float(current_token.lexeme.parse::<f32>().unwrap()))),
            TokenKind::T_IDENTIFIER => {
                let id_index: usize = self.sym_table.borrow().find(&current_token.lexeme);
                if id_index == 0xFFFFFFFF { // if symbol has not been defined
                    panic!("Undefined symbol '{}'", current_token.lexeme);
                }
                Some(ASTNode::make_leaf(ASTNodeKind::AST_IDENT, LitType::Integer(id_index as i32)))
            },
            _ => {
                println!("{:?}", current_token);
                panic!("please provide an integer number");
            }
        }
    }

    fn token_match(&mut self, kind: TokenKind) -> &Token {
        let current: &Token = self.current_token;
        if kind != current.kind {
            panic!("Expected the token to be '{:?}' but found '{:?}'", kind, current.kind);
        }
        self.skip_to_next_token();
        current
    }

    fn skip_to_next_token(&mut self) {
        self.current += 1;
        if self.current >= self.tokens.len() { return; }
        self.current_token = &self.tokens[self.current];
    }

    // Jump to the token after the token of type 'kind'.
    fn jump_past(&mut self, kind: TokenKind) {
        while self.current_token.kind != kind {
            self.skip_to_next_token();
        }
        self.skip_to_next_token();
    }
}

#[cfg(test)]
mod tests {
    use super::*; 

    // test depth 1 binary tree
    #[test]
    fn test_depth_one_bin_tree() {
        let mut tokener: Tokenizer = Tokenizer::new("5+5");
        let _tokens: Vec<Token> = tokener.start_scan();
    }
}