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

use std::cell::RefCell;
use std::rc::Rc;

use crate::register;
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
        items
    };
}

// Actual parser
pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    current: usize,
    current_token: &'a Token,
    sym_table: Rc<RefCell<Symtable>>, // symbol table for global identifiers
    ast_traverser: ASTTraverser,
    reg_manager: Rc<RefCell<register::RegisterManager>>
}

impl<'a> Parser<'a> {
    #[inline]
    pub fn new(tokens: &'a Vec<Token>, reg_manager: Rc<RefCell<register::RegisterManager>>, sym_table: Rc<RefCell<Symtable>>) -> Self {

        let ast_traverser: ASTTraverser = ASTTraverser::new(Rc::clone(&reg_manager), Rc::clone(&sym_table));
        Self { tokens, current: 0, current_token: &tokens[0], sym_table, ast_traverser, reg_manager }
    }

    pub fn parse_stmts(&mut self) {
        // first, parse the globals
        self.parse_globals();
        // main code starts from here
        println!(".text\n.global _main\n_main:");
        loop {
            match self.current_token.kind {
                TokenKind::KW_INT => self.jump_past(TokenKind::T_SEMICOLON),
                TokenKind::T_IDENTIFIER => self.parse_assignment_stmt(),
                TokenKind::T_EOF => break,
                _ => self.skip_to_next_token()
            }
        }
    }

    // This function parses tokens starting from 'KW_INT' to the next 'T_SEMICOLON'.
    fn parse_globals(&mut self) {
        println!(".data");
        loop {
            match self.current_token.kind {
                TokenKind::KW_INT => self.parse_variable_decl_stmt(),
                TokenKind::T_EOF => break,
                _ => self.skip_to_next_token()
            }
        }
        // reinit the parser to its starting state
        self.current_token = &self.tokens[0];
        self.current = 0;
    }

    fn parse_assignment_stmt(&mut self) {
        let id_token: Token = self.token_match(TokenKind::T_IDENTIFIER).clone();
        if self.sym_table.borrow().find(&id_token.lexeme) == 0xFFFFFFFF { // if the symbol has not been defined
            panic!("Assigning to an undefined symbol '{}'", id_token.lexeme);
        }
        _ = self.token_match(TokenKind::T_EQUAL);
        let mut reg_index: usize = 0;
        let mut addr_reg: usize = 0;
        if let Some(expr_node) = self.parse_binary(0) {
            _ = self.token_match(TokenKind::T_SEMICOLON);
            reg_index = self.ast_traverser.traverse(&expr_node); 
            addr_reg = self.reg_manager.borrow_mut().allocate();
        }
        println!("ldr {}, ={}", self.reg_manager.borrow().name(addr_reg), id_token.lexeme);
        println!("str {}, [{}]", self.reg_manager.borrow().name(reg_index), self.reg_manager.borrow().name(addr_reg));
        self.reg_manager.borrow_mut().deallocate(reg_index);
        self.reg_manager.borrow_mut().deallocate(addr_reg);
    }

    fn parse_variable_decl_stmt(&mut self) {
        _ = self.token_match(TokenKind::KW_INT);
        let id_token: Token = self.token_match(TokenKind::T_IDENTIFIER).clone();
        _ = self.token_match(TokenKind::T_SEMICOLON);
        self.sym_table.borrow_mut().add(&id_token.lexeme); // track the symbol that has been defined
        println!("{}: .word 0 // int {};", id_token.lexeme, id_token.lexeme);
    }

    // ptp -> previous token's precedence
    pub fn parse_binary(&mut self, ptp: u16) -> Option<ASTNode> {
        let mut left: Option<ASTNode> = self.parse_primary();
        let current_token_kind: TokenKind = self.current_token.kind;
        let current_token_prec: u16 = TOKEN_PRECENDENCE[current_token_kind as usize];
        if current_token_kind == TokenKind::T_SEMICOLON {
            return left;
        }
        loop {
            if current_token_prec < ptp { break; }
            self.skip_to_next_token();
            let right: Option<ASTNode> = self.parse_binary(current_token_prec);
            left = Some(ASTNode::new(Parser::as_ast_operation(current_token_kind), left.unwrap(), right.unwrap(), LitType::Integer(0)));
            if self.current_token.kind == TokenKind::T_SEMICOLON {
                return left;
            }
        }
        left
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

    fn as_ast_operation(kind: TokenKind) -> ASTNodeKind {
        match kind {
            TokenKind::T_PLUS => ASTNodeKind::AST_ADD,
            TokenKind::T_MINUS => ASTNodeKind::AST_SUBTRACT,
            TokenKind::T_STAR => ASTNodeKind::AST_MULTIPLY,
            TokenKind::T_SLASH => ASTNodeKind::AST_DIVIDE,
            _ => panic!("Please provide an arithmetic operator. {:?} is not an arithmetic operator", kind)
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