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
            if let Some(stmt) = &self.parse_single_stmt() {
                traverser.traverse(stmt);
            }
        }
        println!("mov x0, 0\nmov x16, 1\nsvc 0x80");
    }

    fn parse_single_stmt(&mut self) -> Option<ASTNode> {
         match self.current_token.kind {
            TokenKind::KW_GLOBAL => {
                // ignore globals as they are already parsed
                self.jump_past(TokenKind::T_SEMICOLON);
                None
            },
            TokenKind::KW_LOCAL => {
                self.parse_local_variable_decl_stmt();
                Some(ASTNode::make_leaf(ASTNodeKind::AST_INTLIT, LitType::Integer(12)))
            },
            TokenKind::T_IDENTIFIER => self.parse_assignment_stmt(),
            TokenKind::KW_IF => self.parse_if_stmt(),
            TokenKind::KW_WHILE => self.parse_while_stmt(),
            TokenKind::KW_FOR => self.parse_for_stmt(),
            TokenKind::T_LBRACE => self.parse_compound_stmt(),
            TokenKind::KW_VOID => self.parse_function_stmt(),
            TokenKind::T_EOF => None,
            _ => panic!("Syntax error: {:?}", self.current_token)
        }
    }

    // This function parses tokens starting from 'KW_GLOBAL' to the next 'T_SEMICOLON'.
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

    // parse compound statement(statement starting with '{' and ending with '}')
    fn parse_compound_stmt(&mut self) -> Option<ASTNode> {
        _ = self.token_match(TokenKind::T_LBRACE);
        let mut left: Option<ASTNode> = None;
        loop {
            let tree: Option<ASTNode> = self.parse_single_stmt();
            if tree.is_some() {
                if left.is_none() {
                    left = tree;
                } else {
                    left = Some(ASTNode::new(ASTNodeKind::AST_GLUE, left.clone(), tree.clone(), None));
                }
            }
            if self.current_token.kind == TokenKind::T_RBRACE {
                _ = self.token_match(TokenKind::T_RBRACE); // match and ignore '}'
                break;
            }
        }
        left
    }

    fn parse_function_stmt(&mut self) -> Option<ASTNode> {
        _ = self.token_match(TokenKind::KW_VOID);
        let id_token: Token = self.token_match(TokenKind::T_IDENTIFIER).clone();
        let func_name_index: usize = self.sym_table.borrow_mut().add(id_token.lexeme.as_ref()); // track the symbol that has been defined
        _ = self.token_match(TokenKind::T_LPAREN);
        _ = self.token_match(TokenKind::T_RPAREN);
        let function_body: Option<ASTNode> = self.parse_compound_stmt();
        Some(ASTNode::new(ASTNodeKind::AST_FUNCTION, function_body, None, Some(LitType::Integer(func_name_index as i32))))
    }

    fn parse_while_stmt(&mut self) -> Option<ASTNode> {
        let cond_ast: Option<ASTNode> = self.parse_conditional_stmt(TokenKind::KW_WHILE);
        let while_body: Option<ASTNode> = self.parse_single_stmt();
        Some(ASTNode::new(ASTNodeKind::AST_WHILE, cond_ast, while_body, None))
    }

    fn parse_for_stmt(&mut self) -> Option<ASTNode> {
        _ = self.token_match(TokenKind::KW_FOR); // match and ignore the keyword 'for'
        _ = self.token_match(TokenKind::T_LPAREN); // match and ignore '('
        let pre_stmt: Option<ASTNode> = self.parse_single_stmt(); // initialization statement
        // _ = self.token_match(TokenKind::T_SEMICOLON);
        let cond_ast: Option<ASTNode> = self.parse_equality(); // conditional section of for loop
        if let Some(_icast) = &cond_ast {
            if (_icast.operation < ASTNodeKind::AST_EQEQ) || (_icast.operation > ASTNodeKind::AST_LTHAN) { // if operation kind is not "relational operation"
                panic!("Please provide conditional expression for 'for'");
            }
        }
        _ = self.token_match(TokenKind::T_SEMICOLON); // expect semicolon
        let incr_ast: Option<ASTNode> = self.parse_single_stmt();
        _ = self.token_match(TokenKind::T_RPAREN); // match and ignore ')'
        let for_body: Option<ASTNode> = self.parse_single_stmt();
        let mut tree: ASTNode = ASTNode::new(ASTNodeKind::AST_GLUE, for_body, incr_ast, None);
        tree = ASTNode::new(ASTNodeKind::AST_WHILE, cond_ast, Some(tree), None);
        Some(ASTNode::new(ASTNodeKind::AST_GLUE, pre_stmt, Some(tree), None))
    }

    fn parse_if_stmt(&mut self) -> Option<ASTNode> {
        let cond_ast: Option<ASTNode> = self.parse_conditional_stmt(TokenKind::KW_IF);
        let if_true_ast: Option<ASTNode> = self.parse_single_stmt();
        let mut if_false_ast: Option<ASTNode> = None;
        if self.current_token.kind == TokenKind::KW_ELSE {
            self.skip_to_next_token(); // skip 'else'
            if_false_ast = self.parse_single_stmt();
        }
        Some(ASTNode::with_mid(ASTNodeKind::AST_IF, cond_ast, if_false_ast, if_true_ast.clone(), None))
    }

    // parses tokens that are in the form '(expression [< | > | >= | <= | == | !=] expression)'
    fn parse_conditional_stmt(&mut self, kind: TokenKind) -> Option<ASTNode> {
        _ = self.token_match(kind);
        _ = self.token_match(TokenKind::T_LPAREN); // match and ignore '('
        let cond_ast: Option<ASTNode> = self.parse_equality();
        if let Some(_icast) = &cond_ast {
            if (_icast.operation < ASTNodeKind::AST_EQEQ) || (_icast.operation > ASTNodeKind::AST_LTHAN) { // if operation kind is not "relational operation"
                panic!("'{:?}' is not allowed in {:?}'s condition.", _icast.operation, kind);
            }
        }
        _ = self.token_match(TokenKind::T_RPAREN); // match and ignore ')'
        cond_ast
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
        Some(ASTNode::new(ASTNodeKind::AST_ASSIGN, bin_expr_node, Some(lvalueid), None))
    }

    fn parse_global_variable_decl_stmt(&mut self) {
        _ = self.token_match(TokenKind::KW_GLOBAL);
        _ = self.token_match(TokenKind::KW_INT);
        let id_token: Token = self.token_match(TokenKind::T_IDENTIFIER).clone();
        _ = self.token_match(TokenKind::T_SEMICOLON);
        self.sym_table.borrow_mut().add(&id_token.lexeme); // track the symbol that has been defined
        println!("{}: .space 8 // int {};", id_token.lexeme, id_token.lexeme);
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
                    return Some(ASTNode::new(ASTNodeKind::from_token_kind(current_token_kind), Some(left), Some(right), None));
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