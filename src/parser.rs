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
use crate::types::*;
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
        println!(".text");
        loop {
            if self.current_token.kind == TokenKind::T_EOF { break; }
            if let Some(stmt) = &self.parse_single_stmt() {
                traverser.traverse(stmt);
            }
        }
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
                Some(ASTNode::make_leaf(ASTNodeKind::AST_INTLIT, LitType::I32(12), LitType::I32(0)))
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
                    left = Some(ASTNode::new(ASTNodeKind::AST_GLUE, left.clone(), tree.clone(), None, LitType::None));
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
        let func_return_type_tok: &Token = self.current_token;
        let func_return_type: Option<LitType> = LitType::from_token_kind(func_return_type_tok.kind);
        if let Some(return_type) = func_return_type {
            self.skip_to_next_token(); // skip the return type
            let id_token: Token = self.token_match(TokenKind::T_IDENTIFIER).clone();
            let func_name_index: usize = self.sym_table.borrow_mut().add_symbol(Symbol::new(id_token.lexeme, return_type, SymbolType::Function)); // track the symbol that has been defined
            _ = self.token_match(TokenKind::T_LPAREN);
            _ = self.token_match(TokenKind::T_RPAREN);
            let function_body: Option<ASTNode> = self.parse_compound_stmt();
            Some(ASTNode::new(ASTNodeKind::AST_FUNCTION, function_body, None, Some(LitType::I32(func_name_index as i32)), LitType::None))
        } else {
            panic!("Illegal return type for a function: {:?}", func_return_type);
        }
    }

    fn parse_while_stmt(&mut self) -> Option<ASTNode> {
        let cond_ast: Option<ASTNode> = self.parse_conditional_stmt(TokenKind::KW_WHILE);
        let while_body: Option<ASTNode> = self.parse_single_stmt();
        Some(ASTNode::new(ASTNodeKind::AST_WHILE, cond_ast, while_body, None, LitType::None))
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
        let mut tree: ASTNode = ASTNode::new(ASTNodeKind::AST_GLUE, for_body, incr_ast, None, LitType::None);
        tree = ASTNode::new(ASTNodeKind::AST_WHILE, cond_ast, Some(tree), None, LitType::None);
        Some(ASTNode::new(ASTNodeKind::AST_GLUE, pre_stmt, Some(tree), None, LitType::None))
    }

    fn parse_if_stmt(&mut self) -> Option<ASTNode> {
        let cond_ast: Option<ASTNode> = self.parse_conditional_stmt(TokenKind::KW_IF);
        let if_true_ast: Option<ASTNode> = self.parse_single_stmt();
        let mut if_false_ast: Option<ASTNode> = None;
        if self.current_token.kind == TokenKind::KW_ELSE {
            self.skip_to_next_token(); // skip 'else'
            if_false_ast = self.parse_single_stmt();
        }
        Some(ASTNode::with_mid(ASTNodeKind::AST_IF, cond_ast, if_false_ast, if_true_ast.clone(), None, LitType::None))
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
        if self.sym_table.borrow().find_symbol(&id_token.lexeme) == 0xFFFFFFFF { // if the symbol has not been defined
            panic!("Assigning to an undefined symbol '{}'", id_token.lexeme);
        }
        _ = self.token_match(TokenKind::T_EQUAL);
        let bin_expr_node: Option<ASTNode> = self.parse_equality();
        let _result_type: LitType = bin_expr_node.as_ref().unwrap().result_type.clone();
        _ = self.token_match(TokenKind::T_SEMICOLON);
        let lvalueid: ASTNode = ASTNode::make_leaf(ASTNodeKind::AST_LVIDENT, LitType::String(id_token.lexeme), LitType::String(String::from("")));
        Some(ASTNode::new(ASTNodeKind::AST_ASSIGN, bin_expr_node, Some(lvalueid), None, _result_type))
    }

    fn parse_global_variable_decl_stmt(&mut self) {
        _ = self.token_match(TokenKind::KW_GLOBAL);
        // let mut sym: MaybeUninit<Symbol> = MaybeUninit::<Symbol>::uninit();
        let mut sym: Symbol = Symbol::new(String::from(""), LitType::I32(0), SymbolType::Variable);
        let mut data_size: i32 = 4;
        match self.current_token.kind {
            TokenKind::KW_INT => (),
            | TokenKind::KW_CHAR => {
                data_size = 2;
                sym.lit_type = LitType::U8(0);
            },
            _ => panic!("Can't create variable of type {:?}", self.current_token.kind)
        }
        self.skip_to_next_token();
        let id_token: Token = self.token_match(TokenKind::T_IDENTIFIER).clone();
        sym.name = id_token.lexeme.clone();
        _ = self.token_match(TokenKind::T_SEMICOLON);
        self.sym_table.borrow_mut().add_symbol(sym); // track the symbol that has been defined
        println!("{}: .space {} // int {};", id_token.lexeme, data_size, id_token.lexeme);
    }
    
    fn parse_local_variable_decl_stmt(&mut self) {
        _ = self.token_match(TokenKind::KW_LOCAL);
        _ = self.token_match(TokenKind::KW_INT);
        let _id_token: Token = self.token_match(TokenKind::T_IDENTIFIER).clone();
        _ = self.token_match(TokenKind::T_SEMICOLON);
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
        let left: Option<ASTNode> = self.parse_func_call_expr();
        self.try_parsing_binary(left, vec![TokenKind::T_SLASH, TokenKind::T_STAR])
    }

    fn parse_func_call_expr(&mut self) -> Option<ASTNode> {
        let possible_id_node: Option<ASTNode> = self.parse_primary(); // could be other type of node than identifier node
        if self.current_token.kind == TokenKind::T_LPAREN {
            self.token_match(TokenKind::T_LPAREN); // match and ignore '('
            let (symbol_index, result_type): (usize, LitType) = {
                let id_type: LitType = possible_id_node.clone().unwrap().value.unwrap();
                match id_type {
                    LitType::I32(id_idx) => {
                        if id_idx as usize == 0xFFFFFFFF {
                            panic!("Undefined symbol!");
                        }
                        let symbol: Symbol = self.sym_table.borrow().get_symbol(id_idx as usize).clone();
                        (id_idx, symbol.lit_type.clone())
                    },
                    _ => panic!("{:?}", id_type)
                };
                (0, LitType::None)
            };
            self.token_match(TokenKind::T_RPAREN); // match and ignore ')'
            Some(ASTNode::make_leaf(ASTNodeKind::AST_FUNC_CALL, LitType::I32(symbol_index as i32), result_type))
        } else {
            possible_id_node
        }
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
                    // check type compatibility
                    let compat_result: (bool, i32, i32) = left.value.as_ref().unwrap().compatible(right.value.as_ref().unwrap());
                    let (_left, _right, _rtype): (ASTNode, ASTNode, LitType) = match compat_result {
                        (false, _, _) => panic!("Incompatible types: {:?} and {:?}", left.value.as_ref(), right.value.as_ref()),
                        // `left` has to be promoted
                        (true, 1, 0) => (ASTNode::make_leaf(left.operation, left.value.unwrap().convert(right.value.clone().unwrap()), left.result_type.clone()), right, left.result_type),
                        // `right` has to be promoted
                        (true, 0, 1) => (left.clone(), ASTNode::make_leaf(right.operation, right.value.unwrap().convert(left.value.clone().unwrap()), right.result_type.clone()), right.result_type),
                        _ => {
                            let rt: LitType =  left.result_type.clone();
                            (left.clone(), right, rt)
                        }
                    };
                    return Some(ASTNode::new(ASTNodeKind::from_token_kind(current_token_kind), Some(_left), Some(_right), None, _rtype));
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
            TokenKind::T_INT_NUM => Some(ASTNode::make_leaf(ASTNodeKind::AST_INTLIT, LitType::I32(current_token.lexeme.parse::<i32>().unwrap()), LitType::I32(0))),
            TokenKind::T_FLOAT_NUM | TokenKind::T_DOUBLE_NUM => Some(ASTNode::make_leaf(ASTNodeKind::AST_INTLIT, LitType::F32(current_token.lexeme.parse::<f32>().unwrap()), LitType::F32(0.0))),
            TokenKind::T_CHAR => Some(ASTNode::make_leaf(ASTNodeKind::AST_INTLIT, LitType::U8(current_token.lexeme.parse::<u8>().unwrap()), LitType::U8(0))),
            TokenKind::T_IDENTIFIER => {
                let id_index: usize = self.sym_table.borrow().find_symbol(&current_token.lexeme);
                if id_index == 0xFFFFFFFF { // if symbol has not been defined
                    panic!("Undefined symbol '{}'", current_token.lexeme);
                }
                let symbol: Symbol = self.sym_table.borrow().get_symbol(id_index).clone();
                Some(ASTNode::make_leaf(ASTNodeKind::AST_IDENT, LitType::I32(id_index as i32), symbol.lit_type))
            },
            _ => {
                println!("{:?}", current_token);
                panic!("Unrecognized primitive type: {:?}", self.current_token);
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

#[allow(clippy::redundant_pattern_matching)]
#[cfg(test)]
mod tests {
    use super::*; 

    // test addition operation
    #[test]
    fn test_depth_one_bin_tree() {
        let mut tokener: Tokenizer = Tokenizer::new("5+5");
        let tokens: Vec<Token> = tokener.start_scan();
        let sym_table: Rc<RefCell<Symtable>> = Rc::new(RefCell::new(Symtable::new()));
        let mut p: Parser = Parser::new(&tokens, Rc::clone(&sym_table));
        let result: Option<ASTNode> = p.parse_equality();
        matches!(result, Some(_));
        assert_eq!(result.unwrap().operation, ASTNodeKind::AST_ADD);
    }
}