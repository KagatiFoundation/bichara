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
        let mut nodes: Vec<ASTNode> = vec![];
        loop {
            if self.current_token.kind == TokenKind::T_EOF { break; }
            if let Some(stmt) = self.parse_single_stmt() {
                nodes.push(stmt);
            }
        }
        self.dump_globals();
        // .text section starts from here
        println!("\n.text");
        for node in &nodes {
            traverser.traverse(node);
        }
        println!("mov x0, 0\nmov x16, 1\nsvc 0x80");
    }

    fn dump_globals(&self) {
        println!(".data\n\t.align 2\n.L2:");
        for symbol in self.sym_table.borrow().iter() {
            if symbol.sym_type == SymbolType::Variable {
                println!("\t.word 0");
            }
        }
    }

    fn parse_single_stmt(&mut self) -> Option<ASTNode> {
         match self.current_token.kind {
            TokenKind::KW_GLOBAL => {
                self.parse_global_variable_decl_stmt();
                None
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
                    left = Some(ASTNode::new(ASTNodeKind::AST_GLUE, left.clone(), tree.clone(), None, LitTypeVariant::None));
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
            let func_name_index: usize = self.sym_table.borrow_mut().add_symbol(Symbol::new(id_token.lexeme, return_type.variant(), SymbolType::Function)); // track the symbol that has been defined
            _ = self.token_match(TokenKind::T_LPAREN);
            _ = self.token_match(TokenKind::T_RPAREN);
            let function_body: Option<ASTNode> = self.parse_compound_stmt();
            Some(ASTNode::new(ASTNodeKind::AST_FUNCTION, function_body, None, Some(LitType::I32(func_name_index as i32)), LitTypeVariant::None))
        } else {
            panic!("Illegal return type for a function: {:?}", func_return_type);
        }
    }

    fn parse_while_stmt(&mut self) -> Option<ASTNode> {
        let cond_ast: Option<ASTNode> = self.parse_conditional_stmt(TokenKind::KW_WHILE);
        let while_body: Option<ASTNode> = self.parse_single_stmt();
        Some(ASTNode::new(ASTNodeKind::AST_WHILE, cond_ast, while_body, None, LitTypeVariant::None))
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
        let mut tree: ASTNode = ASTNode::new(ASTNodeKind::AST_GLUE, for_body, incr_ast, None, LitTypeVariant::None);
        tree = ASTNode::new(ASTNodeKind::AST_WHILE, cond_ast, Some(tree), None, LitTypeVariant::None);
        Some(ASTNode::new(ASTNodeKind::AST_GLUE, pre_stmt, Some(tree), None, LitTypeVariant::None))
    }

    fn parse_if_stmt(&mut self) -> Option<ASTNode> {
        let cond_ast: Option<ASTNode> = self.parse_conditional_stmt(TokenKind::KW_IF);
        let if_true_ast: Option<ASTNode> = self.parse_single_stmt();
        let mut if_false_ast: Option<ASTNode> = None;
        if self.current_token.kind == TokenKind::KW_ELSE {
            self.skip_to_next_token(); // skip 'else'
            if_false_ast = self.parse_single_stmt();
        }
        Some(ASTNode::with_mid(ASTNodeKind::AST_IF, cond_ast, if_false_ast, if_true_ast.clone(), None, LitTypeVariant::None))
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
        let _id_index_symt: usize = self.sym_table.borrow().find_symbol(&id_token.lexeme);
        let symbol: Symbol = self.sym_table.borrow().get_symbol(_id_index_symt).clone();
        if _id_index_symt == 0xFFFFFFFF { // if the symbol has not been defined
            panic!("Assigning to an undefined symbol '{}'", id_token.lexeme);
        }
        // Check if we are assigning to a type other than SymbolType::Variable. If yes, panic!
        if symbol.sym_type != SymbolType::Variable {
            panic!("Assigning to type '{:?}' is not allowed!", symbol.sym_type);
        }
        _ = self.token_match(TokenKind::T_EQUAL);
        let bin_expr_node: Option<ASTNode> = self.parse_equality();
        let bin_expr_res_type: LitTypeVariant = bin_expr_node.as_ref().unwrap().result_type;
        // Test if expression type matches the variable type.
        if bin_expr_res_type != symbol.lit_type && !Parser::is_assignment_compatible(symbol.lit_type, bin_expr_node.as_ref().unwrap()) {
            panic!("Expression result does not match variable type! {:?} = {:?}???", symbol, bin_expr_res_type);
        }
        let _result_type: LitTypeVariant = bin_expr_node.as_ref().unwrap().result_type;
        _ = self.token_match(TokenKind::T_SEMICOLON);
        let lvalueid: ASTNode = ASTNode::make_leaf(ASTNodeKind::AST_LVIDENT, LitType::I32(_id_index_symt as i32), symbol.lit_type);
        Some(ASTNode::new(ASTNodeKind::AST_ASSIGN, bin_expr_node, Some(lvalueid), None, _result_type))
    }

    fn is_assignment_compatible(var_type: LitTypeVariant, node: &ASTNode) -> bool {
        match var_type {
            LitTypeVariant::I32 => {
                match node.result_type {
                    LitTypeVariant::I32 => true,
                    LitTypeVariant::U8 => node.operation != ASTNodeKind::AST_IDENT,
                    _ => false
                }
            },
            _ => false,
        }
    }

    fn parse_global_variable_decl_stmt(&mut self) {
        _ = self.token_match(TokenKind::KW_GLOBAL);
        // let mut sym: MaybeUninit<Symbol> = MaybeUninit::<Symbol>::uninit();
        let mut sym: Symbol = Symbol::new(String::from(""), LitTypeVariant::I32, SymbolType::Variable);
        sym.lit_type = match self.current_token.kind {
            TokenKind::KW_INT => LitTypeVariant::I32,
            | TokenKind::KW_CHAR => LitTypeVariant::U8,
            TokenKind::KW_LONG => LitTypeVariant::I64,
            _ => panic!("Can't create variable of type {:?}", self.current_token.kind)
        };
        self.skip_to_next_token();
        let id_token: Token = self.token_match(TokenKind::T_IDENTIFIER).clone();
        sym.name = id_token.lexeme.clone();
        _ = self.token_match(TokenKind::T_SEMICOLON);
        self.sym_table.borrow_mut().add_symbol(sym); // track the symbol that has been defined
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
            let (symbol_index, result_type): (usize, LitTypeVariant) = {
                let id_type: LitType = possible_id_node.clone().unwrap().value.unwrap();
                match id_type {
                    LitType::I32(id_idx) => {
                        let symbol: Symbol = self.sym_table.borrow().get_symbol(id_idx as usize).clone();
                        (id_idx as usize, symbol.lit_type)
                    },
                    _ => panic!("parse_func_call_expr: {:?}", id_type)
                }
            };
            self.token_match(TokenKind::T_RPAREN); // match and ignore ')'
            Some(ASTNode::make_leaf(ASTNodeKind::AST_FUNC_CALL, LitType::I32(symbol_index as i32), result_type))
        } else {
            possible_id_node
        }
    }

    fn try_parsing_binary(&mut self, left_side_tree: Option<ASTNode>, tokens: Vec<TokenKind>) -> Option<ASTNode> {
        if let Some(ref mut left) = left_side_tree.clone() {
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
                if let Some(ref mut right) = self.parse_equality() {
                    // check type compatibility
                    if left.result_type != right.result_type {
                        let compatibility: (bool, u8, u8) = left.value.as_ref().unwrap().compatible(right.value.as_ref().unwrap());
                        match compatibility {
                            (true, 0, 1) => right.result_type = left.result_type,
                            (true, 1, 0) => left.result_type = right.result_type,
                            _ => panic!("Incompatible types: '{:?}' and '{:?}'", left.result_type, right.result_type)
                        }
                    }
                    let result_type: LitTypeVariant = left.result_type;
                    return Some(ASTNode::new(ASTNodeKind::from_token_kind(current_token_kind), Some(left.clone()), Some(right.clone()), None, result_type));
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
            TokenKind::T_INT_NUM => Some(ASTNode::make_leaf(ASTNodeKind::AST_INTLIT, LitType::I32(current_token.lexeme.parse::<i32>().unwrap()), LitTypeVariant::I32)),
            TokenKind::T_CHAR => Some(ASTNode::make_leaf(ASTNodeKind::AST_INTLIT, LitType::U8(current_token.lexeme.parse::<u8>().unwrap()), LitTypeVariant::U8)),
            TokenKind::T_LONG_NUM => Some(ASTNode::make_leaf(ASTNodeKind::AST_INTLIT, LitType::I64(current_token.lexeme.parse::<i64>().unwrap()), LitTypeVariant::I64)),
            TokenKind::T_FLOAT_NUM 
            | TokenKind::T_DOUBLE_NUM => Some(ASTNode::make_leaf(ASTNodeKind::AST_INTLIT, LitType::F32(current_token.lexeme.parse::<f32>().unwrap()), LitTypeVariant::F32)),
            TokenKind::T_IDENTIFIER => {
                let id_index: usize = self.sym_table.borrow().find_symbol(&current_token.lexeme);
                if id_index == 0xFFFFFFFF { // if symbol has not been defined
                    panic!("Undefined symbol '{}'", current_token.lexeme);
                }
                let symbol: Symbol = self.sym_table.borrow().get_symbol(id_index).clone();
                Some(ASTNode::make_leaf(ASTNodeKind::AST_IDENT, LitType::I32(id_index as i32), symbol.lit_type))
            },
            TokenKind::T_LPAREN => { // group expression: e.g: (a * (b + c)))
                let group_expr: Option<ASTNode> = self.parse_equality();
                // Group expression terminates with ')'. Match and ignore ')'.
                self.token_match(TokenKind::T_RPAREN); 
                group_expr
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
}

#[cfg(test)]
mod tests {
    use super::*; 

    #[test]
    fn test_group_expression_tree_structure() {
        let mut tokener: Tokenizer = Tokenizer::new("(5 + (3 * 4))");
        let tokens: Vec<Token> = tokener.start_scan();
        let sym_table: Rc<RefCell<Symtable>> = Rc::new(RefCell::new(Symtable::new()));
        let mut p: Parser = Parser::new(&tokens, Rc::clone(&sym_table));
        let result: Option<ASTNode> = p.parse_equality();
        assert!(result.is_some());
        let upvalue: ASTNode = result.unwrap();
        let left_tree: &ASTNode = (*upvalue.left).as_ref().unwrap();
        let right_tree: &ASTNode = (*upvalue.right).as_ref().unwrap();
        assert_eq!(upvalue.operation, ASTNodeKind::AST_ADD);
        assert_eq!(left_tree.operation, ASTNodeKind::AST_INTLIT);
        assert_eq!(right_tree.operation, ASTNodeKind::AST_MULTIPLY);
    }

    // test addition operation
    #[test]
    fn test_depth_one_bin_tree() {
        let mut tokener: Tokenizer = Tokenizer::new("5+5");
        let tokens: Vec<Token> = tokener.start_scan();
        let sym_table: Rc<RefCell<Symtable>> = Rc::new(RefCell::new(Symtable::new()));
        let mut p: Parser = Parser::new(&tokens, Rc::clone(&sym_table));
        let result: Option<ASTNode> = p.parse_equality();
        assert!(result.is_some());
        assert_eq!(result.unwrap().operation, ASTNodeKind::AST_ADD);
    }

    // test if-else block
    #[test]
    fn test_if_else_statement_block() {
        let mut tokener: Tokenizer = Tokenizer::new("if (4 > 5) { global int a; } else { global int b; }");
        let tokens: Vec<Token> = tokener.start_scan();
        let sym_table: Rc<RefCell<Symtable>> = Rc::new(RefCell::new(Symtable::new()));
        let mut p: Parser = Parser::new(&tokens, Rc::clone(&sym_table));
        let result: Option<ASTNode> = p.parse_if_stmt();
        assert!(result.is_some(), "If this assertion did not pass, then if-else block is probably malformed.");
        let upvalue: &ASTNode = result.as_ref().unwrap();
        // Global variable declaration statements produce None as result. 
        // So, both 'mid (if)' and 'right (else)' has to be None types
        assert!(upvalue.mid.is_none(), "global declarations inside comppound statement should produce None result");
        assert!(upvalue.right.is_none(), "global declarations inside comppound statement should produce None result");
        assert_eq!(upvalue.operation, ASTNodeKind::AST_IF); // main AST node is of AST_IF type
        assert_eq!((*upvalue.left).as_ref().unwrap().operation, ASTNodeKind::AST_GTHAN, "Unexpected ASTNodeKind; expected be ASTNodeKind::AST_GTHAN.");
    }

    #[test]
    fn test_while_statement_block() {
        
    }
}