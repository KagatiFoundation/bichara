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
use std::process::exit;
use std::rc::Rc;

use crate::error;
use crate::tokenizer::*;
use crate::symtable::*; 
use crate::enums::*;
use crate::types::*;
use crate::ast::*;


#[derive(Eq, PartialEq, Debug)]
enum ParseError {
    UnexpectedToken(Token), // unexpected token was encountered
    SymbolNotFound(Token), // symbol not found; symbol has not been defined before
    NotCallable(Token), // if a token being called is not callable type
    None, // just a placeholder
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::NotCallable(_) => write!(f, "not callable"),
            _ => write!(f, "other")
        }
    }
}

type ParseResult = Result<ASTNode, ParseError>;

// Actual parser
pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    current: usize,
    current_token: &'a Token,
    sym_table: Rc<RefCell<Symtable>>, // symbol table for global identifiers
    // ID of a function that is presently being parsed. This field's value is 0xFFFFFFFF 
    // if the parser is not inside a function.
    current_function_id: usize, 
}

impl<'a> Parser<'a> {
    #[inline]
    pub fn new(tokens: &'a Vec<Token>, sym_table: Rc<RefCell<Symtable>>) -> Self {
        Self { tokens, current: 0, current_token: &tokens[0], sym_table, current_function_id: 0xFFFFFFFF }
    }

    pub fn start(&mut self, traverser: &mut ASTTraverser) {
        let mut nodes: Vec<ASTNode> = vec![];
        loop {
            if self.current_token.kind == TokenKind::T_EOF { break; }
            if let Ok(stmt) = self.parse_single_stmt() {
                nodes.push(stmt);
            }
        }
        self.dump_globals();
        // .text section starts from here
        println!("\n.text");
        for node in &nodes {
            traverser.traverse(node, self.current_function_id);
        }
        println!("mov x0, 0\nmov x16, 1\nsvc 0x80");
    }

    fn dump_globals(&self) {
        println!(".data\n\t.align 2\n.L2:");
        for symbol in self.sym_table.borrow().iter() {
            if symbol.sym_type == SymbolType::Variable {
                match symbol.lit_type {
                    LitTypeVariant::F32Ptr 
                    | LitTypeVariant::F64Ptr 
                    | LitTypeVariant::I16Ptr
                    | LitTypeVariant::I32Ptr 
                    | LitTypeVariant::I64Ptr
                    | LitTypeVariant::U8Ptr 
                    | LitTypeVariant::VoidPtr => println!("\t.space 8"),
                    _ => println!("\t.word 0")
                }
            }
        }
    }

    fn parse_single_stmt(&mut self) -> ParseResult {
         match self.current_token.kind {
            TokenKind::KW_GLOBAL => {
                self.parse_global_variable_decl_stmt();
                Err(ParseError::UnexpectedToken(self.current_token.clone()))
            },
            TokenKind::T_IDENTIFIER => self.parse_assignment_stmt(),
            TokenKind::KW_IF => self.parse_if_stmt(),
            TokenKind::KW_WHILE => self.parse_while_stmt(),
            TokenKind::KW_FOR => self.parse_for_stmt(),
            TokenKind::T_LBRACE => self.parse_compound_stmt(),
            TokenKind::KW_DEF => self.parse_function_stmt(),
            TokenKind::KW_RETURN => self.parse_return_stmt(),
            TokenKind::T_EOF => Err(ParseError::UnexpectedToken(self.current_token.clone())),
            _ => panic!("Syntax error: {:?}", self.current_token)
        }
    }

    // parse compound statement(statement starting with '{' and ending with '}')
    fn parse_compound_stmt(&mut self) -> ParseResult {
        _ = self.token_match(TokenKind::T_LBRACE);
        let mut left: Option<ASTNode> = None;
        loop {
            let tree: ParseResult = self.parse_single_stmt();
            if tree.is_ok() {
                if left.is_none() {
                    left = tree.ok();
                } else {
                    left = Some(ASTNode::new(ASTNodeKind::AST_GLUE, left.clone(), tree.ok(), None, LitTypeVariant::None));
                }
            }
            if self.current_token.kind == TokenKind::T_RBRACE {
                _ = self.token_match(TokenKind::T_RBRACE); // match and ignore '}'
                break;
            }
        }
        if let Some(node) = left {
            Ok(node)
        } else {
            Err(ParseError::UnexpectedToken(self.current_token.clone()))
        }
    }

    fn parse_function_stmt(&mut self) -> ParseResult {
        _ = self.token_match(TokenKind::KW_DEF); // match and ignore function declaration keyword 'def'
        let id_token: Token = self.token_match(TokenKind::T_IDENTIFIER).clone();
        _ = self.token_match(TokenKind::T_LPAREN);
        _ = self.token_match(TokenKind::T_RPAREN);
        _ = self.token_match(TokenKind::T_ARROW); // match and ignore '->' operator
        let curr_tok_kind: TokenKind = self.current_token.kind;
        let func_return_type: LitTypeVariant = LitTypeVariant::from_token_kind(curr_tok_kind);
        if func_return_type == LitTypeVariant::None {
            error::report_unexpected_token(self.current_token, Some("Not a valid return type for a function."));
            exit(1);
        }
        self.skip_to_next_token(); // skip return type
        self.current_function_id = self.sym_table.borrow_mut().add_symbol(Symbol::new(id_token.lexeme, func_return_type, SymbolType::Function));
        let function_body: ParseResult = self.parse_compound_stmt();
        let temp_func_id: usize = self.current_function_id;
        self.current_function_id = 0xFFFFFFFF; // exiting out of function body
        Ok(ASTNode::new(ASTNodeKind::AST_FUNCTION, function_body.ok(), None, Some(LitType::I32(temp_func_id as i32)), func_return_type))
    }

    #[allow(unused_mut)]
    #[allow(unused_assignments)]
    fn parse_return_stmt(&mut self) -> ParseResult {
        let mut void_ret_type: bool = false;
        let mut func_symbol: Symbol; 
        // check whether parser's parsing a function or not
        if self.current_function_id == 0xFFFFFFFF {
            error::report_unexpected_token(self.current_token, Some("'return' statement outside a function is not valid."));
            exit(1); // NOTE: do error recovery; do not exit
        } else {
            func_symbol = self.sym_table.borrow().get_symbol(self.current_function_id).clone();
            void_ret_type = func_symbol.lit_type == LitTypeVariant::Void;
        }
        _ = self.token_match(TokenKind::KW_RETURN);
        if void_ret_type { // if function has void as a return type, panic if any expression follows the keyword 'return'
            if self.current_token.kind != TokenKind::T_SEMICOLON {
                error::report_unexpected_token(self.current_token, Some("Expected ';' because function has a 'void' return type."));
                exit(2); // NOTE: do error recovery; do not exit
            }
            // skip semicolon
            self.token_match(TokenKind::T_SEMICOLON);
            return Ok(ASTNode::make_leaf(ASTNodeKind::AST_RETURN, LitType::Void, LitTypeVariant::Void));
        } 
        let return_expr: ParseResult = self.parse_equality();
        let return_expr_type: LitTypeVariant = return_expr.as_ref().unwrap().result_type;
        if return_expr_type != func_symbol.lit_type {
            panic!("Return value's type does not match function's return type.");
        }
        _ = self.token_match(TokenKind::T_SEMICOLON); // expect semicolon to end a return statement
        Ok(ASTNode::new(ASTNodeKind::AST_RETURN, return_expr.ok(), None, None, return_expr_type))
    }

    fn parse_while_stmt(&mut self) -> ParseResult {
        let cond_ast: ParseResult = self.parse_conditional_stmt(TokenKind::KW_WHILE);
        let while_body: ParseResult = self.parse_single_stmt();
        Ok(ASTNode::new(ASTNodeKind::AST_WHILE, cond_ast.ok(), while_body.ok(), None, LitTypeVariant::None))
    }

    fn parse_for_stmt(&mut self) -> ParseResult {
        _ = self.token_match(TokenKind::KW_FOR); // match and ignore the keyword 'for'
        _ = self.token_match(TokenKind::T_LPAREN); // match and ignore '('
        let pre_stmt: ParseResult = self.parse_single_stmt(); // initialization statement
        // _ = self.token_match(TokenKind::T_SEMICOLON);
        let cond_ast: ParseResult = self.parse_equality(); // conditional section of for loop
        if let Ok(_icast) = &cond_ast {
            if (_icast.operation < ASTNodeKind::AST_EQEQ) || (_icast.operation > ASTNodeKind::AST_LTHAN) { // if operation kind is not "relational operation"
                panic!("Please provide conditional expression for 'for'");
            }
        }
        _ = self.token_match(TokenKind::T_SEMICOLON); // expect semicolon
        let incr_ast: ParseResult = self.parse_single_stmt();
        _ = self.token_match(TokenKind::T_RPAREN); // match and ignore ')'
        let for_body: ParseResult = self.parse_single_stmt();
        let mut tree: ASTNode = ASTNode::new(ASTNodeKind::AST_GLUE, for_body.ok(), incr_ast.ok(), None, LitTypeVariant::None);
        tree = ASTNode::new(ASTNodeKind::AST_WHILE, cond_ast.ok(), Some(tree), None, LitTypeVariant::None);
        Ok(ASTNode::new(ASTNodeKind::AST_GLUE, pre_stmt.ok(), Some(tree), None, LitTypeVariant::None))
    }

    fn parse_if_stmt(&mut self) -> ParseResult {
        let cond_ast: ParseResult = self.parse_conditional_stmt(TokenKind::KW_IF);
        let if_true_ast: ParseResult = self.parse_single_stmt();
        let mut if_false_ast: ParseResult = Err(ParseError::None);
        if self.current_token.kind == TokenKind::KW_ELSE {
            self.skip_to_next_token(); // skip 'else'
            if_false_ast = self.parse_single_stmt();
        }
        Ok(ASTNode::with_mid(ASTNodeKind::AST_IF, cond_ast.ok(), if_false_ast.ok(), if_true_ast.ok(), None, LitTypeVariant::None))
    }

    // parses tokens that are in the form '(expression [< | > | >= | <= | == | !=] expression)'
    fn parse_conditional_stmt(&mut self, kind: TokenKind) -> ParseResult {
        _ = self.token_match(kind);
        _ = self.token_match(TokenKind::T_LPAREN); // match and ignore '('
        let cond_ast: ParseResult = self.parse_equality();
        if let Ok(_icast) = &cond_ast {
            if (_icast.operation < ASTNodeKind::AST_EQEQ) || (_icast.operation > ASTNodeKind::AST_LTHAN) { // if operation kind is not "relational operation"
                panic!("'{:?}' is not allowed in {:?}'s condition.", _icast.operation, kind);
            }
        }
        _ = self.token_match(TokenKind::T_RPAREN); // match and ignore ')'
        cond_ast
    }

    fn parse_assignment_stmt(&mut self) -> ParseResult {
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
        let bin_expr_node: ParseResult = self.parse_equality();
        let bin_expr_res_type: LitTypeVariant = bin_expr_node.as_ref().unwrap().result_type;
        // Test if expression type matches the variable type.
        if bin_expr_res_type != symbol.lit_type && !Parser::is_assignment_compatible(symbol.lit_type, bin_expr_node.as_ref().unwrap()) {
            panic!("Expression result does not match variable type! {:?} = {:?}???", symbol, bin_expr_res_type);
        }
        let _result_type: LitTypeVariant = bin_expr_node.as_ref().unwrap().result_type;
        _ = self.token_match(TokenKind::T_SEMICOLON);
        let lvalueid: ASTNode = ASTNode::make_leaf(ASTNodeKind::AST_LVIDENT, LitType::I32(_id_index_symt as i32), symbol.lit_type);
        Ok(ASTNode::new(ASTNodeKind::AST_ASSIGN, bin_expr_node.ok(), Some(lvalueid), None, _result_type))
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
        sym.lit_type = self.parse_id_type();
        if sym.lit_type == LitTypeVariant::None {
            panic!("Can't create variable of type {:?}", self.current_token.kind);
        }
        let id_token: Token = self.token_match(TokenKind::T_IDENTIFIER).clone();
        sym.name = id_token.lexeme.clone();
        _ = self.token_match(TokenKind::T_SEMICOLON);
        self.sym_table.borrow_mut().add_symbol(sym); // track the symbol that has been defined
    }

    fn parse_id_type(&mut self) -> LitTypeVariant {
        let mut typ: LitTypeVariant = LitTypeVariant::from_token_kind(self.current_token.kind);
        if typ == LitTypeVariant::None {
            return typ; // return None type
        }
        self.skip_to_next_token(); // skip the parsed type's token
        loop {
            // could encounter '*'; need to parse it as well
            let curr_kind: TokenKind = self.current_token.kind;
            if curr_kind != TokenKind::T_STAR { break; }
            self.skip_to_next_token();
            typ = LitTypeVariant::to_pointer_type(typ);
        }
        typ
    }

    fn parse_equality(&mut self) -> ParseResult {
        let left: ParseResult = self.parse_comparision();
        self.try_parsing_binary(left, vec![TokenKind::T_EQEQ, TokenKind::T_NEQ])
    }

    fn parse_comparision(&mut self) -> ParseResult {
        let left: ParseResult = self.parse_addition();
        self.try_parsing_binary(left, vec![TokenKind::T_GTHAN, TokenKind::T_LTHAN, TokenKind::T_GTEQ, TokenKind::T_LTEQ])
    }

    fn parse_addition(&mut self) -> ParseResult {
        let left: ParseResult = self.parse_factor();
        self.try_parsing_binary(left, vec![TokenKind::T_PLUS, TokenKind::T_MINUS])
    }

    fn parse_factor(&mut self) -> ParseResult {
        let left: ParseResult = self.parse_func_call_expr();
        self.try_parsing_binary(left, vec![TokenKind::T_SLASH, TokenKind::T_STAR])
    }

    fn parse_func_call_expr(&mut self) -> ParseResult {
        let possible_func_name_token: Token = self.current_token.clone();
        let possible_id_node: ParseResult = self.parse_mem_prefix(); // could be other type of node than identifier node
        match possible_id_node.as_ref() {
            Err(_) => possible_id_node, // this node contains errornous token information
            Ok(node) => {
                if self.current_token.kind == TokenKind::T_LPAREN {
                    if node.operation != ASTNodeKind::AST_IDENT { // If something is being called, then that has to be an identifier type. tokenoken
                        return Err(ParseError::NotCallable(possible_func_name_token));
                    }
                    let id_type: &LitType = node.value.as_ref().unwrap();
                    let mut symbol_index: i32 = 0x1FFFFFFF;
                    if id_type.is_i32() {
                        symbol_index = id_type.unwrap_i32();
                    } else {
                        // handle the case when we do not receive symbol index
                    }
                    let result_type: LitTypeVariant = self.sym_table.borrow().get_symbol(symbol_index as usize).clone().lit_type;
                    self.token_match(TokenKind::T_LPAREN); // match and ignore '('
                    self.token_match(TokenKind::T_RPAREN); // match and ignore ')'
                    Ok(ASTNode::make_leaf(ASTNodeKind::AST_FUNC_CALL, LitType::I32(symbol_index), result_type))
                } else {
                    possible_id_node
                }
            }
        }
    }

    fn try_parsing_binary(&mut self, mut left_side_tree: ParseResult, tokens: Vec<TokenKind>) -> ParseResult {
        let mut ok: bool = false;
        match left_side_tree {
            Ok(ref mut left) => {
                let current_token_kind: TokenKind = self.current_token.kind;
                for token in tokens {
                    if token == current_token_kind {
                        ok = true;
                        break;
                    }
                }
                if ok {
                    self.skip_to_next_token(); // skip the operator
                    let right_side_tree: ParseResult = self.parse_mem_prefix();
                    let mut right: ASTNode = right_side_tree?;
                    if left.result_type != right.result_type {
                        let compatibility: (bool, u8, u8) = left.value.as_ref().unwrap().compatible(right.value.as_ref().unwrap());
                        match compatibility {
                            (true, 0, 1) => right.result_type = left.result_type,
                            (true, 1, 0) => left.result_type = right.result_type,
                            _ => panic!("Incompatible types: '{:?}' and '{:?}'", left.result_type, right.result_type)
                        }
                    }
                    let result_type: LitTypeVariant = left.result_type;
                    Ok(ASTNode::new(ASTNodeKind::from_token_kind(current_token_kind), Some(left.clone()), Some(right.clone()), None, result_type))
                } else {
                    left_side_tree
                }
            },
            _ => left_side_tree
        }
    }

    // parse memory related prefixes such as '*' for dereferencing and '&' for memory address
    fn parse_mem_prefix(&mut self) -> ParseResult {
        let curr_kind: TokenKind = self.current_token.kind;
        match curr_kind {
            TokenKind::T_AMPERSAND => {
                // for now, multiple layer of addressing is not possible. i.e. &&&a is not possible. 
                // still, any '&' prefixing an identifier is parsed
                self.skip_to_next_token(); // skip '&'
                let mut tree: ParseResult = self.parse_mem_prefix();
                if let Ok(ref mut add_tree) = tree {
                    if add_tree.operation != ASTNodeKind::AST_IDENT {
                        panic!("Can't take address of '{:?}'", add_tree.value);
                    }
                    add_tree.operation = ASTNodeKind::AST_ADDR;
                    add_tree.result_type = LitTypeVariant::to_pointer_type(add_tree.result_type);
                }
                tree
            },
            TokenKind::T_STAR => {
                // same as '&'; parsing multiple '*' but not supporting it(yet)
                self.skip_to_next_token(); // skip '*'
                let mut tree: ParseResult = self.parse_mem_prefix();
                if let Ok(ref mut add_tree) = tree {
                    if add_tree.operation != ASTNodeKind::AST_IDENT {
                        panic!("Can't dereference type '{:?}'", add_tree.value);
                    }
                    add_tree.operation = ASTNodeKind::AST_DEREF;
                    add_tree.result_type = LitTypeVariant::to_value_type(add_tree.result_type);
                }
                tree
            },
            _ => self.parse_primary()
        }
    }

    fn parse_primary(&mut self) -> ParseResult {
        let current_token: Token = self.current_token.clone();
        self.skip_to_next_token();
        match current_token.kind {
            TokenKind::T_INT_NUM => Ok(ASTNode::make_leaf(ASTNodeKind::AST_INTLIT, LitType::I32(current_token.lexeme.parse::<i32>().unwrap()), LitTypeVariant::I32)),
            TokenKind::T_CHAR => Ok(ASTNode::make_leaf(ASTNodeKind::AST_INTLIT, LitType::U8(current_token.lexeme.parse::<u8>().unwrap()), LitTypeVariant::U8)),
            TokenKind::T_LONG_NUM => Ok(ASTNode::make_leaf(ASTNodeKind::AST_INTLIT, LitType::I64(current_token.lexeme.parse::<i64>().unwrap()), LitTypeVariant::I64)),
            TokenKind::T_FLOAT_NUM 
            | TokenKind::T_DOUBLE_NUM => Ok(ASTNode::make_leaf(ASTNodeKind::AST_INTLIT, LitType::F32(current_token.lexeme.parse::<f32>().unwrap()), LitTypeVariant::F32)),
            TokenKind::T_IDENTIFIER => {
                let id_index: usize = self.sym_table.borrow().find_symbol(&current_token.lexeme);
                if id_index == 0xFFFFFFFF { // if symbol has not been defined
                    return Err(ParseError::SymbolNotFound(current_token));
                }
                let symbol: Symbol = self.sym_table.borrow().get_symbol(id_index).clone();
                Ok(ASTNode::make_leaf(ASTNodeKind::AST_IDENT, LitType::I32(id_index as i32), symbol.lit_type))
            },
            TokenKind::T_LPAREN => { // group expression: e.g: (a * (b + c)))
                let group_expr: ParseResult = self.parse_equality();
                // Group expression terminates with ')'. Match and ignore ')'.
                self.token_match(TokenKind::T_RPAREN); 
                Ok(group_expr.unwrap())
            },
            _ => {
                println!("{:?}", current_token);
                panic!("Unrecognized primitive type: {:?}", current_token);
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
        let result: ParseResult = p.parse_equality();
        assert!(result.is_ok());
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
        let result: ParseResult = p.parse_equality();
        assert!(result.is_ok());
        assert_eq!(result.unwrap().operation, ASTNodeKind::AST_ADD);
    }

    // test if-else block
    #[test]
    fn test_if_else_statement_block() {
        let mut tokener: Tokenizer = Tokenizer::new("if (4 > 5) { global integer a; } else { global integer b; }");
        let tokens: Vec<Token> = tokener.start_scan();
        let sym_table: Rc<RefCell<Symtable>> = Rc::new(RefCell::new(Symtable::new()));
        let mut p: Parser = Parser::new(&tokens, Rc::clone(&sym_table));
        let result: ParseResult = p.parse_if_stmt();
        assert!(result.is_ok(), "If this assertion did not pass, then if-else block is probably malformed.");
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

    // test return statements
}