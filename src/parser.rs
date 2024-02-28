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

use crate::ast::*;
use crate::enums::*;
use crate::error;
use crate::symtable::*;
use crate::tokenizer::*;
use crate::types::*;

#[derive(Eq, PartialEq, Debug)]
enum ParseError {
    UnexpectedToken(Token), // unexpected token was encountered
    SymbolNotFound(Token),  // symbol not found; symbol has not been defined before
    NotCallable(Token),     // if a token being called is not callable type
    GlobalInsideFunction(Token),
    UnsubscritableToken(Token),
    None, // just a placeholder
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::NotCallable(_) => write!(f, "not callable"),
            ParseError::GlobalInsideFunction(_) => {
                write!(f, "global variable declaration inside a function")
            }
            ParseError::UnsubscritableToken(_) => {
                write!(f, "token is not subscriptable")
            }
            _ => write!(f, "other"),
        }
    }
}

type ParseResult = Result<ASTNode, ParseError>;

// Actual parser
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    current_token: Token,
    sym_table: Rc<RefCell<Symtable>>, // symbol table for global identifiers
    // ID of a function that is presently being parsed. This field's value is 0xFFFFFFFF
    // if the parser is not inside a function.
    current_function_id: usize,
    _label_id: &'static mut usize, // label generator
}

impl Parser {
    #[inline]
    pub fn new(tokens: Vec<Token>, sym_table: Rc<RefCell<Symtable>>, label_id: &'static mut usize) -> Self {
        let current_token: Token = tokens[0].clone();
        Self {
            tokens,
            current: 0,
            current_token,
            sym_table,
            current_function_id: 0xFFFFFFFF,
            _label_id: label_id
        }
    }

    pub fn start(&mut self, traverser: &mut ASTTraverser) {
        let mut nodes: Vec<ASTNode> = vec![];
        loop {
            if self.current_token.kind == TokenKind::T_EOF {
                break;
            }
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
        for symbol in self.sym_table.borrow().iter() {
            if symbol.sym_type == SymbolType::Function {
                continue;
            }
            println!(".data\n.global {}", symbol.name);
            if symbol.sym_type == SymbolType::Variable {
                Parser::dump_global_with_alignment(symbol);
            } else if symbol.sym_type == SymbolType::Array {
                let array_data_size: usize = symbol.lit_type.size();
                println!("{}:", symbol.name);
                for _ in 0..symbol.size {
                    Parser::alloc_data_space(array_data_size);
                }
            }
        }
    }

    fn alloc_data_space(size: usize) {
        match size {
            1 => println!(".byte 0"),
            4 => println!(".word 0"),
            8 => println!(".quad 0"),
            _ => panic!("Not possible to generate space for size: {}", size),
        }
    }

    fn dump_global_with_alignment(symbol: &Symbol) {
        match symbol.lit_type {
            LitTypeVariant::F32Ptr
            | LitTypeVariant::F64Ptr
            | LitTypeVariant::I16Ptr
            | LitTypeVariant::I32Ptr
            | LitTypeVariant::I64Ptr
            | LitTypeVariant::U8Ptr
            | LitTypeVariant::VoidPtr => println!("{}: .align 8\n\t.quad 0", symbol.name),
            LitTypeVariant::I32 => println!("{}: .align 4\n\t.word 0", symbol.name),
            LitTypeVariant::U8 => println!("{}:\t.byte 0", symbol.name),
            _ => panic!("Symbol's size is not supported right now: '{:?}'", symbol),
        }
    }

    fn parse_single_stmt(&mut self) -> ParseResult {
        match self.current_token.kind {
            TokenKind::KW_GLOBAL => {
                if let Some(e) = self.parse_global_variable_decl_stmt() {
                    panic!("{}", e);
                }
                Err(ParseError::UnexpectedToken(self.current_token.clone()))
            }
            TokenKind::T_IDENTIFIER => self.parse_assignment_stmt(),
            TokenKind::KW_IF => self.parse_if_stmt(),
            TokenKind::KW_WHILE => self.parse_while_stmt(),
            TokenKind::KW_FOR => self.parse_for_stmt(),
            TokenKind::T_LBRACE => self.parse_compound_stmt(),
            TokenKind::KW_DEF => self.parse_function_stmt(),
            TokenKind::KW_RETURN => self.parse_return_stmt(),
            TokenKind::T_EOF => Err(ParseError::UnexpectedToken(self.current_token.clone())),
            _ => self.parse_equality(),
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
                    left = Some(ASTNode::new(
                        ASTNodeKind::AST_GLUE,
                        left.clone(),
                        tree.ok(),
                        None,
                        LitTypeVariant::None,
                    ));
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
            error::report_unexpected_token(
                &self.current_token,
                Some("Not a valid return type for a function."),
            );
        }
        self.skip_to_next_token(); // skip return type
        let function_id: Option<usize> = self.sym_table.borrow_mut().add_symbol(Symbol::new(
            id_token.lexeme.clone(),
            func_return_type,
            SymbolType::Function,
        ));
        if function_id.is_none() {
            panic!("Symbol already defined: '{:?}'", id_token.lexeme);
        }
        self.current_function_id = function_id.unwrap();
        let function_body: ParseResult = self.parse_compound_stmt();
        let temp_func_id: usize = self.current_function_id;
        self.current_function_id = 0xFFFFFFFF; // exiting out of function body
        Ok(ASTNode::new(
            ASTNodeKind::AST_FUNCTION,
            function_body.ok(),
            None,
            Some(LitType::I32(temp_func_id as i32)),
            func_return_type,
        ))
    }

    #[allow(unused_mut)]
    #[allow(unused_assignments)]
    fn parse_return_stmt(&mut self) -> ParseResult {
        let mut void_ret_type: bool = false;
        let mut func_symbol: Option<Symbol> = None;
        // check whether parser's parsing a function or not
        if self.current_function_id == 0xFFFFFFFF {
            error::report_unexpected_token(
                &self.current_token,
                Some("'return' statement outside a function is not valid."),
            );
        } else {
            let sym: Option<Symbol> = Some(self.sym_table.borrow().get_symbol(self.current_function_id).unwrap().clone());
            if sym.is_none() {
                panic!("Undefined function. ok");
            }
            func_symbol = sym;
            void_ret_type = func_symbol.as_ref().unwrap().lit_type == LitTypeVariant::Void;
        }
        _ = self.token_match(TokenKind::KW_RETURN);
        if void_ret_type {
            // if function has void as a return type, panic if any expression follows the keyword 'return'
            if self.current_token.kind != TokenKind::T_SEMICOLON {
                error::report_unexpected_token(
                    &self.current_token,
                    Some("Expected ';' because function has a 'void' return type."),
                );
            }
            // skip semicolon
            self.token_match(TokenKind::T_SEMICOLON);
            return Ok(ASTNode::make_leaf(
                ASTNodeKind::AST_RETURN,
                LitType::Void,
                LitTypeVariant::Void,
            ));
        }
        let return_expr: ParseResult = self.parse_equality();
        let return_expr_type: LitTypeVariant = return_expr.as_ref().unwrap().result_type;
        if return_expr_type != func_symbol.as_ref().unwrap().lit_type {
            panic!("Return value's type does not match function's return type.");
        }
        _ = self.token_match(TokenKind::T_SEMICOLON); // expect semicolon to end a return statement
        Ok(ASTNode::new(
            ASTNodeKind::AST_RETURN,
            return_expr.ok(),
            None,
            None,
            return_expr_type,
        ))
    }

    fn parse_while_stmt(&mut self) -> ParseResult {
        let cond_ast: ParseResult = self.parse_conditional_stmt(TokenKind::KW_WHILE);
        let while_body: ParseResult = self.parse_single_stmt();
        Ok(ASTNode::new(
            ASTNodeKind::AST_WHILE,
            cond_ast.ok(),
            while_body.ok(),
            None,
            LitTypeVariant::None,
        ))
    }

    fn parse_for_stmt(&mut self) -> ParseResult {
        _ = self.token_match(TokenKind::KW_FOR); // match and ignore the keyword 'for'
        _ = self.token_match(TokenKind::T_LPAREN); // match and ignore '('
        let pre_stmt: ParseResult = self.parse_single_stmt(); // initialization statement
                                                              // _ = self.token_match(TokenKind::T_SEMICOLON);
        let cond_ast: ParseResult = self.parse_equality(); // conditional section of for loop
        if let Ok(_icast) = &cond_ast {
            if (_icast.operation < ASTNodeKind::AST_EQEQ)
                || (_icast.operation > ASTNodeKind::AST_LTHAN)
            {
                // if operation kind is not "relational operation"
                panic!("Please provide conditional expression for 'for'");
            }
        }
        _ = self.token_match(TokenKind::T_SEMICOLON); // expect semicolon
        let incr_ast: ParseResult = self.parse_single_stmt();
        _ = self.token_match(TokenKind::T_RPAREN); // match and ignore ')'
        let for_body: ParseResult = self.parse_single_stmt();
        let mut tree: ASTNode = ASTNode::new(
            ASTNodeKind::AST_GLUE,
            for_body.ok(),
            incr_ast.ok(),
            None,
            LitTypeVariant::None,
        );
        tree = ASTNode::new(
            ASTNodeKind::AST_WHILE,
            cond_ast.ok(),
            Some(tree),
            None,
            LitTypeVariant::None,
        );
        Ok(ASTNode::new(
            ASTNodeKind::AST_GLUE,
            pre_stmt.ok(),
            Some(tree),
            None,
            LitTypeVariant::None,
        ))
    }

    fn parse_if_stmt(&mut self) -> ParseResult {
        let cond_ast: ParseResult = self.parse_conditional_stmt(TokenKind::KW_IF);
        let if_true_ast: ParseResult = self.parse_single_stmt();
        let mut if_false_ast: ParseResult = Err(ParseError::None);
        if self.current_token.kind == TokenKind::KW_ELSE {
            self.skip_to_next_token(); // skip 'else'
            if_false_ast = self.parse_single_stmt();
        }
        Ok(ASTNode::with_mid(
            ASTNodeKind::AST_IF,
            cond_ast.ok(),
            if_false_ast.ok(),
            if_true_ast.ok(),
            None,
            LitTypeVariant::None,
        ))
    }

    // parses tokens that are in the form '(expression [< | > | >= | <= | == | !=] expression)'
    fn parse_conditional_stmt(&mut self, kind: TokenKind) -> ParseResult {
        _ = self.token_match(kind);
        _ = self.token_match(TokenKind::T_LPAREN); // match and ignore '('
        let cond_ast: ParseResult = self.parse_equality();
        if let Ok(_icast) = &cond_ast {
            if (_icast.operation < ASTNodeKind::AST_EQEQ)
                || (_icast.operation > ASTNodeKind::AST_LTHAN)
            {
                // if operation kind is not "relational operation"
                panic!(
                    "'{:?}' is not allowed in {:?}'s condition.",
                    _icast.operation, kind
                );
            }
        }
        _ = self.token_match(TokenKind::T_RPAREN); // match and ignore ')'
        cond_ast
    }

    fn parse_assignment_stmt(&mut self) -> ParseResult {
        let id_token: Token = self.token_match(TokenKind::T_IDENTIFIER).clone();
        let _id_index_symt_op: Option<usize> = self.sym_table.borrow().find_symbol(&id_token.lexeme);
        if _id_index_symt_op.is_none() {
            // if the symbol has not been defined
            panic!("Assigning to an undefined symbol '{}'", id_token.lexeme);
        }
        let _id_index_symt: usize = _id_index_symt_op.unwrap();
        let symbol: Symbol = self.sym_table.borrow().get_symbol(_id_index_symt).unwrap().clone();
        // Check if we are assigning to a type other than SymbolType::Variable. If yes, panic!
        if symbol.sym_type != SymbolType::Variable {
            panic!("Assigning to type '{:?}' is not allowed!", symbol.sym_type);
        }
        _ = self.token_match(TokenKind::T_EQUAL);
        let mut bin_expr_node: ParseResult = self.parse_equality();
        let bin_expr_res_type: LitTypeVariant = bin_expr_node.as_ref().unwrap().result_type;
        // Test if expression type matches the variable type.
        let compat_node: Option<ASTNode> = bin_expr_node
            .as_mut()
            .ok()
            .unwrap()
            .modify(symbol.lit_type, ASTNodeKind::AST_NONE);
        if compat_node.is_none() {
            panic!(
                "Can't assign value of type '{:?}' to variable of type: '{:?}'",
                bin_expr_res_type, symbol.lit_type
            );
        }
        let _result_type: LitTypeVariant = compat_node.as_ref().unwrap().result_type;
        _ = self.token_match(TokenKind::T_SEMICOLON);
        let lvalueid: ASTNode = ASTNode::make_leaf(
            ASTNodeKind::AST_LVIDENT,
            LitType::I32(_id_index_symt as i32),
            symbol.lit_type,
        );
        Ok(ASTNode::new(
            ASTNodeKind::AST_ASSIGN,
            compat_node,
            Some(lvalueid),
            None,
            _result_type,
        ))
    }

    fn parse_global_variable_decl_stmt(&mut self) -> Option<ParseError> {
        if self.current_function_id != 0xFFFFFFFF {
            // if we are inside a function
            return Some(ParseError::GlobalInsideFunction(self.current_token.clone()));
        }
        _ = self.token_match(TokenKind::KW_GLOBAL);
        // let mut sym: MaybeUninit<Symbol> = MaybeUninit::<Symbol>::uninit();
        let mut sym: Symbol = Symbol::uninit();
        sym.lit_type = self.parse_id_type();
        if sym.lit_type == LitTypeVariant::None {
            panic!(
                "Can't create variable of type {:?}",
                self.current_token.kind
            );
        }
        let id_token: Token = self.token_match(TokenKind::T_IDENTIFIER).clone();
        sym.name = id_token.lexeme.clone();
        // if it is going to be an array type
        if self.current_token.kind == TokenKind::T_LBRACKET {
            return self.parse_global_array_var_decl_stmt(sym);
        }
        _ = self.token_match(TokenKind::T_SEMICOLON);
        sym.size = 1;
        self.sym_table.borrow_mut().add_symbol(sym); // track the symbol that has been defined
        None
    }

    fn parse_global_array_var_decl_stmt(&mut self, mut sym: Symbol) -> Option<ParseError> {
        self.skip_to_next_token(); // skip '['
        let array_size_token: Token = self.current_token.clone();
        let mut array_size_type: TokenKind = TokenKind::T_NONE;
        for t in [
            TokenKind::T_INT_NUM,
            TokenKind::T_CHAR,
            TokenKind::T_LONG_NUM,
        ] {
            if array_size_token.kind == t {
                array_size_type = t;
            }
        }
        if array_size_type == TokenKind::T_NONE {
            panic!(
                "Array size must be specified with an integer value. Given: '{:?}'",
                array_size_token.lexeme
            );
        }
        self.token_match(array_size_type);
        self.token_match(TokenKind::T_RBRACKET);
        self.token_match(TokenKind::T_SEMICOLON);
        sym.sym_type = SymbolType::Array;
        sym.size = array_size_token.lexeme.parse::<usize>().unwrap();
        self.sym_table.borrow_mut().add_symbol(sym);
        None
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
            if curr_kind != TokenKind::T_STAR {
                break;
            }
            self.skip_to_next_token();
            typ = LitTypeVariant::pointer_type(typ);
        }
        typ
    }

    fn parse_equality(&mut self) -> ParseResult {
        let left: ParseResult = self.parse_comparision();
        self.try_parsing_binary(left, vec![TokenKind::T_EQEQ, TokenKind::T_NEQ])
    }

    fn parse_comparision(&mut self) -> ParseResult {
        let left: ParseResult = self.parse_addition();
        self.try_parsing_binary(
            left,
            vec![
                TokenKind::T_GTHAN,
                TokenKind::T_LTHAN,
                TokenKind::T_GTEQ,
                TokenKind::T_LTEQ,
            ],
        )
    }

    fn parse_addition(&mut self) -> ParseResult {
        let left: ParseResult = self.parse_factor();
        self.try_parsing_binary(left, vec![TokenKind::T_PLUS, TokenKind::T_MINUS])
    }

    fn parse_factor(&mut self) -> ParseResult {
        let left: ParseResult = self.parse_mem_prefix();
        self.try_parsing_binary(left, vec![TokenKind::T_SLASH, TokenKind::T_STAR])
    }

    fn try_parsing_binary(
        &mut self,
        mut left_side_tree: ParseResult,
        tokens: Vec<TokenKind>,
    ) -> ParseResult {
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
                    let ast_op: ASTNodeKind = ASTNodeKind::from_token_kind(current_token_kind);
                    let right_side_tree: ParseResult = self.parse_mem_prefix();
                    let mut right: ASTNode = right_side_tree?;
                    let temp_left: Option<ASTNode> = left.modify(right.result_type, ast_op);
                    let temp_right: Option<ASTNode> = right.modify(left.result_type, ast_op);
                    if temp_left.is_none() && temp_right.is_none() {
                        panic!(
                            "Incompatible types: '{:?}' and '{:?}' for operator '{:?}'",
                            left.result_type, right.result_type, ast_op
                        );
                    }
                    let left_node: Option<ASTNode> = if temp_left.is_some() {
                        temp_left
                    } else { Some(left.clone()) };
                    let right_node: Option<ASTNode> = if temp_right.is_some() {
                        temp_right
                    } else { Some(right.clone()) };
                    let result_type: LitTypeVariant = left.result_type;
                    Ok(ASTNode::new(
                        ast_op,
                        left_node,
                        right_node,
                        None,
                        result_type,
                    ))
                } else {
                    left_side_tree
                }
            }
            _ => left_side_tree,
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
                    add_tree.result_type = LitTypeVariant::pointer_type(add_tree.result_type);
                }
                tree
            }
            TokenKind::T_STAR => {
                // same as '&'; parsing multiple '*' but not supporting it(yet)
                self.skip_to_next_token(); // skip '*'
                let mut tree: ParseResult = self.parse_mem_prefix();
                if let Ok(ref mut add_tree) = tree {
                    if add_tree.operation != ASTNodeKind::AST_IDENT {
                        panic!("Can't dereference type '{:?}'", add_tree.value);
                    }
                    add_tree.operation = ASTNodeKind::AST_DEREF;
                    add_tree.result_type = LitTypeVariant::value_type(add_tree.result_type);
                }
                tree
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> ParseResult {
        let current_token: Token = self.current_token.clone();
        self.skip_to_next_token();
        match current_token.kind {
            TokenKind::T_INT_NUM => Ok(ASTNode::make_leaf(
                ASTNodeKind::AST_INTLIT,
                LitType::I32(current_token.lexeme.parse::<i32>().unwrap()),
                LitTypeVariant::I32,
            )),
            TokenKind::T_CHAR => Ok(ASTNode::make_leaf(
                ASTNodeKind::AST_INTLIT,
                LitType::U8(current_token.lexeme.parse::<u8>().unwrap()),
                LitTypeVariant::U8,
            )),
            TokenKind::T_LONG_NUM => Ok(ASTNode::make_leaf(
                ASTNodeKind::AST_INTLIT,
                LitType::I64(current_token.lexeme.parse::<i64>().unwrap()),
                LitTypeVariant::I64,
            )),
            TokenKind::T_FLOAT_NUM | TokenKind::T_DOUBLE_NUM => Ok(ASTNode::make_leaf(
                ASTNodeKind::AST_INTLIT,
                LitType::F32(current_token.lexeme.parse::<f32>().unwrap()),
                LitTypeVariant::F32,
            )),
            TokenKind::T_STRING => {
                let str_label: usize = *self._label_id;
                *self._label_id += 1;
                println!("_L{}: .ascii \"{}\"", str_label, current_token.lexeme);
                Ok(ASTNode::make_leaf(ASTNodeKind::AST_STRLIT, LitType::I32(str_label as i32), LitTypeVariant::U8Ptr))
            }
            TokenKind::T_IDENTIFIER => {
                let id_index_op: Option<usize> = self.sym_table.borrow().find_symbol(&current_token.lexeme);
                if id_index_op.is_none() {
                    // if symbol has not been defined
                    return Err(ParseError::SymbolNotFound(current_token));
                }
                let id_index: usize = id_index_op.unwrap();
                let symbol: Symbol = self.sym_table.borrow().get_symbol(id_index).unwrap().clone();
                let curr_tok_kind: TokenKind = self.current_token.kind;
                if curr_tok_kind == TokenKind::T_LPAREN {
                    self.parse_func_call_expr(&symbol, id_index, &current_token)
                } else if curr_tok_kind == TokenKind::T_LBRACKET {
                    self.parse_array_index_expr(&symbol, id_index, &current_token)
                } else {
                    Ok(ASTNode::make_leaf(
                        ASTNodeKind::AST_IDENT,
                        LitType::I32(id_index as i32),
                        symbol.lit_type,
                    ))
                }
            }
            TokenKind::T_LPAREN => {
                // group expression: e.g: (a * (b + c)))
                let group_expr: ParseResult = self.parse_equality();
                // Group expression terminates with ')'. Match and ignore ')'.
                self.token_match(TokenKind::T_RPAREN);
                Ok(group_expr.unwrap())
            }
            _ => {
                println!("{:?}", current_token);
                panic!("Unrecognized primitive type: {:?}", current_token);
            }
        }
    }

    fn parse_array_index_expr(
        &mut self,
        called_symbol: &Symbol,
        sym_index: usize,
        sym_token: &Token,
    ) -> ParseResult {
        _ = self.token_match(TokenKind::T_LBRACKET);
        let array_access_expr_result: ParseResult = self.parse_equality();
        #[allow(clippy::question_mark)]
        if array_access_expr_result.is_err() {
            return array_access_expr_result;
        }
        let array_access_expr: ASTNode = array_access_expr_result.ok().unwrap();
        if called_symbol.sym_type != SymbolType::Array {
            return Err(ParseError::UnsubscritableToken(sym_token.clone()));
        }
        _ = self.token_match(TokenKind::T_RBRACKET);
        Ok(ASTNode::new(
            ASTNodeKind::AST_ARRAY_ACCESS,
            Some(array_access_expr),
            None,
            Some(LitType::I32(sym_index as i32)),
            called_symbol.lit_type,
        ))
    }

    fn parse_func_call_expr(
        &mut self,
        called_symbol: &Symbol,
        sym_index: usize,
        sym_token: &Token,
    ) -> ParseResult {
        _ = self.token_match(TokenKind::T_LPAREN);
        if called_symbol.sym_type != SymbolType::Function {
            return Err(ParseError::NotCallable(sym_token.clone()));
        }
        _ = self.token_match(TokenKind::T_RPAREN);
        Ok(ASTNode::make_leaf(
            ASTNodeKind::AST_FUNC_CALL,
            LitType::I32(sym_index as i32),
            called_symbol.lit_type,
        ))
    }

    fn token_match(&mut self, kind: TokenKind) -> &Token {
        let current: Token = self.current_token.clone();
        if kind != current.kind {
            panic!(
                "Expected the token to be '{:?}' but found '{:?}'",
                kind, current.kind
            );
        }
        self.skip_to_next_token();
        &self.tokens[self.current - 1]
    }

    fn skip_to_next_token(&mut self) {
        self.current += 1;
        if self.current >= self.tokens.len() {
            return;
        }
        self.current_token = self.tokens[self.current].clone();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_group_expression_tree_structure() {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new("(5 + (3 * 4))");
        let sym_table: Rc<RefCell<Symtable>> = Rc::new(RefCell::new(Symtable::new()));
        let mut p: Parser = Parser::new(tokener.start_scan(), Rc::clone(&sym_table), unsafe { &mut LABEL_ID });
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
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new("5+5");
        let sym_table: Rc<RefCell<Symtable>> = Rc::new(RefCell::new(Symtable::new()));
        let mut p: Parser = Parser::new(tokener.start_scan(), Rc::clone(&sym_table), unsafe { &mut LABEL_ID });
        let result: ParseResult = p.parse_equality();
        assert!(result.is_ok());
        assert_eq!(result.unwrap().operation, ASTNodeKind::AST_ADD);
    }

    // test if-else block
    #[test]
    fn test_if_else_statement_block() {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new("if (4 > 5) { global integer a; } else { global integer b; }");
        let sym_table: Rc<RefCell<Symtable>> = Rc::new(RefCell::new(Symtable::new()));
        let mut p: Parser = Parser::new(tokener.start_scan(), Rc::clone(&sym_table), unsafe { &mut LABEL_ID });
        let result: ParseResult = p.parse_if_stmt();
        assert!(
            result.is_ok(),
            "If this assertion did not pass, then if-else block is probably malformed."
        );
        let upvalue: &ASTNode = result.as_ref().unwrap();
        // Global variable declaration statements produce None as result.
        // So, both 'mid (if)' and 'right (else)' has to be None types
        assert!(
            upvalue.mid.is_none(),
            "global declarations inside comppound statement should produce None result"
        );
        assert!(
            upvalue.right.is_none(),
            "global declarations inside comppound statement should produce None result"
        );
        assert_eq!(upvalue.operation, ASTNodeKind::AST_IF); // main AST node is of AST_IF type
        assert_eq!(
            (*upvalue.left).as_ref().unwrap().operation,
            ASTNodeKind::AST_GTHAN,
            "Unexpected ASTNodeKind; expected be ASTNodeKind::AST_GTHAN."
        );
    }

    // If following two tests pass, then we can conclude that every other pointer type declarations,
    // dereferencing, and addressing will work.
    #[test]
    fn test_integer_id_addr_load() {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new("global integer *b; global integer a; b = &a;");
        let sym_table: Rc<RefCell<Symtable>> = Rc::new(RefCell::new(Symtable::new()));
        let mut p: Parser = Parser::new(tokener.start_scan(), Rc::clone(&sym_table), unsafe { &mut LABEL_ID });
        p.parse_global_variable_decl_stmt();
        p.parse_global_variable_decl_stmt();
        let result: ParseResult = p.parse_single_stmt();
        assert!(result.is_ok());
        let upvalue: &ASTNode = result.as_ref().unwrap();
        assert_eq!(upvalue.operation, ASTNodeKind::AST_ASSIGN);
        assert_eq!(
            (*upvalue.right).as_ref().unwrap().operation,
            ASTNodeKind::AST_LVIDENT
        );
        assert_eq!(
            (*upvalue.left).as_ref().unwrap().operation,
            ASTNodeKind::AST_ADDR
        );
    }

    #[test]
    fn test_integer_id_deref() {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new("global integer *b; global integer a; a = *b;");
        let sym_table: Rc<RefCell<Symtable>> = Rc::new(RefCell::new(Symtable::new()));
        let mut p: Parser = Parser::new(tokener.start_scan(), Rc::clone(&sym_table), unsafe { &mut LABEL_ID });
        // Skipping first two statements. Because, global variable declaration
        // doesn't produce any AST node.
        p.parse_global_variable_decl_stmt();
        p.parse_global_variable_decl_stmt();
        let result: ParseResult = p.parse_single_stmt();
        assert!(result.is_ok());
        let upvalue: &ASTNode = result.as_ref().unwrap();
        assert_eq!(upvalue.operation, ASTNodeKind::AST_ASSIGN);
        assert_eq!(
            (*upvalue.right).as_ref().unwrap().operation,
            ASTNodeKind::AST_LVIDENT
        );
        assert_eq!(
            (*upvalue.left).as_ref().unwrap().operation,
            ASTNodeKind::AST_DEREF
        );
    }

    // Return statement outside a function is not valid!
    #[test]
    #[should_panic]
    fn test_simple_return_stmt_outside_func() {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new("return;");
        let sym_table: Rc<RefCell<Symtable>> = Rc::new(RefCell::new(Symtable::new()));
        let mut p: Parser = Parser::new(tokener.start_scan(), Rc::clone(&sym_table), unsafe { &mut LABEL_ID });
        _ = p.parse_single_stmt();
    }

    #[test]
    fn test_func_decl_stmt() {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new("def main() -> void { return; }");
        let sym_table: Rc<RefCell<Symtable>> = Rc::new(RefCell::new(Symtable::new()));
        let mut p: Parser = Parser::new(tokener.start_scan(), Rc::clone(&sym_table), unsafe { &mut LABEL_ID });
        let func_stmt: ParseResult = p.parse_single_stmt();
        assert!(func_stmt.is_ok());
        let upvalue: &ASTNode = func_stmt.as_ref().unwrap();
        assert_eq!(upvalue.operation, ASTNodeKind::AST_FUNCTION);
        assert_eq!(upvalue.result_type, LitTypeVariant::Void);
        assert_eq!(
            (*upvalue.left).as_ref().unwrap().operation,
            ASTNodeKind::AST_RETURN
        );
        matches!(upvalue.value.as_ref().unwrap(), LitType::I32(_)); // id of function 'main'
    }

    /*
    #[test]
    fn test_non_void_function_without_return_stmt() {
        let mut tokener: Tokenizer = Tokenizer::new("def main() -> integer { global integer a; }");
        let tokens: Vec<Token> = tokener.start_scan();
        let sym_table: Rc<RefCell<Symtable>> = Rc::new(RefCell::new(Symtable::new()));
        let mut p: Parser = Parser::new(&tokens, Rc::clone(&sym_table));
        let result: ParseResult = p.parse_single_stmt();
    }
    */

    #[test]
    fn test_array_decl_stmt_success() {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new("global integer nums[12];");
        let sym_table: Rc<RefCell<Symtable>> = Rc::new(RefCell::new(Symtable::new()));
        let mut p: Parser = Parser::new(tokener.start_scan(), Rc::clone(&sym_table), unsafe { &mut LABEL_ID });
        let array_decl_stmt: Option<ParseError> = p.parse_global_variable_decl_stmt();
        assert!(array_decl_stmt.is_none());
    }
    
    #[test]
    #[should_panic]
    fn test_array_decl_stmt_panic_array_size() {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new("global integer nums[abcd];");
        let sym_table: Rc<RefCell<Symtable>> = Rc::new(RefCell::new(Symtable::new()));
        let mut p: Parser = Parser::new(tokener.start_scan(), Rc::clone(&sym_table), unsafe { &mut LABEL_ID });
        let array_decl_stmt: Option<ParseError> = p.parse_global_variable_decl_stmt();
        assert!(array_decl_stmt.is_none());
    }
    
    #[test]
    #[should_panic]
    fn test_array_decl_stmt_panic_array_no_size_given() {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new("global integer nums[];");
        let sym_table: Rc<RefCell<Symtable>> = Rc::new(RefCell::new(Symtable::new()));
        let mut p: Parser = Parser::new(tokener.start_scan(), Rc::clone(&sym_table), unsafe { &mut LABEL_ID });
        let array_decl_stmt: Option<ParseError> = p.parse_global_variable_decl_stmt();
        assert!(array_decl_stmt.is_none());
    }

    // helper function to parse a statement from string which does not contain variable declaration
    fn parse_single_statement_no_decl(input: &'static str) -> ParseResult {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new(input);
        let sym_table: Rc<RefCell<Symtable>> = Rc::new(RefCell::new(Symtable::new()));
        let mut p: Parser = Parser::new(tokener.start_scan(), Rc::clone(&sym_table), unsafe { &mut LABEL_ID });
        p.parse_single_stmt()
    }

    // helper function to parse a statement from string which may contain one or more variable declarations
    fn parse_single_stmt_with_decl(input: &'static str, decl_count: usize) -> ParseResult {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new(input);
        let sym_table: Rc<RefCell<Symtable>> = Rc::new(RefCell::new(Symtable::new()));
        let mut p: Parser = Parser::new(tokener.start_scan(), Rc::clone(&sym_table), unsafe { &mut LABEL_ID });
        for _ in 0..decl_count {
            p.parse_global_variable_decl_stmt();
        }
        p.parse_single_stmt()
    }

    #[test]
    fn test_array_access_stmt_success() {
        let result: ParseResult = parse_single_stmt_with_decl("global integer nums[12]; global integer value; value = nums[5] + 12;", 2);
        assert!(result.is_ok());
        let node: &ASTNode = result.as_ref().unwrap();
        let expr_node: &ASTNode = (*node.left).as_ref().unwrap();
        let array_access: &ASTNode = (*expr_node.left).as_ref().unwrap();
        assert_eq!(array_access.operation, ASTNodeKind::AST_ARRAY_ACCESS);
    }

    #[test]
    fn test_return_stmt_inside_non_void_function() {
        let func_stmt: ParseResult = parse_single_statement_no_decl("def main() -> integer { return 1234; }");
        assert!(func_stmt.is_ok());
        let upvalue: &ASTNode = func_stmt.as_ref().unwrap();
        assert_eq!(upvalue.operation, ASTNodeKind::AST_FUNCTION);
        assert_eq!(upvalue.result_type, LitTypeVariant::I32);
        let left_node: &ASTNode = (*upvalue.left).as_ref().unwrap();
        assert_eq!(left_node.operation, ASTNodeKind::AST_RETURN);
        assert_eq!(
            (*left_node.left).as_ref().unwrap().operation,
            ASTNodeKind::AST_INTLIT
        ); // function should return an integer literal
        matches!(upvalue.value.as_ref().unwrap(), LitType::I32(_)); // id of function 'main'
    }
}