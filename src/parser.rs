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

use crate::ast::ASTKind;
use crate::ast::ASTOperation;
use crate::ast::BinExpr;
use crate::ast::Expr;
use crate::ast::FuncCallExpr;
use crate::ast::FuncDeclStmt;
use crate::ast::IdentExpr;
use crate::ast::LitValExpr;
use crate::ast::ReturnStmt;
use crate::ast::Stmt;
use crate::ast::SubscriptExpr;
use crate::ast::AST;
use crate::enums::*;
use crate::error;
use crate::function::FunctionInfo;
use crate::function::FunctionInfoTable;
use crate::symtable;
use crate::symtable::*;
use crate::tokenizer::*;
use crate::types;
use crate::types::*;

#[derive(Eq, PartialEq, Debug, Clone)]
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
            ParseError::NotCallable(token) => {
                error::report_errornous_token_and_exit(token, &format!("'{}' is not callable", token.lexeme), 1);
                Ok(())
            }
            ParseError::GlobalInsideFunction(token) => {
                error::report_errornous_token_and_exit(token, "global variable declaration inside a function", 1);
                Ok(())
            }
            ParseError::UnsubscritableToken(token) => {
                error::report_errornous_token_and_exit(token, &format!("'{}' is not subscriptable", token.lexeme), 1);
                Ok(())
            }
            ParseError::SymbolNotFound(token) => {
                error::report_errornous_token_and_exit(token, &format!("Symbol is not defined: '{}'", token.lexeme), 1);
                Ok(())
            }
            _ => write!(f, "other"),
        }
    }
}

type ParseResult = Result<AST, ParseError>;

// Actual parser
pub struct Parser<'a> {
    tokens: Vec<Token>,
    current: usize,
    current_token: Token,
    main_sym_table: &'a mut Symtable, // symbol table for global identifiers
    // ID of a function that is presently being parsed. This field's value is 0xFFFFFFFF
    // if the parser is not inside a function.
    func_info_table: &'a mut FunctionInfoTable,
    current_function_id: usize,
    _label_id: &'static mut usize, // label generator
    local_offset: i32, // offset of a new local variable
    next_global_sym_pos: usize, // position of next global symbol
    next_local_sym_pos: usize, // position of next local symbol
}

impl<'a> Parser<'a> {
    #[inline]
    pub fn new(
        tokens: Vec<Token>, 
        main_sym_table: &'a mut Symtable, 
        func_info_table: &'a mut FunctionInfoTable, 
        label_id: &'static mut usize
    ) -> Self {
        let current_token: Token = tokens[0].clone();
        Self {
            tokens,
            current: 0,
            current_token,
            main_sym_table,
            func_info_table,
            current_function_id: 0xFFFFFFFF,
            _label_id: label_id,
            local_offset: 0,
            next_global_sym_pos: 0,
            next_local_sym_pos: NSYMBOLS - 1
        }
    }

    pub fn parse(&mut self) -> Vec<AST> {
        let mut nodes: Vec<AST> = vec![];
        loop {
            if self.current_token.kind == TokenKind::T_EOF {
                break;
            }
            let stmt_parse_result: ParseResult = self.parse_single_stmt();
            if let Ok(stmt) = stmt_parse_result {
                nodes.push(stmt);
            } else if let Some(parse_error) = stmt_parse_result.err() {
                if parse_error != ParseError::None {
                    panic!("{parse_error}");
                }
            }
        }
        self.dump_globals();
        // .text section starts from here
        println!("\n.text");
        nodes
    }

    fn dump_globals(&self) {
        for symbol in self.main_sym_table.iter() {
            // symbol information is not generated if any of the following conditions matches
            let not_process_cond: Vec<bool> = vec![symbol.sym_type == SymbolType::Function, symbol.lit_type == LitTypeVariant::None, symbol.class == StorageClass::LOCAL];
            if not_process_cond.iter().any(|item| *item) {
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
            TokenKind::KW_GLOBAL => self.parse_var_decl_stmt(StorageClass::GLOBAL),
            TokenKind::KW_LOCAL => self.parse_var_decl_stmt(StorageClass::LOCAL),
            TokenKind::T_IDENTIFIER => self.parse_assignment_stmt(),
            TokenKind::KW_IF => self.parse_if_stmt(),
            TokenKind::KW_WHILE => self.parse_while_stmt(),
            TokenKind::KW_FOR => self.parse_for_stmt(),
            TokenKind::T_LBRACE => self.parse_compound_stmt(),
            TokenKind::KW_DEF => self.parse_function_stmt(),
            TokenKind::KW_RETURN => self.parse_return_stmt(),
            TokenKind::T_EOF => Err(ParseError::UnexpectedToken(self.current_token.clone())),
            _ => {
                let result: ParseResult = self.parse_equality();
                self.token_match(TokenKind::T_SEMICOLON);
                result
            }
        }
    }

    // parse compound statement(statement starting with '{' and ending with '}')
    fn parse_compound_stmt(&mut self) -> ParseResult {
        _ = self.token_match(TokenKind::T_LBRACE);
        let mut left: Option<AST> = None;
        loop {
            let tree: ParseResult = self.parse_single_stmt();
            if tree.is_ok() {
                if left.is_none() {
                    left = tree.ok();
                } else {
                    left = Some(AST::new(
                        ASTKind::StmtAST(Stmt::Glue),
                        ASTOperation::AST_GLUE,
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
        let function_id: Option<usize> = self.add_symbol_global(Symbol::new(
            id_token.lexeme.clone(),
            func_return_type,
            SymbolType::Function,
            crate::symtable::StorageClass::GLOBAL
        ));
        if function_id.is_none() {
            panic!("Symbol already defined: '{:?}'", id_token.lexeme);
        }
        self.current_function_id = function_id.unwrap();
        // function body
        let function_body: ParseResult = self.parse_compound_stmt();
        let temp_func_id: usize = self.current_function_id;
        self.current_function_id = 0xFFFFFFFF; // exiting out of function body
        // function information collection
        let stack_offset: i32 = (self.local_offset + 15) & !15;
        let func_info: FunctionInfo = FunctionInfo::new(id_token.lexeme.clone(), function_id.unwrap(), stack_offset, func_return_type);
        self.func_info_table.add(func_info);
        // reset offset counter after parsing a function
        self.local_offset = 0; 
        Ok(AST::new(
            ASTKind::StmtAST(Stmt::FuncDecl(FuncDeclStmt{func_id: temp_func_id})),
            ASTOperation::AST_FUNCTION,
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
            let sym: Option<Symbol> = Some(self.main_sym_table.get_symbol(self.current_function_id).unwrap().clone());
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
            return Ok(AST::create_leaf(
                ASTKind::StmtAST(Stmt::Return(
                    ReturnStmt { func_id: self.current_function_id }
                )),
                ASTOperation::AST_RETURN,
                None,
                LitTypeVariant::Void,
            ));
        }
        let return_expr: ParseResult = self.parse_equality();
        let return_expr_type: LitTypeVariant = return_expr.as_ref().unwrap().result_type;
        if return_expr_type != func_symbol.as_ref().unwrap().lit_type {
            panic!("Return value's type does not match function's return type.");
        }
        _ = self.token_match(TokenKind::T_SEMICOLON); // expect semicolon to end a return statement
        Ok(AST::new(
            ASTKind::StmtAST(Stmt::Return(ReturnStmt{func_id: self.current_function_id})),
            ASTOperation::AST_RETURN,
            return_expr.ok(),
            None,
            None,
            return_expr_type,
        ))
    }

    fn parse_while_stmt(&mut self) -> ParseResult {
        let cond_ast: ParseResult = self.parse_conditional_stmt(TokenKind::KW_WHILE);
        let while_body: ParseResult = self.parse_single_stmt();
        Ok(AST::new(
            ASTKind::StmtAST(Stmt::While),
            ASTOperation::AST_WHILE,
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
            if (_icast.operation < ASTOperation::AST_EQEQ)
                || (_icast.operation > ASTOperation::AST_LTHAN)
            {
                // if operation kind is not "relational operation"
                panic!("Please provide conditional expression for 'for'");
            }
        }
        _ = self.token_match(TokenKind::T_SEMICOLON); // expect semicolon
        let incr_ast: ParseResult = self.parse_single_stmt();
        _ = self.token_match(TokenKind::T_RPAREN); // match and ignore ')'
        let for_body: ParseResult = self.parse_single_stmt();
        let mut tree: AST = AST::new(
            ASTKind::StmtAST(Stmt::Glue),
            ASTOperation::AST_GLUE,
            for_body.ok(),
            incr_ast.ok(),
            None,
            LitTypeVariant::None,
        );
        tree = AST::new(
            ASTKind::StmtAST(Stmt::While),
            ASTOperation::AST_WHILE,
            cond_ast.ok(),
            Some(tree),
            None,
            LitTypeVariant::None,
        );
        Ok(AST::new(
            ASTKind::StmtAST(Stmt::Glue),
            ASTOperation::AST_GLUE,
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
        Ok(AST::with_mid(
            ASTKind::StmtAST(Stmt::If),
            ASTOperation::AST_IF,
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
            if (_icast.operation < ASTOperation::AST_EQEQ)
                || (_icast.operation > ASTOperation::AST_LTHAN)
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

    fn parse_var_decl_stmt(&mut self, var_class: symtable::StorageClass) -> ParseResult {
        let inside_func: bool = self.current_function_id != 0xFFFFFFFF;
        match var_class {
            StorageClass::LOCAL => {
                assert!(inside_func, "local variable outside a function???");
                _ = self.token_match(TokenKind::KW_LOCAL);
            },
            StorageClass::GLOBAL => {
                assert!(!inside_func, "global variable inside a function???");
                _ = self.token_match(TokenKind::KW_GLOBAL);
            }
            StorageClass::PARAM => {
                unimplemented!()
            }
        }
        let mut sym: Symbol = Symbol::uninit();
        let sym_id_type: LitTypeVariant = self.parse_id_type();
        if sym_id_type == LitTypeVariant::None {
            panic!(
                "Can't create variable of type {:?}",
                self.current_token.kind
            );
        }
        let id_token: Token = self.token_match(TokenKind::T_IDENTIFIER).clone();
        // CHECK IF THE VARIABLE EXISTS IN THE SAME SCOPE ALREADY!!!
        // symbol info
        sym.class = var_class;
        sym.lit_type = sym_id_type;
        sym.name = id_token.lexeme.clone();
        sym.size = 1;
        // if it is going to be an array type
        if self.current_token.kind == TokenKind::T_LBRACKET {
            return self.parse_array_var_decl_stmt(sym);
        }
        // checking whether variable is assigned at the time of it's declaration
        let mut assignment_parse_res: Option<ParseResult> = None;
        if self.current_token.kind == TokenKind::T_EQUAL { // if identifier name is followed by an equal sign
            _ = self.token_match(TokenKind::T_EQUAL); // match and ignore '=' sign
            assignment_parse_res = Some(self.parse_equality());
        }
        if let Some(Err(parse_error)) = assignment_parse_res {
            return Err(parse_error);
        }
        // test assignability
        let mut return_result: ParseResult = Err(ParseError::None); // this indicates variable is declared without assignment
        // ***** NOTE *****: I am assuming that the symbol is added without any problem!!!
        let symbol_add_pos: usize = if inside_func {
            self.add_symbol_local(sym.clone()).unwrap()
        } else {
            self.add_symbol_global(sym.clone()).unwrap()
        };
        if let Some(assign_ast_node_res) = assignment_parse_res {
            let mut assign_ast_node: AST = assign_ast_node_res?;
            let compat_node: Option<AST> = Parser::validate_assign_compatibility(&sym, &mut assign_ast_node);
            let _result_type: LitTypeVariant = compat_node.as_ref().unwrap().result_type;
            if inside_func {
                sym.local_offset = self.gen_next_local_offset(_result_type);
            }
            let lvalueid: AST = AST::create_leaf(
                ASTKind::StmtAST(Stmt::LValue(symbol_add_pos)),
                ASTOperation::AST_LVIDENT,
                None,
                sym.lit_type,
            );
            // calculate offset here
            return_result = Ok(AST::new(
                ASTKind::StmtAST(Stmt::Assignment),
                ASTOperation::AST_ASSIGN,
                compat_node,
                Some(lvalueid),
                None,
                _result_type,
            ));
        }
        _ = self.token_match(TokenKind::T_SEMICOLON);
        return_result
    }

    fn gen_next_local_offset(&mut self, var_type: LitTypeVariant) -> i32 {
        let temp_offset: i32 = if var_type.size() > 4 {
            var_type.size() as i32
        } else { 4 };
        self.local_offset += temp_offset;
        self.local_offset
    }

    fn parse_array_var_decl_stmt(&mut self, mut sym: Symbol) -> ParseResult {
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
        if sym.class == StorageClass::LOCAL {
            self.add_symbol_local(sym);
        } else {
            self.add_symbol_global(sym);
        }
        Err(ParseError::None) // this indicates variable is declared without assignment
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
    
    fn parse_assignment_stmt(&mut self) -> ParseResult {
        let id_token: Token = self.token_match(TokenKind::T_IDENTIFIER).clone();
        let symbol_info: Option<(usize, StorageClass)> = self.find_symbol(&id_token.lexeme);
        if symbol_info.is_none() {
            return Err(ParseError::SymbolNotFound(id_token));
        }
        let id_index_symt: usize = symbol_info.unwrap().0;
        let symbol: Symbol = self.main_sym_table.get_symbol(id_index_symt).unwrap().clone();
        // Check if we are assigning to a type other than SymbolType::Variable. If yes, panic!
        if symbol.sym_type != SymbolType::Variable {
            panic!("Assigning to type '{:?}' is not allowed!", symbol.sym_type);
        }
        _ = self.token_match(TokenKind::T_EQUAL);
        let mut bin_expr_ast_node: AST = self.parse_equality()?;
        let compat_node: Option<AST> = Parser::validate_assign_compatibility(&symbol, &mut bin_expr_ast_node);
        // following code is going to break at some point. unwrap()ing an Option type without checking?
        let _result_type: LitTypeVariant = compat_node.as_ref().unwrap().result_type;
        _ = self.token_match(TokenKind::T_SEMICOLON);
        let lvalueid: AST = AST::create_leaf(
            ASTKind::StmtAST(Stmt::LValue(id_index_symt)),
            ASTOperation::AST_LVIDENT,
            None,
            symbol.lit_type,
        );
        Ok(AST::new(
            ASTKind::StmtAST(Stmt::Assignment),
            ASTOperation::AST_ASSIGN,
            compat_node,
            Some(lvalueid),
            None,
            _result_type,
        ))
    }

    fn validate_assign_compatibility(symbol: &Symbol, node: &mut AST) -> Option<AST> {
        let result_type: LitTypeVariant = node.result_type;
        let compat_node: Option<AST> = types::modify_ast_node_type(node, symbol.lit_type, ASTOperation::AST_NONE);
        if compat_node.is_none() {
            panic!(
                "Can't assign value of type '{:?}' to variable of type: '{:?}'",
                result_type, symbol.lit_type
            );
        }
        compat_node
    }

    fn parse_equality(&mut self) -> ParseResult {
        let left: ParseResult = self.parse_comparision();
        loop {
            // repeatedly parse expression
            let a: i32 = 23;
            if a == 23 { break; }
        }
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
        left_side_tree: ParseResult, 
        tokens: Vec<TokenKind>
    ) -> ParseResult {
        let left: AST = left_side_tree.clone()?;
        let current_token_kind: TokenKind = self.current_token.kind;
        if !tokens.contains(&current_token_kind) {
            return left_side_tree;
        }
        self.skip_to_next_token(); // skip the operator
        let ast_op: ASTOperation = ASTOperation::from_token_kind(current_token_kind);
        let right: AST = self.parse_mem_prefix()?;
        let modif_left_node: Option<AST> = types::modify_ast_node_type(
            &mut AST::create_leaf(
                left.kind, 
                ast_op, 
                None, 
                left.result_type
            ), 
            right.result_type,
            ast_op
        );
        let modif_right_node: Option<AST> = types::modify_ast_node_type(
            &mut AST::create_leaf(
                    right.kind, 
                    ast_op, 
                    None, 
                    right.result_type
                ), 
            left.result_type, 
            ast_op
        );
        if modif_left_node.is_none() && modif_right_node.is_none() {
            panic!(
                "Incompatible types: '{:?}' and '{:?}' for operator '{:?}'",
                left.result_type, right.result_type, ast_op
            );
        }
        let result_type: LitTypeVariant = modif_left_node.as_ref().unwrap().result_type;
        Ok(AST::create_leaf(
                ASTKind::ExprAST(
                    Expr::Binary(
                        BinExpr { 
                            operation: ast_op, 
                            left: Box::new(modif_left_node.unwrap().kind.unwrap_expr()), 
                            right: Box::new(modif_right_node.unwrap().kind.unwrap_expr()), 
                            result_type
                        }
                    )
                ),
                ast_op,
                None,
                result_type
            )
        )
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
                    if add_tree.operation != ASTOperation::AST_IDENT {
                        panic!("Can't take address of '{:?}'", add_tree);
                    }
                    add_tree.operation = ASTOperation::AST_ADDR;
                    add_tree.result_type = LitTypeVariant::pointer_type(add_tree.result_type);
                }
                tree
            }
            TokenKind::T_STAR => {
                // same as '&'; parsing multiple '*' but not supporting it(yet)
                self.skip_to_next_token(); // skip '*'
                let mut tree: ParseResult = self.parse_mem_prefix();
                if let Ok(ref mut add_tree) = tree {
                    if add_tree.operation != ASTOperation::AST_IDENT {
                        panic!("Can't dereference type '{:?}'", add_tree.value);
                    }
                    add_tree.operation = ASTOperation::AST_DEREF;
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
            TokenKind::T_INT_NUM => Ok(Parser::create_expr_ast(LitType::I32(current_token.lexeme.parse::<i32>().unwrap()), ASTOperation::AST_INTLIT)),
            TokenKind::T_CHAR => Ok(Parser::create_expr_ast(LitType::U8(current_token.lexeme.parse::<u8>().unwrap()), ASTOperation::AST_INTLIT)),
            TokenKind::T_LONG_NUM => Ok(Parser::create_expr_ast(LitType::I64(current_token.lexeme.parse::<i64>().unwrap()), ASTOperation::AST_INTLIT)),
            TokenKind::T_FLOAT_NUM | TokenKind::T_DOUBLE_NUM => Ok(Parser::create_expr_ast(LitType::F64(current_token.lexeme.parse::<f64>().unwrap()), ASTOperation::AST_INTLIT)),
            TokenKind::T_STRING => {
                let str_label: usize = *self._label_id;
                *self._label_id += 1;
                let str_const_symbol: Symbol = Symbol::new(format!("_L{}---{}", str_label, current_token.lexeme.clone()), LitTypeVariant::U8Ptr, SymbolType::Constant, StorageClass::GLOBAL);
                self.add_symbol_global(str_const_symbol);
                println!("_L{}: .ascii \"{}\"", str_label, current_token.lexeme);
                Ok(AST::create_leaf(
                    ASTKind::ExprAST(Expr::LitVal(LitValExpr{
                        value: LitType::Str(current_token.lexeme.clone()),
                        result_type: LitTypeVariant::U8Ptr
                    })),
                    ASTOperation::AST_STRLIT, 
                    None,
                    LitTypeVariant::U8Ptr
                ))
            }
            TokenKind::T_IDENTIFIER => {
                let sym_find_res: Option<usize> = if self.is_scope_func() {
                    Some(self.find_symbol(&current_token.lexeme).unwrap().0)
                } else {
                    self.find_symbol_global(&current_token.lexeme)
                };
                if sym_find_res.is_none() {
                    return Err(ParseError::SymbolNotFound(current_token));
                }
                let id_index: usize = sym_find_res.unwrap();
                let symbol: Symbol = self.main_sym_table.get_symbol(id_index).unwrap().clone();
                let curr_tok_kind: TokenKind = self.current_token.kind;
                if curr_tok_kind == TokenKind::T_LPAREN {
                    self.parse_func_call_expr(&symbol, id_index, &current_token)
                } else if curr_tok_kind == TokenKind::T_LBRACKET {
                    self.parse_array_index_expr(&symbol, id_index, &current_token)
                } else {
                    Ok(AST::create_leaf(
                        ASTKind::ExprAST(Expr::Ident(IdentExpr{
                            symtbl_pos: id_index,
                            result_type: symbol.lit_type
                        })),
                        ASTOperation::AST_IDENT,
                        None,
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

    fn create_expr_ast(value: LitType, operation: ASTOperation) -> AST {
        AST::create_leaf(
            ASTKind::ExprAST(
                Expr::LitVal(LitValExpr{
                    value: value.clone(),
                    result_type: value.variant()
                }
            )),
            operation,
            None,
            value.variant()
        )
    }

    fn parse_array_index_expr(
        &mut self,
        indexed_symbol: &Symbol,
        sym_index: usize,
        sym_token: &Token,
    ) -> ParseResult {
        _ = self.token_match(TokenKind::T_LBRACKET);
        let array_access_expr_result: ParseResult = self.parse_equality();
        #[allow(clippy::question_mark)]
        if array_access_expr_result.is_err() {
            return array_access_expr_result;
        }
        let array_access_expr: AST = array_access_expr_result.ok().unwrap();
        if indexed_symbol.sym_type != SymbolType::Array {
            return Err(ParseError::UnsubscritableToken(sym_token.clone()));
        }
        _ = self.token_match(TokenKind::T_RBRACKET);
        Ok(AST::create_leaf(
            ASTKind::ExprAST(Expr::Subscript(SubscriptExpr{
                index: Box::new(array_access_expr.kind.unwrap_expr()),
                symtbl_pos: sym_index,
                result_type: indexed_symbol.lit_type
            })),
            ASTOperation::AST_ARRAY_ACCESS,
            None,
            indexed_symbol.lit_type,
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
        // Allocating extra 16 bytes to store x29(Frame Pointer), and x30(Link Register).
        // Storing and then loading them is collectively called a Frame Record.
        self.local_offset += 16; 
        Ok(AST::create_leaf(
            ASTKind::ExprAST(Expr::FuncCall(FuncCallExpr{
                symtbl_pos: sym_index,
                result_type: called_symbol.lit_type
            })),
            ASTOperation::AST_FUNC_CALL,
            None,
            called_symbol.lit_type,
        ))
    }

    fn add_symbol_global(&mut self, sym: Symbol) -> Option<usize> {
        let insert_pos: Option<usize> = self.main_sym_table.insert(self.next_global_sym_pos, sym);
        self.next_global_sym_pos += 1;
        insert_pos
    }
    
    fn add_symbol_local(&mut self, sym: Symbol) -> Option<usize> {
        let insert_pos: Option<usize> = self.main_sym_table.insert(self.next_local_sym_pos, sym);
        self.next_local_sym_pos -= 1;
        insert_pos
    }

    fn find_symbol(&self, name: &str) -> Option<(usize, StorageClass)> {
        // first search in the local scope to determine if a symbol exists in this scope
        if let Some(global_pos) = self.find_symbol_local(name) {
            return Some((global_pos, StorageClass::LOCAL));
        }
        // then inquire global scope
        if let Some(local_pos) = self.find_symbol_global(name) {
            return Some((local_pos, StorageClass::GLOBAL));
        }
        None
    }

    fn find_symbol_global(&self, name: &str) -> Option<usize> {
        for index in 0..self.next_global_sym_pos {
            if let Some(symbol) = self.main_sym_table.get_symbol(index) {
                if symbol.name == name {
                    return Some(index);
                }
            }
        }
        None
    }

    fn find_symbol_local(&self, name: &str) -> Option<usize> {
        for index in (self.next_local_sym_pos+1)..NSYMBOLS {
            if let Some(symbol) = self.main_sym_table.get_symbol(index) {
                if symbol.name == name {
                    return Some(index);
                }
            }
        }
        None
    }

    fn is_scope_global(&self) -> bool {
        self.current_function_id == 0xFFFFFFFF
    }

    fn is_scope_func(&self) -> bool {
        self.current_function_id != 0xFFFFFFFF
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
    use crate::ast::ASTOperation;

    use super::*;

    #[test]
    fn test_group_expression_tree_structure() {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new("(5 + (3 * 4))");
        let mut sym_table: Symtable = Symtable::new();
        let mut func_table: FunctionInfoTable = FunctionInfoTable::new();
        let mut p: Parser = Parser::new(tokener.start_scan(), &mut sym_table, &mut func_table, unsafe { &mut LABEL_ID });
        let result: ParseResult = p.parse_equality();
        assert!(result.is_ok());
        let upvalue: AST = result.unwrap();
        let left_tree = upvalue.left.as_ref().unwrap();
        let right_tree: &AST = upvalue.right.as_ref().unwrap();
        assert_eq!(upvalue.operation, ASTOperation::AST_ADD);
        assert_eq!(left_tree.operation, ASTOperation::AST_INTLIT);
        assert_eq!(right_tree.operation, ASTOperation::AST_MULTIPLY);
    }

    // test addition operation
    #[test]
    fn test_depth_one_bin_tree() {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new("5+5");
        let mut sym_table: Symtable = Symtable::new();
        let mut func_table: FunctionInfoTable = FunctionInfoTable::new();
        let mut p: Parser = Parser::new(tokener.start_scan(), &mut sym_table, &mut func_table, unsafe { &mut LABEL_ID });
        let result: ParseResult = p.parse_equality();
        assert!(result.is_ok());
        assert_eq!(result.unwrap().operation, ASTOperation::AST_ADD);
    }

    // test if-else block
    #[test]
    fn test_if_else_statement_block() {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new("if (4 > 5) { global integer a; } else { global integer b; }");
        let mut sym_table: Symtable = Symtable::new();
        let mut func_table: FunctionInfoTable = FunctionInfoTable::new();
        let mut p: Parser = Parser::new(tokener.start_scan(), &mut sym_table, &mut func_table, unsafe { &mut LABEL_ID });
        let result: ParseResult = p.parse_if_stmt();
        assert!(
            result.is_ok(),
            "If this assertion did not pass, then if-else block is probably malformed."
        );
        let upvalue: &AST = result.as_ref().unwrap();
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
        assert_eq!(upvalue.operation, ASTOperation::AST_IF); // main AST node is of AST_IF type
        assert_eq!(
            upvalue.left.as_ref().unwrap().operation,
            ASTOperation::AST_GTHAN,
            "Unexpected ASTOperation; expected be ASTOperation::AST_GTHAN."
        );
    }

    // If following two tests pass, then we can conclude that every other pointer type declarations,
    // dereferencing, and addressing will work.
    #[test]
    fn test_integer_id_addr_load() {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new("global integer *b; global integer a; b = &a;");
        let mut sym_table: Symtable = Symtable::new();
        let mut func_table: FunctionInfoTable = FunctionInfoTable::new();
        let mut p: Parser = Parser::new(tokener.start_scan(), &mut sym_table, &mut func_table, unsafe { &mut LABEL_ID });
        _ = p.parse_var_decl_stmt(StorageClass::GLOBAL);
        _ = p.parse_var_decl_stmt(StorageClass::GLOBAL);
        let result: ParseResult = p.parse_single_stmt();
        assert!(result.is_ok());
        let upvalue: &AST = result.as_ref().unwrap();
        assert_eq!(upvalue.operation, ASTOperation::AST_ASSIGN);
        assert_eq!(
            upvalue.right.as_ref().unwrap().operation,
            ASTOperation::AST_LVIDENT
        );
        assert_eq!(
            upvalue.left.as_ref().unwrap().operation,
            ASTOperation::AST_ADDR
        );
    }

    #[test]
    fn test_integer_id_deref() {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new("global integer *b; global integer a; a = *b;");
        let mut sym_table: Symtable = Symtable::new();
        let mut func_table: FunctionInfoTable = FunctionInfoTable::new();
        let mut p: Parser = Parser::new(tokener.start_scan(), &mut sym_table, &mut func_table, unsafe { &mut LABEL_ID });
        // Skipping first two statements. Because, global variable declaration
        // doesn't produce any AST node.
        _ = p.parse_var_decl_stmt(StorageClass::GLOBAL);
        _ = p.parse_var_decl_stmt(StorageClass::GLOBAL);
        let result: ParseResult = p.parse_single_stmt();
        assert!(result.is_ok());
        let upvalue: &AST = result.as_ref().unwrap();
        assert_eq!(upvalue.operation, ASTOperation::AST_ASSIGN);
        assert_eq!(
            upvalue.right.as_ref().unwrap().operation,
            ASTOperation::AST_LVIDENT
        );
        assert_eq!(
            upvalue.left.as_ref().unwrap().operation,
            ASTOperation::AST_DEREF
        );
    }

    // Return statement outside a function is not valid!
    #[test]
    #[should_panic]
    fn test_simple_return_stmt_outside_func() {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new("return;");
        let mut sym_table: Symtable = Symtable::new();
        let mut func_table: FunctionInfoTable = FunctionInfoTable::new();
        let mut p: Parser = Parser::new(tokener.start_scan(), &mut sym_table, &mut func_table, unsafe { &mut LABEL_ID });
        _ = p.parse_single_stmt();
    }

    #[test]
    fn test_func_decl_stmt() {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new("def main() -> void { return; }");
        let mut sym_table: Symtable = Symtable::new();
        let mut func_table: FunctionInfoTable = FunctionInfoTable::new();
        let mut p: Parser = Parser::new(tokener.start_scan(), &mut sym_table, &mut func_table, unsafe { &mut LABEL_ID });
        let func_stmt: ParseResult = p.parse_single_stmt();
        assert!(func_stmt.is_ok());
        let upvalue: &AST = func_stmt.as_ref().unwrap();
        assert_eq!(upvalue.operation, ASTOperation::AST_FUNCTION);
        assert_eq!(upvalue.result_type, LitTypeVariant::Void);
        assert_eq!(
            upvalue.left.as_ref().unwrap().operation,
            ASTOperation::AST_RETURN
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
        let mut sym_table: Symtable = Symtable::new();
        let mut func_table: FunctionInfoTable = FunctionInfoTable::new();
        let mut p: Parser = Parser::new(tokener.start_scan(), &mut sym_table, &mut func_table, unsafe { &mut LABEL_ID });
        let array_decl_stmt: ParseResult = p.parse_var_decl_stmt(StorageClass::GLOBAL);
        assert!(array_decl_stmt.is_err()); // ParseError::None
    }
    
    #[test]
    #[should_panic]
    fn test_array_decl_stmt_panic_array_size() {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new("global integer nums[abcd];");
        let mut sym_table: Symtable = Symtable::new();
        let mut func_table: FunctionInfoTable = FunctionInfoTable::new();
        let mut p: Parser = Parser::new(tokener.start_scan(), &mut sym_table, &mut func_table, unsafe { &mut LABEL_ID });
        let array_decl_stmt: ParseResult = p.parse_var_decl_stmt(StorageClass::GLOBAL);
        assert!(array_decl_stmt.is_err()); // ParseError::None
    }
    
    #[test]
    #[should_panic]
    fn test_array_decl_stmt_panic_array_no_size_given() {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new("global integer nums[];");
        let mut sym_table: Symtable = Symtable::new();
        let mut func_table: FunctionInfoTable = FunctionInfoTable::new();
        let mut p: Parser = Parser::new(tokener.start_scan(), &mut sym_table, &mut func_table, unsafe { &mut LABEL_ID });
        let array_decl_stmt: ParseResult = p.parse_var_decl_stmt(StorageClass::GLOBAL);
        assert!(array_decl_stmt.is_err()); // ParseError::None
    }

    // helper function to parse a statement from string which does not contain variable declaration
    fn parse_single_statement_no_decl(input: &'static str) -> ParseResult {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new(input);
        let mut sym_table: Symtable = Symtable::new();
        let mut func_table: FunctionInfoTable = FunctionInfoTable::new();
        let mut p: Parser = Parser::new(tokener.start_scan(), &mut sym_table, &mut func_table, unsafe { &mut LABEL_ID });
        p.parse_single_stmt()
    }

    // helper function to parse a statement from string which may contain one or more variable declarations
    fn parse_single_stmt_with_decl(input: &'static str, decl_count: usize) -> ParseResult {
        static mut LABEL_ID: usize = 0;
        let mut tokener: Tokenizer = Tokenizer::new(input);
        let mut sym_table: Symtable = Symtable::new();
        let mut func_table: FunctionInfoTable = FunctionInfoTable::new();
        let mut p: Parser = Parser::new(tokener.start_scan(), &mut sym_table, &mut func_table, unsafe { &mut LABEL_ID });
        for _ in 0..decl_count {
            _ = p.parse_var_decl_stmt(StorageClass::GLOBAL);
        }
        p.parse_single_stmt()
    }

    #[test]
    fn test_array_access_stmt_success() {
        let result: ParseResult = parse_single_stmt_with_decl("global integer nums[12]; global integer value; value = nums[5] + 12;", 2);
        assert!(result.is_ok());
        let node: &AST = result.as_ref().unwrap();
        let expr_node: &AST = node.left.as_ref().unwrap();
        let array_access: &AST = expr_node.left.as_ref().unwrap();
        assert_eq!(array_access.operation, ASTOperation::AST_ARRAY_ACCESS);
    }

    #[test]
    fn test_return_stmt_inside_non_void_function() {
        let func_stmt: ParseResult = parse_single_statement_no_decl("def main() -> integer { return 1234; }");
        assert!(func_stmt.is_ok());
        let upvalue: &AST = func_stmt.as_ref().unwrap();
        assert_eq!(upvalue.operation, ASTOperation::AST_FUNCTION);
        assert_eq!(upvalue.result_type, LitTypeVariant::I32);
        let left_node: &AST = upvalue.left.as_ref().unwrap();
        assert_eq!(left_node.operation, ASTOperation::AST_RETURN);
        assert_eq!(
            left_node.left.as_ref().unwrap().operation,
            ASTOperation::AST_INTLIT
        ); // function should return an integer literal
        matches!(upvalue.value.as_ref().unwrap(), LitType::I32(_)); // id of function 'main'
    }
}