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

use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{
        ASTKind, ASTOperation, Expr, Stmt, AST
    }, 
    context::CompilerCtx, 
    types::LitTypeVariant, 
    Symbol
};

use super::{
    sa_errors::{SAError, SATypeError}, 
    sa_types::SAResult, 
    type_checker::TypeChecker
};

#[allow(non_camel_case_types)]
enum _InternalSAErrType {
    __ISET_UndefinedSymbol__ {
        name: String // name of the symbol
    }
}

pub struct SemanticAnalyzer<'sa> {
    pub ctx: Rc<RefCell<CompilerCtx<'sa>>>,
}

impl<'sa> SemanticAnalyzer<'sa> {
    pub fn new(ctx: Rc<RefCell<CompilerCtx<'sa>>>) -> Self {
        Self {
            ctx,
        }
    }

    /// Start the analysis process
    /// 
    /// This starts an analysis process for the given list of nodes. 
    /// This function panics if it encounters any form of error.
    pub fn start_analysis(&mut self, nodes: &mut Vec<AST>) {
        for node in nodes {
            let result: SAResult = self.analyze_node(node, ASTOperation::AST_NONE);
            if let Err(analysis_err) = result {
                analysis_err.dump();
            }
        }
    }

    fn analyze_node(&mut self, node: &mut AST, parent_ast_op: ASTOperation) -> SAResult {
        match node.operation {
            ASTOperation::AST_VAR_DECL => self.analyze_var_decl_stmt(node),
            ASTOperation::AST_FUNCTION => self.analyze_func_decl_stmt(node),
            ASTOperation::AST_FUNC_CALL => self.analyze_func_call(node),
            ASTOperation::AST_GLUE => {
                if let Some(left) = &mut node.left {
                    self.analyze_node(left, parent_ast_op)?;
                }
                if let Some(right) = &mut node.right {
                    self.analyze_node(right, parent_ast_op)?;
                }
                Ok(())
            }
            _ => panic!("'{:?}' is not supported ASTOperation for 'analyze_node' yet!", node.operation)
        }
    }

    fn analyze_func_decl_stmt(&mut self, node: &mut AST) -> SAResult {
        let func_id: usize = match &node.kind {
            ASTKind::StmtAST(stmt_ast) => {
                match stmt_ast {
                    Stmt::FuncDecl(func_decl_stmt) => {
                        func_decl_stmt.func_id
                    },
                    _ => panic!("not a function declaration statement")
                }
            },
            _ => panic!("not a statement")
        };

        let mut ctx_borrow = self.ctx.borrow_mut();
        let func_name: String = ctx_borrow.sym_table.get_symbol(func_id).unwrap().name.clone();
        if let Some(_finfo) = ctx_borrow.func_table.get(&func_name) {
            ctx_borrow.switch_to_func_scope(func_id);
        } else {
            panic!("Function '{}' not found", func_name);
        }
        drop(ctx_borrow);

        if let Some(func_body) = &mut node.left {
            return self.analyze_node(func_body, ASTOperation::AST_FUNCTION);
        }
        self.ctx.borrow_mut().switch_to_global_scope();
        Ok(())
    }

    /// Analyze the function call expression
    /// 
    /// This analysis is done to check if the arguments to this 
    /// function call are valid.
    fn analyze_func_call(&self, node: &mut AST) -> SAResult {
        let func_name: String = if let ASTKind::ExprAST(Expr::FuncCall(func_call_expr)) = &mut node.kind {
            func_call_expr.symbol_name.to_string()
        }
        else {
            panic!("Not a function call expression")
        };
        let ctx_borrow = self.ctx.borrow();
        if let Some(func_sym_pos) = ctx_borrow.sym_table.find_symbol(&func_name) {
            if let Some(func_sym) = ctx_borrow.sym_table.get_symbol(func_sym_pos) {
                if !TypeChecker::is_callable(func_sym) {
                    return Err(
                        SAError::TypeError(
                            SATypeError::NonCallable { 
                                sym_name: func_sym.name.clone() 
                            }
                        )
                    );
                } else {
                    if let Err(annotation_err) = self.annotate_expr_with_respective_type(node.kind.as_expr_mut().unwrap()) {
                        return Err(self.construct_sa_err(node, annotation_err));
                    }
                    if let Expr::FuncCall(func_call) = node.kind.as_expr_mut().unwrap() {
                        func_call.result_type = func_sym.lit_type;
                    } 
                    return Ok(());
                }
            }
        }
        Err(SAError::UndefinedSymbol { sym_name: func_name, token: node.start_token.clone().unwrap() })
    }


    fn analyze_var_decl_stmt(&self, node: &mut AST) -> SAResult {
        let mut ctx_borrow = self.ctx.borrow_mut();
        if let ASTKind::StmtAST(stmt) = &node.kind {
            return match stmt {
                Stmt::VarDecl(var_decl_stmt) => {
                    let symbol: Result<&mut Symbol, crate::context::CtxError> = ctx_borrow.find_sym_mut(&var_decl_stmt.sym_name);
                    if symbol.is_err() {
                        panic!("not good. how did I end up here? searching for a non-existing symbol while declaring a variable?");
                    }
                    let assign_expr: &mut Box<AST> = node.left.as_mut().unwrap();
                    if let ASTKind::ExprAST(expr) = &mut assign_expr.kind {
                        if let Err(expr_err) = self.annotate_expr_with_respective_type(expr) {
                            return Err(self.construct_sa_err(assign_expr, expr_err));
                        }
                        TypeChecker::type_check_var_decl_stmt(symbol.ok().unwrap(), expr)
                    } else {
                        Ok(())
                    }
                },
                Stmt::ArrVarDecl(arr_var_decl_stmt) => {
                    let symbol_res: Result<&mut Symbol, crate::context::CtxError> = ctx_borrow.find_sym_mut(&arr_var_decl_stmt.sym_name);
                    if symbol_res.is_err() {
                        panic!("not good. how did I end up here? searching for a non-existing symbol while declaring a variable?");
                    }
                    let symbol: &mut crate::Symbol = symbol_res.ok().unwrap();
                    // array size validation
                    if symbol.size != arr_var_decl_stmt.vals.len() {
                        return Err(SAError::ArrayLengthError { 
                            expected: symbol.size, 
                            found: arr_var_decl_stmt.vals.len() 
                        });
                    }
                    TypeChecker::type_check_arr_var_decl_stmt(symbol, &arr_var_decl_stmt.vals)
                },
                _ => panic!("Not a variable declaration statement...")
            };
        }
        Ok(())
    }

    /// Annotates the given expression with its respective type, if identifiable.
    /// 
    /// # Errors
    /// Returns an error if the expression type is not supported or the symbol's type 
    /// is undefined.
    fn annotate_expr_with_respective_type(&self, expr: &mut Expr) -> Result<(), _InternalSAErrType> {
        match expr {
            Expr::Ident(ident_expr) => {
                if let Some(ident_type) = self.get_ident_type(&ident_expr.sym_name) {
                    ident_expr.result_type = ident_type;
                    Ok(())
                }
                else {
                    Err(_InternalSAErrType::__ISET_UndefinedSymbol__ { 
                        name: ident_expr.sym_name.clone() 
                    })
                }
            },
            Expr::LitVal(_) => Ok(()),
            Expr::FuncCall(func_call_expr) => {
                for arg in &mut func_call_expr.args {
                    self.annotate_expr_with_respective_type(arg)?;
                }
                Ok(())
            },
            Expr::Binary(bin_expr) => {
                self.annotate_expr_with_respective_type(&mut bin_expr.left)?;
                self.annotate_expr_with_respective_type(&mut bin_expr.right)?;
                if TypeChecker::is_bin_expr_type_compatibile(bin_expr) {
                    Ok(())
                } else {
                    Err(_InternalSAErrType::__ISET_UndefinedSymbol__ { name: "".to_string() })
                }
            }
            _ => panic!("Type annotation not supported for {:?} yet!", expr)
        }
    }

    /// Retrieves the literal type of a symbol by its name, if available.
    /// This function does not return errors, but may return `None` if the 
    /// symbol cannot be found.
    fn get_ident_type(&self, sym_name: &str) -> Option<LitTypeVariant> {
        let ctx_borrow = self.ctx.borrow();
        ctx_borrow.find_sym(sym_name).ok().map(|symbol| symbol.lit_type)
    }

    fn construct_sa_err(&self, ast: &AST, err: _InternalSAErrType) -> SAError {
        match err {
            _InternalSAErrType::__ISET_UndefinedSymbol__ { name } => {
                SAError::UndefinedSymbol { sym_name: name, token: ast.start_token.clone().unwrap() }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use crate::{
        ast::{
            ASTKind, ASTOperation, BinExpr, Expr, LitValExpr, Stmt, VarDeclStmt, AST
        }, 
        context::CompilerCtx, 
        semantic::{sa_errors::{SAError, SATypeError}, sa_types::SAResult}, 
        types::{
            LitType, 
            LitTypeVariant
        }, 
        FunctionInfoTable, 
        StorageClass, 
        Symbol, 
        SymbolType, 
        Symtable
    };

    use super::SemanticAnalyzer;

    fn create_symt(syms: Vec<Symbol>) -> Symtable<Symbol> {
        let mut symt = Symtable::<Symbol>::new();
        for sym in syms {
            symt.add_symbol(sym);
        }
        symt
    }

    fn create_funt() -> FunctionInfoTable {
        FunctionInfoTable::new()
    }

    fn create_ctx<'ctx>(symt: &'ctx mut Symtable<Symbol>, func_table: &'ctx mut FunctionInfoTable) -> CompilerCtx<'ctx> {
        CompilerCtx::new(symt, func_table)
    }

    fn create_i32_var_decl_ast(pos: usize, name: String) -> AST {
        AST::new(
            ASTKind::StmtAST(Stmt::VarDecl(
                VarDeclStmt {
                    class: StorageClass::LOCAL,
                    symtbl_pos: pos,
                    sym_name: name
                }
            )), 
            ASTOperation::AST_VAR_DECL, 
            Some(AST::create_leaf(
                ASTKind::ExprAST(
                    Expr::LitVal(LitValExpr { 
                        value: LitType::I32(123), 
                        result_type: LitTypeVariant::I32 
                    })
                ), 
                ASTOperation::AST_INTLIT, 
                LitTypeVariant::I32, 
                None, 
                None
            )
            ), 
            None, 
            LitTypeVariant::I32
        )
    }

    fn create_i32_var_decl_ast_with_bin_expr(pos: usize, name: String) -> AST {
        AST::new(
            ASTKind::StmtAST(Stmt::VarDecl(
                VarDeclStmt {
                    class: StorageClass::LOCAL,
                    symtbl_pos: pos,
                    sym_name: name
                }
            )), 
            ASTOperation::AST_VAR_DECL, 
            Some(AST::create_leaf(
                ASTKind::ExprAST(
                    Expr::Binary(BinExpr {
                        operation: ASTOperation::AST_ADD,
                        left: Box::new(
                            Expr::LitVal(LitValExpr { 
                                value: LitType::I32(123), 
                                result_type: LitTypeVariant::I32 
                            })
                        ),
                        right: Box::new(
                            Expr::LitVal(
                                LitValExpr {
                                    value: LitType::Str("bichara".to_string(), 7),
                                    result_type: LitTypeVariant::Str
                                }
                            )
                        ),
                        result_type: LitTypeVariant::I32
                    })
                    
                ), 
                ASTOperation::AST_INTLIT, 
                LitTypeVariant::I32, 
                None, 
                None
            )
            ), 
            None, 
            LitTypeVariant::I32
        )
    }

    fn create_i32_symbol(name: String) -> Symbol {
        Symbol::new(name, LitTypeVariant::I32, SymbolType::Variable, StorageClass::LOCAL)
    }

    fn create_symbol_without_explicit_type(name: String) -> Symbol {
        Symbol::new(name, LitTypeVariant::None, SymbolType::Variable, StorageClass::LOCAL)
    }

    #[test]
    fn test_no_type_var_decl_stmt_analysis() {
        let symt: &mut Symtable<Symbol> = &mut create_symt(
            vec![
                create_symbol_without_explicit_type("number".to_string()),
            ]
        );
        let funct: &mut FunctionInfoTable = &mut create_funt();
        let ctx: Rc<RefCell<CompilerCtx<'_>>> = Rc::new(RefCell::new(create_ctx(symt, funct)));

        let a_analyzer: SemanticAnalyzer<'_> = SemanticAnalyzer::new(Rc::clone(&ctx));

        let mut no_type_var_ast: AST = create_i32_var_decl_ast(0, "number".to_string());
        let ar2: SAResult = a_analyzer.analyze_var_decl_stmt(&mut no_type_var_ast);
        assert!(ar2.is_ok());

        // type has to be updated of non-type symbol
        assert_eq!(symt.get_symbol(0).unwrap().lit_type, LitTypeVariant::I32);
    }

    #[test]
    fn test_bin_expr_var_decl_stmt_analysis() {
        let symt: &mut Symtable<Symbol> = &mut create_symt(
            vec![
                create_symbol_without_explicit_type("number".to_string()),
            ]
        );
        let funct: &mut FunctionInfoTable = &mut create_funt();
        let ctx: Rc<RefCell<CompilerCtx<'_>>> = Rc::new(RefCell::new(create_ctx(symt, funct)));

        let a_analyzer: SemanticAnalyzer<'_> = SemanticAnalyzer::new(Rc::clone(&ctx));

        let mut no_type_var_ast: AST = create_i32_var_decl_ast_with_bin_expr(0, "number".to_string());
        let ar2: SAResult = a_analyzer.analyze_var_decl_stmt(&mut no_type_var_ast);
        assert!(ar2.is_err());
        matches!(
            ar2, 
            Err(
                SAError::TypeError(
                    SATypeError::IncompatibleTypes { 
                        a: LitTypeVariant::I32, 
                        b: LitTypeVariant::Str, 
                        operation: ASTOperation::AST_ADD
                    }
                )
            )
        );

        // type has to be updated of non-type symbol
        // assert_eq!(symt.get_symbol(0).unwrap().lit_type, LitTypeVariant::I32);
    }

    #[test]
    fn test_var_decl_stmt_analysis() {
        let symt: &mut Symtable<Symbol> = &mut create_symt(
            vec![
                create_i32_symbol("number".to_string()),
            ]
        );
        let funct: &mut FunctionInfoTable = &mut create_funt();
        let ctx: Rc<RefCell<CompilerCtx<'_>>> = Rc::new(RefCell::new(create_ctx(symt, funct)));

        let mut var_ast: AST = create_i32_var_decl_ast(0, "number".to_string());

        let a_analyzer: SemanticAnalyzer<'_> = SemanticAnalyzer::new(Rc::clone(&ctx));

        let analysis_res: SAResult = a_analyzer.analyze_var_decl_stmt(&mut var_ast);
        assert!(analysis_res.is_ok());
    }
}