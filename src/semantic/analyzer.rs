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

use crate::{ast::{ASTKind, ASTOperation, FuncCallExpr, Stmt, AST}, context::CompilerCtx, FunctionInfo, Symbol};

use super::{sa_errors::SAError, sa_types::SAResult, type_checker::TypeChecker};

pub struct SemanticAnalyzer<'sa> {
    pub ctx: Rc<RefCell<CompilerCtx<'sa>>>
}

impl<'sa> SemanticAnalyzer<'sa> {
    pub fn new(ctx: Rc<RefCell<CompilerCtx<'sa>>>) -> Self {
        Self {
            ctx
        }
    }

    pub fn start_analysis(&self, nodes: &Vec<AST>) {
        for node in nodes {
            let _result: SAResult = self.analyze_node(node, ASTOperation::AST_NONE);
        }
    }

    fn analyze_node(&self, node: &AST, parent_ast_op: ASTOperation) -> SAResult {
        match node.operation {
            ASTOperation::AST_VAR_DECL => self.analyze_var_decl_stmt(node),
            ASTOperation::AST_FUNCTION => self.analyze_func_decl_stmt(node),
            ASTOperation::AST_FUNC_CALL => self.analyze_func_call_expr(node),
            ASTOperation::AST_GLUE => {
                if let Some(left) = node.left.as_ref() {
                    _ = self.analyze_node(left, parent_ast_op);
                }
                if let Some(right) = node.right.as_ref() {
                    _ = self.analyze_node(right, parent_ast_op);
                }
                Ok(())
            }
            _ => panic!("'{:?}' is not supported ASTOperation for 'analyze_node' yet!", node.operation)
        }
    }

    fn analyze_func_decl_stmt(&self, node: &AST) -> SAResult {
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

        let ctx_borrow: std::cell::Ref<CompilerCtx<'_>> = self.ctx.borrow();
        let func_name: String = ctx_borrow.sym_table.get_symbol(func_id).unwrap().name.clone();
        if ctx_borrow.func_table.get(&func_name).is_none() {
            panic!("Function '{}' not found", func_name);
        };
        drop(ctx_borrow);

        if let Some(func_body) = &node.left {
            return self.analyze_node(func_body, ASTOperation::AST_FUNCTION);
        }
        Ok(())
    }

    fn analyze_func_call_expr(&self, node: &AST) -> SAResult {
        Ok(())
    }

    fn analyze_var_decl_stmt(&self, node: &AST) -> SAResult {
        let mut ctx_borrow = self.ctx.borrow_mut();
        if let ASTKind::StmtAST(stmt) = &node.kind {
            return match stmt {
                Stmt::VarDecl(var_decl_stmt) => {
                    let symbol: &mut Symbol = ctx_borrow.sym_table.get_mut_or_fail(var_decl_stmt.symtbl_pos);
                    let assign_expr: &ASTKind = &node.left.as_ref().unwrap().kind;

                    if let ASTKind::ExprAST(expr) = assign_expr {
                        TypeChecker::type_check_var_decl_stmt(symbol, expr)
                    } else {
                        Ok(())
                    }
                },
                Stmt::ArrVarDecl(arr_var_decl_stmt) => {
                    let sym: &mut Symbol = ctx_borrow.sym_table.get_mut_or_fail(arr_var_decl_stmt.symtbl_pos);
                    // array size validation
                    if sym.size != arr_var_decl_stmt.vals.len() {
                        return Err(SAError::ArrayLengthError { 
                            expected: sym.size, 
                            found: arr_var_decl_stmt.vals.len() 
                        });
                    }
                    TypeChecker::type_check_arr_var_decl_stmt(sym, &arr_var_decl_stmt.vals)
                },
                _ => panic!("Not a variable declaration statement...")
            };
        }
        Ok(())
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

    fn create_i32_var_decl_ast(pos: usize) -> AST {
        AST::new(
            ASTKind::StmtAST(Stmt::VarDecl(
                VarDeclStmt {
                    class: StorageClass::LOCAL,
                    symtbl_pos: pos
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

    fn create_i32_var_decl_ast_with_bin_expr(pos: usize) -> AST {
        AST::new(
            ASTKind::StmtAST(Stmt::VarDecl(
                VarDeclStmt {
                    class: StorageClass::LOCAL,
                    symtbl_pos: pos
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

        let no_type_var_ast: AST = create_i32_var_decl_ast(0);
        let ar2: SAResult = a_analyzer.analyze_var_decl_stmt(&no_type_var_ast);
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

        let no_type_var_ast: AST = create_i32_var_decl_ast_with_bin_expr(0);
        let ar2: SAResult = a_analyzer.analyze_var_decl_stmt(&no_type_var_ast);
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
                create_i32_symbol("number2".to_string())
            ]
        );
        let funct: &mut FunctionInfoTable = &mut create_funt();
        let ctx: Rc<RefCell<CompilerCtx<'_>>> = Rc::new(RefCell::new(create_ctx(symt, funct)));

        let var_ast: AST = create_i32_var_decl_ast(0);

        let a_analyzer: SemanticAnalyzer<'_> = SemanticAnalyzer::new(Rc::clone(&ctx));

        let analysis_res: SAResult = a_analyzer.analyze_var_decl_stmt(&var_ast);
        assert!(analysis_res.is_ok());
    }
}