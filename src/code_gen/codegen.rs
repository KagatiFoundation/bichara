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

use std::cell::RefMut;

use crate::ast::ASTKind;
use crate::ast::ArrVarDeclStmt;
use crate::ast::AssignStmt;
use crate::ast::BinExpr;
use crate::ast::Expr;
use crate::ast::FuncCallExpr;
use crate::ast::FuncCallStmt;
use crate::ast::LitValExpr;
use crate::ast::Stmt;
use crate::ast::VarDeclStmt;
use crate::ast::AST;
use crate::ast::ASTOperation;
use crate::types::LitType;
use crate::types::LitTypeVariant;
use crate::StorageClass;

use super::register::RegManager;
use super::CodeGenErr;

pub type CodeGenResult = Result<usize, CodeGenErr>;

/// Indicating no register was produced from an code generation operation.
pub const NO_REG: usize = 0xFFFFFFFF;

pub const EARLY_RETURN: usize = 0xEEEEEEEE;

pub trait CodeGen {
    /// Starts the code generation process.
    ///
    /// This method initializes code generation by generating global symbols and marking the start of the `.text` section.
    ///
    /// # Arguments
    ///
    /// * `nodes` - The Abstract Syntax Tree (AST) list used for code generation.
    ///
    /// # Examples
    ///
    /// ```
    /// use code_gen::CodeGen;
    /// use ast::AST;
    ///
    /// let mut generator = CodeGen::new();
    /// let ast = AST::new();
    /// generator.start_gen(&ast);
    /// ```
    fn start_gen(&mut self, nodes: Vec<AST>) where Self: Sized {
        self.gen_global_symbols();
        // .text section starts from here
        println!("\n.text");
        for node in &nodes {
            let result: CodeGenResult = self.gen_code_from_ast(node, 0xFFFFFFFF, ASTOperation::AST_NONE);
            if result.is_err() {
                println!("Error during code generation!");
            }
        }
    }

    /// Generate global symbols into the data section.
    ///
    /// This function iterates over the main symbol table, printing symbol 
    /// information for each global symbol. Symbols are skipped if they meet 
    /// any of the following conditions:
    ///   - The symbol type is `Function`.
    ///   - The literal type is `None`.
    ///   - The storage class is `LOCAL`.
    /// 
    /// For each processed global symbol, the appropriate `.data` directive and 
    /// symbol name are printed. If the symbol is a variable, its global data is 
    /// printed with proper alignment using `dump_global_with_alignment`. If the 
    /// symbol is an array, the appropriate data space is allocated for each element 
    /// based on its size and data type.
    fn gen_global_symbols(&self);

    /// Generates code for a given AST node and returns a register index 
    /// for further processing.
    ///
    /// This function takes a reference to an Abstract Syntax Tree (AST) node
    /// (`ast_node`) that needs code generation, a register index (`reg`) 
    /// indicating the register to use for the operation, and the parent AST 
    /// operation (`parent_ast_kind`) to provide context during code generation.
    ///
    /// # Arguments
    ///
    /// * `ast_node` - A reference to an AST node for which code needs to be generated.
    /// * `reg` - A usize value representing the register index to use for the operation.
    /// * `parent_ast_kind` - An enum representing the parent operation in the AST hierarchy.
    ///
    /// # Returns
    ///
    /// The function returns a usize value representing the register index for further 
    /// processing.
    fn gen_code_from_ast(
        &mut self, ast_node: &AST, 
        reg: usize, 
        parent_ast_kind: ASTOperation) 
    -> CodeGenResult where Self: Sized {
        if ast_node.operation == ASTOperation::AST_IF {
            self.gen_if_stmt(ast_node, reg)
        } 
        else if ast_node.operation == ASTOperation::AST_WHILE {
            self.gen_while_stmt(ast_node)
        } 
        else if ast_node.operation == ASTOperation::AST_FUNCTION {
            self.gen_function_stmt(ast_node)
        } 
        else if ast_node.operation == ASTOperation::AST_BREAK {
            self.gen_break_stmt(reg)
        }
        else if ast_node.operation == ASTOperation::AST_GLUE {
            if let Some(left) = ast_node.left.as_ref() {
                _ = self.gen_code_from_ast(left, reg, parent_ast_kind);
                self.reg_manager().deallocate_all();
            }
            if let Some(right) = ast_node.right.as_ref() {
                _ = self.gen_code_from_ast(right, reg, parent_ast_kind);
                self.reg_manager().deallocate_all();
            }
            Ok(NO_REG)
        }
        else if ast_node.operation == ASTOperation::AST_FUNC_CALL {
            if ast_node.result_type == LitTypeVariant::Void {
                if let ASTKind::StmtAST(func_call_stmt) = &ast_node.kind {
                    match func_call_stmt {
                        Stmt::FuncCall(func_call) => {
                            return self.gen_func_call_stmt(func_call);
                        },
                        _ => return Ok(NO_REG)
                    }
                }
            }
            Ok(NO_REG)
        }
        else if ast_node.operation == ASTOperation::AST_RETURN {
            let early_return = parent_ast_kind != ASTOperation::AST_FUNCTION;
            let possible_ret_stmt: Stmt = ast_node.kind.clone().unwrap_stmt();
            return match possible_ret_stmt {
                Stmt::Return(_) => {
                    if ast_node.left.is_some() {
                        let return_expr: &AST = ast_node.left.as_ref().unwrap();
                        _ = self.gen_expr(&return_expr.kind.clone().unwrap_expr(), ast_node.operation, reg, parent_ast_kind)?;
                    }
                    self.gen_return_stmt(early_return)
                }
                _ => Ok(NO_REG)
            };
        }
        else if ast_node.operation == ASTOperation::AST_VAR_DECL {
            if let ASTKind::StmtAST(var_decl) = &ast_node.kind {
                return match var_decl {
                    Stmt::VarDecl(var_decl_stmt) => {
                        if var_decl_stmt.class != StorageClass::LOCAL {
                            Ok(NO_REG)
                        } else {
                            let assign_expr: &ASTKind = &ast_node.left.as_ref().unwrap().kind;
                            if let ASTKind::ExprAST(__expr) = assign_expr {
                                _ = self.gen_local_var_decl_stmt(var_decl_stmt, __expr);
                            }
                            Ok(NO_REG)
                        }
                    },
                    _ => Ok(NO_REG)
                };
            }
            Ok(NO_REG) 
        } 
        else if ast_node.operation == ASTOperation::AST_ARR_VAR_DECL {
            if let ASTKind::StmtAST(arr_var_decl) = &ast_node.kind {
                return match arr_var_decl {
                    Stmt::ArrVarDecl(arr_var_decl_stmt) => {
                        if arr_var_decl_stmt.class != StorageClass::LOCAL {
                            Ok(NO_REG)
                        } 
                        else {
                            self.gen_local_arr_var_decl_stmt(arr_var_decl_stmt)
                        }
                    },
                    _ => Ok(NO_REG)
                }
            }
            Ok(NO_REG)
        }
        else if (ast_node.operation == ASTOperation::AST_NONE)
            || (ast_node.operation == ASTOperation::AST_ARR_VAR_DECL) {
            return Ok(NO_REG);
        }  
        else if ast_node.operation == ASTOperation::AST_ASSIGN {
            let possible_assign_stmt: Stmt = ast_node.kind.clone().unwrap_stmt();
            return match possible_assign_stmt {
                Stmt::Assignment(assign) => {
                    let assign_expr = &ast_node.right.as_ref().unwrap().kind;
                    if let ASTKind::ExprAST(__expr) = assign_expr {
                        _ = self.gen_var_assignment_stmt(&assign, __expr);
                    }
                    Ok(NO_REG)
                },
                _ => Ok(NO_REG)
            };
        }
        else if ast_node.operation == ASTOperation::AST_LOOP {
            let possible_loop_stmt: Stmt = ast_node.kind.clone().unwrap_stmt();
            return match possible_loop_stmt {
                Stmt::Loop => {
                    self.gen_loop_stmt(ast_node)
                },
                _ => Ok(NO_REG)
            }
        }
        else {
            let expr_ast: Expr = ast_node.kind.clone().unwrap_expr();
            self.gen_expr(&expr_ast, ast_node.operation, reg, parent_ast_kind)
        }
    }

    fn gen_expr(
        &mut self, 
        expr: &Expr, 
        curr_ast_kind: ASTOperation, 
        reg: usize, 
        parent_ast_kind: ASTOperation
    ) -> CodeGenResult {
        match expr {
            Expr::Binary(bin) => self.gen_bin_expr(bin, curr_ast_kind, reg, parent_ast_kind),
            Expr::LitVal(lit) => self.gen_lit_expr(lit),
            Expr::Ident(ident) => self.gen_load_id_into_reg(ident.symtbl_pos),
            Expr::Widen(widen) => {
                match widen.from.kind.clone() {
                    crate::ast::ASTKind::StmtAST(_) => todo!(),
                    crate::ast::ASTKind::ExprAST(wexpr) => {
                        self.gen_expr(&wexpr, curr_ast_kind, reg, parent_ast_kind)
                    },
                    crate::ast::ASTKind::Empty => Ok(NO_REG)
                }
            },
            Expr::Subscript(subs) => {
                let index_reg: usize = self.gen_expr(&subs.index, curr_ast_kind, reg, parent_ast_kind)?;
                self.gen_array_access2(subs.symtbl_pos, index_reg)
            },
            Expr::FuncCall(func_call) => self.gen_func_call_expr(func_call),
        }
    }

    /// Generates code for the given literal value expression.
    ///
    /// This method generates code to load the literal value into a register based on its type.
    ///
    /// # Arguments
    ///
    /// * `lit_expr` - The literal value expression for which to generate code.
    ///
    /// # Returns
    ///
    /// The register index if code generation is successful, otherwise 0xFFFFFFFF.
    ///
    fn gen_lit_expr(&mut self, lit_expr: &LitValExpr) -> CodeGenResult {
        match lit_expr.result_type {
            LitTypeVariant::U8 |
            LitTypeVariant::I16 |
            LitTypeVariant::I64 |
            LitTypeVariant::I32 => self.gen_load_intlit_into_reg(&lit_expr.value),
            LitTypeVariant::Str => self.gen_load_global_strlit(&lit_expr.value),
            _ => Ok(NO_REG)
        }
    }

    fn gen_bin_expr(&mut self, bin_expr: &BinExpr, curr_ast_kind: ASTOperation, reg: usize, parent_ast_kind: ASTOperation) -> CodeGenResult {
        let leftreg: usize = self.gen_expr(&bin_expr.left, curr_ast_kind, reg, parent_ast_kind)?;
        let rightreg: usize = self.gen_expr(&bin_expr.right, curr_ast_kind, reg, parent_ast_kind)?;

        match bin_expr.operation {
            ASTOperation::AST_ADD => self.gen_add(leftreg, rightreg),
            ASTOperation::AST_SUBTRACT => self.gen_sub(leftreg, rightreg),
            ASTOperation::AST_MULTIPLY => self.gen_mul(leftreg, rightreg),
            ASTOperation::AST_GTHAN
            | ASTOperation::AST_LTHAN
            | ASTOperation::AST_LTEQ
            | ASTOperation::AST_GTEQ
            | ASTOperation::AST_NEQ
            | ASTOperation::AST_EQEQ => {
                if (parent_ast_kind == ASTOperation::AST_IF)
                || (parent_ast_kind == ASTOperation::AST_WHILE)
                {
                    self.gen_cmp_and_jmp(bin_expr.operation, leftreg, rightreg, reg)
                } else {
                    self.gen_cmp_and_set(bin_expr.operation, leftreg, rightreg)
                } 
            },
            _ => Ok(NO_REG)
        }
    }

    /// Generates code for an If statement AST node and returns a constant 
    /// value to signify no register allocation.
    ///
    /// This function takes a mutable reference to `self` to allow modification 
    /// of the current code generation context. It also takes a reference to an 
    /// Abstract Syntax Tree (AST) node representing an If statement (`Stmt::IfStmt`). 
    /// The function generates code for the If statement AST node and always returns 
    /// `0xFFFFFFFF` to signify that no register allocation occurred during the code 
    /// generation process.
    ///
    /// # Arguments
    ///
    /// * `ast` - A reference to an AST node representing an If statement (`Stmt::IfStmt`).
    ///
    /// # Returns
    ///
    /// The function always returns `0xFFFFFFFF` to signify no register allocation during 
    /// code generation.
    fn gen_if_stmt(&mut self, ast: &AST, reg: usize) -> CodeGenResult;
    
    fn gen_jump(&self, label_id: usize) -> CodeGenResult;

    fn gen_label(&mut self, label: usize) -> CodeGenResult;

    fn gen_cmp_and_jmp(&self, operation: ASTOperation, r1: usize, r2: usize, label: usize) -> CodeGenResult;

    fn gen_cmp_and_set(&self, operation: ASTOperation, r1: usize, r2: usize) -> CodeGenResult;

    fn gen_function_stmt(&mut self, ast: &AST) -> CodeGenResult;

    fn gen_while_stmt(&mut self, ast: &AST) -> CodeGenResult;

    /// Loads the value corresponding to the symbol with the given position in the symbol table into a register.
    /// 
    /// # Arguments
    /// 
    /// * `id` - The symbol table position for the symbol.
    /// 
    /// # Returns
    /// 
    /// The register index where the value of the symbol is loaded.
    fn gen_load_id_into_reg(&mut self, id: usize) -> CodeGenResult;

    fn gen_store_reg_value_into_id(&mut self, reg: usize, id: usize) -> CodeGenResult;

    fn gen_add(&mut self, r1: usize, r2: usize) -> CodeGenResult;

    fn gen_sub(&mut self, r1: usize, r2: usize) -> CodeGenResult;
    
    fn gen_mul(&mut self, r1: usize, r2: usize) -> CodeGenResult;
    
    fn gen_load_intlit_into_reg(&mut self, value: &LitType) -> CodeGenResult;

    fn gen_load_global_strlit(&mut self, symbol_id: &LitType) -> CodeGenResult;

    fn gen_array_access(&mut self, symbol_id: usize, expr: &AST) -> CodeGenResult;
    
    fn gen_array_access2(&mut self, symbol_id: usize, index: usize) -> CodeGenResult;

    fn gen_return_stmt(&mut self, early_return: bool) -> CodeGenResult;

    fn gen_func_call_stmt(&mut self, func_call_stmt: &FuncCallStmt) -> CodeGenResult;

    fn gen_func_call_expr(&mut self, func_call_expr: &FuncCallExpr) -> CodeGenResult;

    fn gen_local_var_decl_stmt(&mut self, var_decl_stmt: &VarDeclStmt, expr_ast: &Expr) -> CodeGenResult;
    
    fn gen_local_arr_var_decl_stmt(&mut self, arr_var_decl_stmt: &ArrVarDeclStmt) -> CodeGenResult;

    fn gen_var_assignment_stmt(&mut self, assign_stmt: &AssignStmt, expr_ast: &Expr) -> CodeGenResult;

    fn gen_loop_stmt(&mut self, ast: &AST) -> CodeGenResult;

    fn gen_break_stmt(&mut self, break_label: usize) -> CodeGenResult;

    fn reg_manager(&self) -> RefMut<RegManager>;
}