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

use crate::ast::BinExpr;
use crate::ast::Expr;
use crate::ast::LitValExpr;
use crate::ast::Stmt;
use crate::ast::AST;
use crate::ast::ASTOperation;
use crate::types::LitType;
use crate::types::LitTypeVariant;

use super::register::RegManager;

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
            self.gen_code_from_ast(node, 0xFFFFFFFF, ASTOperation::AST_NONE);
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
    -> usize where Self: Sized {
        if ast_node.operation == ASTOperation::AST_IF {
            self.gen_if_stmt(ast_node)
        } else if ast_node.operation == ASTOperation::AST_WHILE {
            self.gen_while_stmt(ast_node)
        } else if ast_node.operation == ASTOperation::AST_FUNCTION {
            self.gen_function_stmt(ast_node)
        } else if ast_node.operation == ASTOperation::AST_GLUE {
            if let Some(left) = ast_node.left.as_ref() {
                self.gen_code_from_ast(left, 0xFFFFFFFF, ast_node.operation);
                self.reg_manager().deallocate_all();
            }
            if let Some(right) = ast_node.right.as_ref() {
                self.gen_code_from_ast(right, 0xFFFFFFFF, ast_node.operation);
                self.reg_manager().deallocate_all();
            }
            return 0xFFFFFFFF;
        } else if ast_node.operation == ASTOperation::AST_ASSIGN {
            let possible_lv_stmt: Stmt = ast_node.kind.clone().unwrap_stmt();
            return match possible_lv_stmt {
                Stmt::Assignment(asst) => {
                    #[allow(unused_assignments)] // turn of this annoying warning
                    let mut expr_res_reg: usize = 0;
                    let assign_right_kind: crate::ast::ASTKind = ast_node.right.as_ref().unwrap().kind.clone();
                    match assign_right_kind {
                        crate::ast::ASTKind::StmtAST(_) => todo!(),
                        crate::ast::ASTKind::ExprAST(expr) => {
                            expr_res_reg = self.gen_expr(&expr, ast_node.operation, reg, parent_ast_kind);
                        },
                        _ => ()
                    }
                    return self.gen_load_reg_into_id(expr_res_reg, asst.symtbl_pos);
                }
                _ => 0xFFFFFFFF // invalid AST kind with AST_LVIDENT operation
            };
        } else if ast_node.operation == ASTOperation::AST_RETURN {
            let possible_ret_stmt: Stmt = ast_node.kind.clone().unwrap_stmt();
            let return_expr: &AST = ast_node.left.as_ref().unwrap();
            let result_reg: usize = self.gen_expr(&return_expr.kind.clone().unwrap_expr(), ast_node.operation, reg, parent_ast_kind);
            return match possible_ret_stmt {
                Stmt::Return(ret) => self.gen_return_stmt(result_reg, ret.func_id),
                _ => 0xFFFFFFFF
            };
        }
        else if ast_node.operation == ASTOperation::AST_NONE {
            return 0xFFFFFFFF
        }  else {
            let expr_ast: Expr = ast_node.kind.clone().unwrap_expr();
            let reg_used_for_expr: usize = self.gen_expr(&expr_ast, ast_node.operation, reg, parent_ast_kind);
            return reg_used_for_expr;
        }
    }

    fn gen_expr(
        &mut self, 
        expr: &Expr, 
        curr_ast_kind: ASTOperation, 
        reg: usize, 
        parent_ast_kind: ASTOperation
    ) -> usize {
        match expr {
            Expr::Binary(bin) => self.gen_bin_expr(bin, curr_ast_kind, reg, parent_ast_kind),
            Expr::LitVal(lit) => self.gen_lit_expr(lit),
            Expr::Addr(add) => self.gen_id_address_into_another_id(add.symtbl_pos),
            Expr::Deref(der) => self.gen_deref_pointer_id(der.symtbl_pos),
            Expr::Ident(ident) => self.gen_load_id_into_reg(ident.symtbl_pos),
            Expr::Widen(widen) => {
                match widen.from.kind.clone() {
                    crate::ast::ASTKind::StmtAST(_) => todo!(),
                    crate::ast::ASTKind::ExprAST(wexpr) => {
                        self.gen_expr(&wexpr, curr_ast_kind, reg, parent_ast_kind)
                    },
                    crate::ast::ASTKind::Empty => 0xFFFFFFFF
                }
            },
            Expr::Subscript(subs) => {
                let index_reg: usize = self.gen_expr(&subs.index, curr_ast_kind, reg, parent_ast_kind);
                self.gen_array_access2(subs.symtbl_pos, index_reg)
            },
            _ => panic!("Error: Unknown Expr type '{:?}'", expr),
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
    fn gen_lit_expr(&mut self, lit_expr: &LitValExpr) -> usize {
        match lit_expr.result_type {
            LitTypeVariant::U8 |
            LitTypeVariant::I16 |
            LitTypeVariant::I64 |
            LitTypeVariant::I32 => self.gen_load_intlit_into_reg(&lit_expr.value),
            LitTypeVariant::U8Ptr => self.gen_load_global_strlit(&lit_expr.value),
            _ => 0xFFFFFFFF
        }
    }

    fn gen_bin_expr(&mut self, bin_expr: &BinExpr, curr_ast_kind: ASTOperation, reg: usize, parent_ast_kind: ASTOperation) -> usize {
        let leftreg: usize = self.gen_expr(&bin_expr.left, curr_ast_kind, reg, parent_ast_kind);
        let rightreg: usize = self.gen_expr(&bin_expr.right, curr_ast_kind, reg, parent_ast_kind);
        match bin_expr.operation {
            ASTOperation::AST_ADD => self.gen_add(leftreg, rightreg),
            ASTOperation::AST_SUBTRACT => self.gen_sub(leftreg, rightreg),
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
            _ => 0xFFFFFFFF
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
    fn gen_if_stmt(&mut self, ast: &AST) -> usize;
    
    fn gen_jump(&self, label_id: usize);

    fn gen_label(&mut self, label: usize);

    fn gen_cmp_and_jmp(&self, operation: ASTOperation, r1: usize, r2: usize, label: usize) -> usize;

    fn gen_cmp_and_set(&self, operation: ASTOperation, r1: usize, r2: usize) -> usize;

    fn gen_function_stmt(&mut self, ast: &AST) -> usize;

    fn gen_while_stmt(&mut self, ast: &AST) -> usize;

    /// Loads the value corresponding to the symbol with the given position in the symbol table into a register.
    /// 
    /// # Arguments
    /// 
    /// * `id` - The symbol table position for the symbol.
    /// 
    /// # Returns
    /// 
    /// The register index where the value of the symbol is loaded.
    fn gen_load_id_into_reg(&mut self, id: usize) -> usize;

    fn gen_load_reg_into_id(&mut self, reg: usize, symbol_id: usize) -> usize;

    fn gen_add(&mut self, r1: usize, r2: usize) -> usize;

    fn gen_sub(&mut self, r1: usize, r2: usize) -> usize;

    fn gen_load_intlit_into_reg(&mut self, value: &LitType) -> usize;

    fn gen_id_address_into_another_id(&mut self, symbol_id: usize) -> usize;

    fn gen_deref_pointer_id(&mut self, symbol_id: usize) -> usize;

    fn gen_load_global_strlit(&mut self, symbol_id: &LitType) -> usize;

    fn gen_array_access(&mut self, symbol_id: usize, expr: &AST) -> usize;
    
    fn gen_array_access2(&mut self, symbol_id: usize, index: usize) -> usize;

    fn gen_return_stmt(&mut self, result_reg: usize, _func_id: usize) -> usize;

    fn reg_manager(&self) -> RefMut<RegManager>;
}