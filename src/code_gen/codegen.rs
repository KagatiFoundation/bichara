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

use std::cell::RefCell;
use std::rc::Rc;

use crate::ast2::AST;
use crate::ast2::ASTOperation;
use crate::types::LitType;

use super::register::RegManager;

pub trait CodeGen {
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
            return self.gen_if_stmt(ast_node);
        } else if ast_node.operation == ASTOperation::AST_WHILE {
            return self.gen_while_stmt(ast_node);
        } else if ast_node.operation == ASTOperation::AST_FUNCTION {
            return self.gen_function_stmt(ast_node);
        } else if ast_node.operation == ASTOperation::AST_GLUE {
            if let Some(left) = ast_node.left.as_ref() {
                self.gen_code_from_ast(left, 0xFFFFFFFF, ast_node.operation);
                self.reg_manager().borrow_mut().deallocate_all();
            }
            if let Some(right) = ast_node.right.as_ref() {
                self.gen_code_from_ast(right, 0xFFFFFFFF, ast_node.operation);
                self.reg_manager().borrow_mut().deallocate_all();
            }
            return 0xFFFFFFFF;
        }
        let mut leftreg: usize = 0xFFFFFFFF;
        let mut rightreg: usize = 0xFFFFFFFF;
        if let Some(left) = &*ast_node.left {
            leftreg = self.gen_code_from_ast(left, 0xFFFFFFFF, ast_node.operation);
        }
        if let Some(right) = &*ast_node.right {
            rightreg = self.gen_code_from_ast(right, reg, ast_node.operation);
        }
        match ast_node.operation {
            ASTOperation::AST_GTHAN
            | ASTOperation::AST_LTHAN
            | ASTOperation::AST_LTEQ
            | ASTOperation::AST_GTEQ
            | ASTOperation::AST_NEQ
            | ASTOperation::AST_EQEQ => {
                if (parent_ast_kind == ASTOperation::AST_IF)
                    || (parent_ast_kind == ASTOperation::AST_WHILE)
                {
                    self.gen_cmp_and_jmp(ast_node.operation, leftreg, rightreg, reg)
                } else {
                    self.gen_cmp_and_set(ast_node.operation, leftreg, rightreg)
                }
            }
            _ => 90
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

    fn gen_load_id_into_reg(&mut self, id: &LitType) -> usize;

    fn gen_load_reg_into_id(&mut self, reg: usize, id: &LitType) -> usize;

    fn gen_add(&mut self, r1: usize, r2: usize) -> usize;

    fn gen_sub(&mut self, r1: usize, r2: usize) -> usize;

    fn gen_load_intlit_into_reg(&mut self, value: &LitType) -> usize;

    fn gen_id_address_into_another_id(&mut self, id: &LitType) -> usize;

    fn gen_deref_pointer_id(&mut self, id: &LitType) -> usize;

    fn gen_load_global_strlit(&mut self, id: &LitType) -> usize;

    fn gen_array_access(&mut self, id: &LitType, expr: &AST) -> usize;

    fn gen_return_stmt(&mut self, result_reg: usize, _func_id: usize) -> usize;

    fn reg_manager(&self) -> Rc<RefCell<RegManager>>;
}