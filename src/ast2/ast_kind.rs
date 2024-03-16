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

use super::{Expr, Stmt};

pub enum ASTKind {
    StmtAST(Stmt),
    ExprAST(Expr)
}

impl ASTKind {
    /// Checks if the ASTKind is a statement variant.
    pub fn is_stmt(&self) -> bool {
        matches!(self, ASTKind::StmtAST(_))
    }

    /// Checks if the ASTKind is an expression variant.
    pub fn is_expr(&self) -> bool {
        matches!(self, ASTKind::ExprAST(_))
    }

    /// Unwraps the ASTKind, returning the contained statement.
    ///
    /// # Panics
    /// Panics if the ASTKind is not a statement kind.
    pub fn unwrap_stmt(self) -> Stmt {
        match self {
            Self::StmtAST(stmt) => stmt,
            _ => panic!("ASTKind is not a statement kind!")
        }
    }

    /// Unwraps the ASTKind, returning the contained expression.
    ///
    /// # Panics
    /// Panics if the ASTKind is not an expression kind.
    pub fn unwrap_expr(self) -> Expr {
        match self {
            Self::ExprAST(expr) => expr,
            _ => panic!("ASTKind is not an expression kind!")
        }
    }
}