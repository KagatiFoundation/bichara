use crate::ast2;

pub struct Aarch64ASTGen {

}

impl Aarch64ASTGen {
    /// Returns register
    pub fn gen_code(&self, ast: ast2::AST) -> usize {
        match ast.kind {
            ast2::ASTNodeKind::Stmt(ref stmt) => self.gen_stmt(stmt),
            _ => {
                0xFFFFFFFF
            }
        }
    }

    fn gen_stmt(&self, stmt: &ast2::Stmt) -> usize {
        match stmt {
            ast2::Stmt::IfStmt(if_stmt) => {
                0
            }
            _ => 0
        }
    }

    fn gen_if_stmt(&self, ast: ast2::AST) -> usize {
        0xFFFFFFFF
    }
}