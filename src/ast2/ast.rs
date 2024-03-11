pub struct IfStmt {
}

pub enum WhileStmt {

}

pub enum Stmt {
    IfStmt(IfStmt),
    WhileStmt(WhileStmt)
}

pub enum ExprKind {
    Subscript,
    Constant
}

pub enum Expr {

}

pub enum ASTNodeKind {
    Stmt(Stmt),
    Expr(Expr)
}

pub struct AST {
    pub kind: ASTNodeKind,
}