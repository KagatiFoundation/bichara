pub struct IfStmt;

pub struct ForStmt;

pub struct FuncDeclStmt {
    pub func_id: usize, // id of this 'function'
}

pub struct ReturnStmt {
    pub func_id: usize, // id of the function that this 'return' statement is in
}

pub struct AssignmentStmt;

pub struct VarDeclStmt {
    pub symtbl_pos: usize, // position of this symbol in the symbol table
}

pub enum Stmt {
    If(IfStmt),
    For(ForStmt),
    FuncDecl(FuncDeclStmt),
    Return(ReturnStmt),
    Assignment(AssignmentStmt),
    VarDecl(VarDeclStmt)
}