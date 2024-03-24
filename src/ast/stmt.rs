#[derive(Clone, Debug)]
pub struct FuncDeclStmt {
    pub func_id: usize, // id of this 'function'
}

#[derive(Clone, Debug)]
pub struct ReturnStmt {
    pub func_id: usize, // id of the function that this 'return' statement is in
}

#[derive(Clone, Debug)]
pub struct VarDeclStmt {
    pub symtbl_pos: usize, // position of this symbol in the symbol table
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Glue,
    If,
    For,
    While,
    FuncDecl(FuncDeclStmt),
    Return(ReturnStmt),
    Assignment,
    VarDecl(VarDeclStmt),
    LValue(usize), // usize for symbol table position of this left value
}