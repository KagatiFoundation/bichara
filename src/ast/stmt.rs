use crate::StorageClass;

use super::Expr;

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
    pub class: StorageClass
}

#[derive(Clone, Debug)]
pub struct ArrVarDeclStmt {
    pub symtbl_pos: usize,
    pub class: StorageClass,
    pub vals: Vec<Expr>
}

#[derive(Clone, Debug)]
pub struct AssignStmt {
    pub symtbl_pos: usize
}

#[derive(Clone, Debug)]
pub struct FuncCallStmt {
    pub symtbl_pos: usize,
    pub args: Vec<Expr>
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Glue,
    If,
    For,
    While,
    Loop,
    Break,
    ArrVarDecl(ArrVarDeclStmt),
    FuncDecl(FuncDeclStmt),
    Return(ReturnStmt),
    Assignment(AssignStmt),
    VarDecl(VarDeclStmt),
    LValue(usize), // usize for symbol table position of this left value
    FuncCall(FuncCallStmt),
}