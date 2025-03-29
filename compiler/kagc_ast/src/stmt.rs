use kagc_symbol::StorageClass;
use kagc_types::LitTypeVariant;

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

    /// Name of the symbol
    pub sym_name: String,

    pub class: StorageClass
}

#[derive(Clone, Debug)]
pub struct ArrVarDeclStmt {
    pub symtbl_pos: usize,

    /// Name of the symbol
    pub sym_name: String,

    pub class: StorageClass,
    pub vals: Vec<Expr>
}

#[derive(Clone, Debug)]
pub struct AssignStmt {
    /// Name of the symbol
    pub sym_name: String
}

#[derive(Clone, Debug)]
pub struct FuncCallStmt {
    #[deprecated]
    pub symtbl_pos: usize,

    pub symbol_name: String,

    pub args: Vec<Expr>,

    pub result_type: LitTypeVariant
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
    LValue2 {
        name: String
    },
    FuncCall(FuncCallStmt),
}