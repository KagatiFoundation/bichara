use kagc_symbol::StorageClass;

use crate::ir_types::*;

#[derive(Debug)]
pub enum IRInstr {
    Mov(IRLitType, IRLitType),
    Add(IRLitType, IRLitType, IRLitType),
    Call(String, Vec<IRLitType>, IRLitType)
}

#[derive(Debug)]
pub struct IRFunc {
    pub name: String,
    pub params: Vec<IRLitType>,
    pub body: Vec<IR>,
    pub class: StorageClass,
    pub is_leaf: bool
}

#[derive(Debug)]
pub struct IRVarDecl {
    pub sym_name: String,
    pub class: StorageClass,
    pub value: IRInstr,
    pub offset: Option<usize>
}

#[derive(Debug)]
pub enum IR {
    Func(IRFunc),
    VarDecl(IRVarDecl)
}