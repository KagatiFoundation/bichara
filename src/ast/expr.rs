use crate::types::{LitType, LitTypeVariant};

use super::{ASTOperation, AST};

#[derive(Clone, Debug)]
pub struct BinExpr {
    pub operation: ASTOperation,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub result_type: LitTypeVariant
}

#[derive(Clone, Debug)]
pub struct WidenExpr {
    pub from: Box<AST>,
    pub result_type: LitTypeVariant
}

#[derive(Clone, Debug)]
pub struct IdentExpr {
    pub symtbl_pos: usize, // position in the symbol table
    pub result_type: LitTypeVariant
}

#[derive(Clone, Debug)]
pub struct LitValExpr {
    pub value: LitType,
    pub result_type: LitTypeVariant,
}

#[derive(Clone, Debug)]
pub struct SubscriptExpr {
    pub index: Box<Expr>, // subscript index expression
    pub symtbl_pos: usize, // position of the symbol inside symbol table that is being subscripted
    pub result_type: LitTypeVariant
}

#[derive(Clone, Debug)]
pub struct FuncCallExpr {
    pub symtbl_pos: usize, // position of the function being called in the symbol table,
    // args
    pub result_type: LitTypeVariant, // function return type
}

#[derive(Clone, Debug)]
pub enum Expr {
    Binary(BinExpr),
    Widen(WidenExpr),
    Ident(IdentExpr),
    LitVal(LitValExpr),
    Subscript(SubscriptExpr),
    FuncCall(FuncCallExpr),
    Addr(IdentExpr),
    Deref(IdentExpr)
}

impl Expr {
    pub fn result_type(&self) -> LitTypeVariant {
        match self {
            Self::Binary(bin) => bin.result_type,
            Self::Widen(widen) => widen.result_type,
            Self::Ident(ident) => ident.result_type,
            Self::LitVal(lit) => lit.result_type,
            Self::Subscript(sub) => sub.result_type,
            Self::FuncCall(func) => func.result_type,
            Self::Addr(addr) => addr.result_type,
            Self::Deref(addr) => addr.result_type
        }
    }
}