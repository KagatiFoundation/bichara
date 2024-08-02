use core::panic;

use crate::{error::BTypeErr, types::{are_compatible_for_operation, LitType, LitTypeVariant}};

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
    pub result_type: LitTypeVariant, // function return type
    // args
    pub args: Vec<Expr>
}

#[derive(Clone, Debug)]
pub enum Expr {
    Binary(BinExpr),
    Widen(WidenExpr),
    Ident(IdentExpr),
    LitVal(LitValExpr),
    Subscript(SubscriptExpr),
    FuncCall(FuncCallExpr),
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
        }
    }

    pub fn eval(&self) -> Result<LitType, BTypeErr> {
        match self {
            Expr::LitVal(lit_val) => Ok(lit_val.value.clone()),
            Expr::Binary(bin) => {
                let left_evaled: Result<LitType, BTypeErr> = bin.left.eval();
                let right_evaled: Result<LitType, BTypeErr> = bin.right.eval();
                if let (Ok(left_type), Ok(right_type)) = (left_evaled, right_evaled) {
                    let compat_res: (bool, LitTypeVariant) = are_compatible_for_operation::<LitType>(&left_type, &right_type, bin.operation);
                    if !compat_res.0 {
                        return Err(BTypeErr::IncompatibleTypes { 
                            first_type: left_type.to_string(), 
                            second_type: right_type.to_string(), 
                            operator: format!("{:?}", bin.operation)
                        });
                    }
                    match bin.operation {
                        ASTOperation::AST_ADD => match (&left_type, &right_type) {
                            (LitType::I64(l), LitType::I64(r)) => Ok(LitType::I64(l + r)),
                            (LitType::I32(l), LitType::I32(r)) => Ok(LitType::I32(l + r)),
                            (LitType::I16(l), LitType::I16(r)) => Ok(LitType::I16(l + r)),
                            (LitType::U8(l), LitType::U8(r)) => Ok(LitType::U8(l + r)),
                            (LitType::F64(l), LitType::F64(r)) => Ok(LitType::F64(l + r)),
                            (LitType::F32(l), LitType::F32(r)) => Ok(LitType::F32(l + r)),
                            _ => Err(BTypeErr::IncompatibleTypes { 
                                first_type: left_type.to_string(), 
                                second_type: right_type.to_string(), 
                                operator: format!("{:?}", bin.operation)
                            })
                        }
                        _ => panic!()
                    }
                } 
                else {
                    panic!()
                }
            }
            Expr::Ident(_) => {
                panic!("Identifiers in an expression is not supported at compile-time expression evaluation.");
            }
            _ => panic!()
        }
    }
}