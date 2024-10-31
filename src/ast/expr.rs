use core::panic;
use std::collections::HashMap;

use lazy_static::lazy_static;

use crate::{error::BTypeErr, semantic::{sa_errors::{SAError, SATypeError}, type_checker::TypeChecker}, types::{are_compatible_for_operation, LitType, LitTypeVariant}};

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

lazy_static! {
    static ref TYPE_PRECEDENCE_EXPR: std::collections::HashMap<u8, u8> = {
        let mut typ: std::collections::HashMap<u8, u8> = HashMap::new();
        typ.insert(LitTypeVariant::I64 as u8, 3);
        typ.insert(LitTypeVariant::I32 as u8, 2);
        typ.insert(LitTypeVariant::I16 as u8, 1);
        typ.insert(LitTypeVariant::U8 as u8, 0);
        typ
    };
}

impl TypeChecker {
    pub fn infer_type(expr: &Expr) -> Result<LitTypeVariant, SAError> {
        return match expr {
            Expr::LitVal(lit_val_expr) => Ok(lit_val_expr.result_type),
            Expr::Ident(ident_expr) => Ok(ident_expr.result_type),
            Expr::FuncCall(func_call_expr) => Ok(func_call_expr.result_type),
            Expr::Binary(bin_expr) => {
                let left_type: LitTypeVariant = TypeChecker::infer_type(&bin_expr.left)?;
                let right_type: LitTypeVariant = TypeChecker::infer_type(&bin_expr.right)?;
                for typ in [&left_type, &right_type] {
                    match typ {
                        LitTypeVariant::Str 
                        | LitTypeVariant::Array
                        | LitTypeVariant::Null
                        | LitTypeVariant::Void => {
                            return Err(
                                SAError::TypeError(SATypeError::IncompatibleTypes { 
                                    a: left_type, 
                                    b: right_type,
                                    operation: bin_expr.operation 
                                })
                            )
                        },
                        _ => ()
                    };
                }
                if left_type != right_type {
                    let lprec: Option<&u8> = TYPE_PRECEDENCE_EXPR.get(&(left_type as u8));
                    let rprec: Option<&u8> = TYPE_PRECEDENCE_EXPR.get(&(right_type as u8));
                    let lp: u8 = if let Some(lp) = lprec {
                        *lp
                    } else {
                        panic!("Type precedence not defined for operation {:?}", left_type);
                    };
                    let rp: u8 = if let Some(rp) = rprec {
                        *rp
                    } else {
                        panic!("Type precedence not defined for operation {:?}", right_type);
                    };
                    if lp > rp {
                        return Ok(left_type);
                    } else {
                        return Ok(right_type);
                    }
                }
                Ok(left_type)
            }
            _ => Err(SAError::None)
        };
    }
}