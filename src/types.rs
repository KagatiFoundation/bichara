/*
MIT License

Copyright (c) 2023 Kagati Foundation

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

use core::panic;
use std::{collections::HashMap, fmt::Display};

use lazy_static::lazy_static;

use crate::{ast::{ASTKind, ASTOperation, Expr, WidenExpr, AST}, tokenizer::TokenKind};

pub trait BTypeComparable {
    fn cmp(&self, other: &Self) -> bool;

    fn variant(&self) -> LitTypeVariant;
}

pub trait TypeSized {
    fn type_size(&self) -> usize;
}

// Literal value types
#[derive(Debug, Clone)]
pub enum LitType {
    I64(i64), // 64-bit integer
    I32(i32), // 32-bit integer
    I16(i16), // 16-bit integer
    U8(u8),   // 8-bit integer. Also known as 'char' in C programming language.
    F64(f64), // Double-precision floating point number. 64-bit float type.
    F32(f32), // Single-precision floating point number. 32-bit float type.
    Void,     // Void return type
    
    /// Value and the label id
    Str(String, usize),

    Null, // null type
    None, // placeholder
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum LitTypeVariant {
    I32 = 1,
    I64,
    I16,
    U8,
    F64,
    F32,
    Void,
    Str,
    Null,
    None, // placeholder
}

impl Display for LitTypeVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I32 => write!(f, "integer"),
            Self::I64 => write!(f, "long"),
            Self::U8 => write!(f, "byte"),
            Self::Str => write!(f, "str"),
            _ => write!(f, ""),
        }
    }
}

impl LitTypeVariant {
    pub fn from_token_kind(kind: TokenKind) -> Self {
        match kind {
            TokenKind::KW_VOID => Self::Void,
            TokenKind::KW_INT => Self::I32,
            TokenKind::KW_CHAR => Self::U8,
            TokenKind::KW_LONG => Self::I64,
            TokenKind::KW_FLOAT => Self::F32,
            TokenKind::KW_DOUBLE => Self::F64,
            _ => Self::None,
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Self::I64 
            | Self::F64
            | Self::Str => 8,
            Self::F32 | Self::I32 => 4,
            Self::U8 => 1,
            Self::I16 => 2,
            _ => 0,
        }
    }
}

impl PartialEq for LitType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::I32(l0), Self::I32(r0)) => *l0 == *r0,
            (Self::I64(l0), Self::I64(r0)) => *l0 == *r0,
            (Self::I16(l0), Self::I16(r0)) => *l0 == *r0,
            (Self::F64(l0), Self::F64(r0)) => *l0 == *r0,
            (Self::F32(l0), Self::F32(r0)) => *l0 == *r0,
            (Self::U8(l0), Self::U8(r0)) => *l0 == *r0,
            _ => false,
        }
    }
}

impl LitType {
    pub fn from_token_kind(kind: TokenKind) -> Option<Self> {
        match kind {
            TokenKind::T_INT_NUM => Some(LitType::I32(0)),
            TokenKind::T_CHAR => Some(LitType::U8(0)),
            TokenKind::T_DOUBLE_NUM => Some(LitType::F64(0.0)),
            TokenKind::T_LONG_NUM => Some(LitType::I64(0)),
            TokenKind::T_STRING => Some(LitType::Str("".to_string(), 0)),
            TokenKind::KW_VOID => Some(LitType::Void),
            _ => None,
        }
    }

    pub fn is_i32(&self) -> bool {
        matches!(self, Self::I32(_))
    }

    pub fn unwrap_i32(&self) -> i32 {
        match self {
            Self::I32(value) => *value,
            _ => panic!("Can't unwrap i32 value from type: '{:?}'", self),
        }
    }

    pub fn is_char(&self) -> bool {
        matches!(self, Self::U8(_))
    }

    pub fn variant(&self) -> LitTypeVariant {
        match self {
            Self::I32(_) => LitTypeVariant::I32,
            Self::I64(_) => LitTypeVariant::I64,
            Self::I16(_) => LitTypeVariant::I16,
            Self::U8(_) => LitTypeVariant::U8,
            Self::F64(_) => LitTypeVariant::F64,
            Self::F32(_) => LitTypeVariant::F32,
            Self::Str(_, _) => LitTypeVariant::Str,
            Self::Void => LitTypeVariant::Void,
            _ => panic!("not a valid type to calculate variant of!"),
        }
    }

    pub fn size(&self) -> usize {
        match self {
            LitType::I32(_) | LitType::F32(_) => 32,
            LitType::I16(_) => 16,
            LitType::U8(_) => 8,
            _ => 0,
        }
    }

    pub fn coerce_to(&self, target_type: LitType) -> Result<LitType, String> {
        match target_type {
            LitType::I16(_) => self.coerce_to_i16(),
            LitType::I32(_) => self.coerce_to_i32(),
            LitType::I64(_) => self.coerce_to_i64(),
            _ => panic!("Error")
        }
    }

    fn coerce_to_i16(&self) -> Result<LitType, String> {
        match self {
            LitType::U8(val) => Ok(LitType::I16(*val as i16)),
            _ => panic!("Error")
        }
    }

    fn coerce_to_i32(&self) -> Result<LitType, String> {
        match self {
            LitType::U8(val) => Ok(LitType::I32(*val as i32)),
            LitType::I16(val) => Ok(LitType::I32(*val as i32)),
            _ => panic!("Error")
        }
    }

    fn coerce_to_i64(&self) -> Result<LitType, String> {
        match self {
            LitType::U8(val) => Ok(LitType::I64(*val as i64)),
            LitType::I16(val) => Ok(LitType::I64(*val as i64)),
            LitType::I32(val) => Ok(LitType::I64(*val as i64)),
            _ => panic!("Error")
        }
    }
}

impl Display for LitType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let result = match self {
            LitType::I32(value) => format!("{}", *value),
            LitType::U8(value) => format!("{}", *value),
            LitType::Str(value, _) => value.clone(),
            _ => panic!()
        };
        _ = writeln!(f, "{}", result);
        Ok(())
    }
}

impl BTypeComparable for LitType {
    fn cmp(&self, other: &LitType) -> bool {
        self.variant() == other.variant()
    }
    
    fn variant(&self) -> LitTypeVariant {
        self.variant()
    }
}

impl TypeSized for LitType {
    fn type_size(&self) -> usize {
        self.size()
    }
}

/// Represents possible errors that may occur during type conversion.
pub enum TypeConversionError {
    /// Indicates an error where a conversion from a larger size to a 
    /// smaller size is attempted, which may not always be possible 
    /// depending on the context.
    BigSizeToSmallSize {
        /// The source literal type variant from which the conversion was attempted.
        from: LitTypeVariant, 
        /// The target literal type variant to which the conversion was attempted.
        to: LitTypeVariant
    }
}

/// Alias for the result of a type conversion operation.
type TypeConversionResult = Result<AST, TypeConversionError>;

pub fn convert_ast(node: &AST, to: LitTypeVariant) -> TypeConversionResult {
    let ltype: LitTypeVariant = node.result_type;
    let lsize: usize = ltype.size();
    let rsize: usize = to.size();
    // if the AST's result type fits into 'to' type
    if rsize > lsize {
        return Ok(AST::create_leaf(
            ASTKind::ExprAST(Expr::Widen(WidenExpr { 
                from: Box::new(node.clone()), 
                result_type: to
            })),
            ASTOperation::AST_WIDEN,
            to
        ));
    }
    Err(TypeConversionError::BigSizeToSmallSize{ 
        from: ltype, to
    })
}

lazy_static! {
    static ref TYPE_PRECEDENCE: std::collections::HashMap<u8, u8> = {
        let mut typ: std::collections::HashMap<u8, u8> = HashMap::new();
        typ.insert(LitTypeVariant::I32 as u8, 2);
        typ.insert(LitTypeVariant::U8 as u8, 0);
        typ
    };
}

pub fn infer_type_from_expr(expr: &Expr) -> LitTypeVariant {
    match expr {
        Expr::LitVal(lit) => lit.result_type,
        Expr::Binary(bin) => {
            let left_type: LitTypeVariant = infer_type_from_expr(&bin.left);
            let right_type: LitTypeVariant = infer_type_from_expr(&bin.right);
            if left_type == LitTypeVariant::Str || right_type == LitTypeVariant::Str {
                if bin.operation == ASTOperation::AST_ADD {
                    return LitTypeVariant::Str;
                } else {
                    panic!("Type mismatch: {:?} and {:?}", left_type, right_type);
                }
            }
            if left_type != right_type {
                let lprec: Option<&u8> = TYPE_PRECEDENCE.get(&(left_type as u8));
                let rprec: Option<&u8> = TYPE_PRECEDENCE.get(&(right_type as u8));
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
                    return left_type;
                } else {
                    return right_type;
                }
            }
            left_type
        },
        Expr::Ident(ident) => ident.result_type,
        Expr::FuncCall(func_call_expr) => func_call_expr.result_type,
        _ => {
            // I am gonna ignore these types for now (as they are not needed right now)
            panic!("Cannot infer type of the expression.");
        }
    }
}

pub fn are_compatible_for_operation<T: BTypeComparable + TypeSized>(left: &T, right: &T, op: ASTOperation) -> (bool, LitTypeVariant) {
    let ltype = left.variant();
    let rtype = right.variant();
    if ltype == rtype {
        return (true, ltype);
    }
    let mut larger_type = ltype;
    let lsize = left.type_size();
    let rsize = right.type_size();
    if rsize > lsize {
        larger_type = rtype;
    }
    match (ltype, rtype) {
        (LitTypeVariant::I32, LitTypeVariant::U8) |
        (LitTypeVariant::U8, LitTypeVariant::I32) | 
        (LitTypeVariant::I64, LitTypeVariant::I32) |
        (LitTypeVariant::I64, LitTypeVariant::U8) | 
        (LitTypeVariant::I32, LitTypeVariant::I64) | 
        (LitTypeVariant::U8, LitTypeVariant::I64) => {
            if matches!(op, ASTOperation::AST_ADD | ASTOperation::AST_SUBTRACT | ASTOperation::AST_MULTIPLY | ASTOperation::AST_DIVIDE) {
                (true, larger_type)
            } else {
                (false, larger_type)
            }
        },
        _ => (false, larger_type)
    }
}

/// Modify the given node's type into the 'to' type.
pub fn modify_ast_node_type(node: &mut AST, to: LitTypeVariant) -> Option<AST> {
    let ltype: LitTypeVariant = node.result_type;
    let lsize: usize = node.result_type.size();
    let rsize: usize = to.size();
    if ltype == to {
        return Some(node.clone());
    }
    if lsize > rsize {
        // the type we are trying to convert is too big for the target type
        panic!("Can't convert type {:?} into {:?}. Size too large.", ltype, to);
    }
    if rsize > lsize {
        return Some(AST::create_leaf(
            ASTKind::ExprAST(Expr::Widen(WidenExpr{
                from: Box::new(node.clone()),
                result_type: to
            })),
            ASTOperation::AST_WIDEN,
            to,
        ));
    }
    // if we reach here, then types are incompatible
    None
}

pub fn is_type_coalescing_possible(src: LitTypeVariant, dest: LitTypeVariant) -> bool {
    match src {
        LitTypeVariant::U8 => matches!(dest, LitTypeVariant::I16 | LitTypeVariant::I32 | LitTypeVariant::I64),
        LitTypeVariant::I16 => matches!(dest, LitTypeVariant::I32 | LitTypeVariant::I64),
        LitTypeVariant::I32 => matches!(dest, LitTypeVariant::I64),
        _ => false
    }
}

// tests
#[cfg(test)]
mod tests {
    use super::LitType;

    #[test]
    fn test_conversion_from_i32_to_u8() {
        let _a: LitType = LitType::I32(12);
    }

    #[test]
    // Trying to conver 1223 into unsigned char. This should fail.
    fn test_conversion_from_i32_to_u8_with_overflowing_value() {
        let _a: LitType = LitType::I32(1233);
    }
}