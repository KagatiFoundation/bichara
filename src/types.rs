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
use std::fmt::Display;

use crate::{ast::{ASTKind, ASTOperation, Expr, WidenExpr, AST}, tokenizer::TokenKind};

// Literal value types
#[derive(Debug, Clone)]
pub enum LitType {
    I64(i64), // 64-bit integer
    I32(i32), // 32-bit integer
    I16(i16), // 16-bit integer
    U8(u8),   // 8-bit integer. Also known as 'char' in C programming language.
    F64(f64), // Double-precicion floating point number. 64-bit float type.
    F32(f32), // Single-precicion floating point number. 32-bit float type.
    Void,     // Void return type
    // pointer types
    I64Ptr(u64),
    I32Ptr(u64),
    I16Ptr(u64),
    U8Ptr(u64),
    F64Ptr(u64),
    F32Ptr(u64),
    VoidPtr(u64),
    Str(String), // This type(String) is not supported by this language.
    // I am using this as a identifier name holder in parsing process.
    Null, // null type
    None, // placeholder
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum LitTypeVariant {
    I32,
    I64,
    I16,
    U8,
    F64,
    F32,
    Void,
    I64Ptr,
    I32Ptr,
    I16Ptr,
    U8Ptr,
    F64Ptr,
    F32Ptr,
    VoidPtr,
    Null,
    None, // placeholder
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

    /// Given the primitive type, get a type which is a pointer to it
    pub fn pointer_type(prim_type: LitTypeVariant) -> Self {
        match prim_type {
            Self::I32 => Self::I32Ptr,
            Self::I64 => Self::I64Ptr,
            Self::U8 => Self::U8Ptr,
            Self::Void => Self::VoidPtr,
            Self::F32 => Self::F32Ptr,
            Self::F64 => Self::F64Ptr,
            _ => panic!("Can't take address of this type: '{:?}'", prim_type),
        }
    }

    pub fn value_type(prim_type: LitTypeVariant) -> Self {
        match prim_type {
            Self::I32Ptr => Self::I32,
            Self::I64Ptr => Self::I64,
            Self::U8Ptr => Self::U8,
            Self::VoidPtr => Self::Void,
            Self::F32Ptr => Self::F32,
            Self::F64Ptr => Self::F64,
            _ => panic!("Not supported value type: '{:?}'", prim_type),
        }
    }

    pub fn is_ptr_type(&self) -> bool {
        matches!(
            self,
            Self::I32Ptr | Self::I64Ptr | Self::U8Ptr | Self::VoidPtr | Self::F32Ptr | Self::F64Ptr
        )
    }

    pub fn size(&self) -> usize {
        match self {
            Self::U8Ptr
            | Self::I64
            | Self::F64
            | Self::F32Ptr
            | Self::F64Ptr
            | Self::I16Ptr
            | Self::I32Ptr
            | Self::I64Ptr
            | Self::VoidPtr => 8,
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

impl Display for LitType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "LitType")
    }
}

impl LitType {
    pub fn from_token_kind(kind: TokenKind) -> Option<Self> {
        match kind {
            TokenKind::T_INT_NUM => Some(LitType::I32(0)),
            TokenKind::T_CHAR => Some(LitType::U8(0)),
            TokenKind::T_DOUBLE_NUM => Some(LitType::F64(0.0)),
            TokenKind::T_LONG_NUM => Some(LitType::I64(0)),
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
            Self::Void => LitTypeVariant::Void,
            Self::I64Ptr(_) => LitTypeVariant::I64Ptr,
            Self::I32Ptr(_) => LitTypeVariant::I32Ptr,
            Self::I16Ptr(_) => LitTypeVariant::I16Ptr,
            Self::U8Ptr(_) => LitTypeVariant::U8Ptr,
            Self::F64Ptr(_) => LitTypeVariant::F64Ptr,
            Self::F32Ptr(_) => LitTypeVariant::F32Ptr,
            Self::VoidPtr(_) => LitTypeVariant::VoidPtr,
            _ => panic!("not a valid type to calculate variant of!"),
        }
    }

    pub fn size(&self) -> usize {
        match self {
            LitType::I64(_)
            | LitType::F64(_)
            | LitType::I64Ptr(_)
            | LitType::I32Ptr(_)
            | LitType::I16Ptr(_)
            | LitType::U8Ptr(_)
            | LitType::F64Ptr(_)
            | LitType::F32Ptr(_)
            | LitType::VoidPtr(_) => 64,
            LitType::I32(_) | LitType::F32(_) => 32,
            LitType::I16(_) => 16,
            LitType::U8(_) => 8,
            _ => 0,
        }
    }

    pub fn compatible(&self, other: &Self) -> (bool, u8, u8) {
        let self_size: usize = self.size();
        let other_size: usize = other.size();
        // Same types, they are compatible
        if self.variant() == other.variant() {
            return (true, 0, 0);
        }
        // Types with zero size are not compatible with anything
        if self_size == 0 || other_size == 0 {
            return (false, 0, 0);
        }
        if self_size < other_size {
            return (true, 1, 0);
        }
        if other_size < self_size {
            return (true, 0, 1);
        }
        (true, 0, 0)
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

/// Modify the given node's type into the 'to' type.
pub fn modify_ast_node_type(node: &mut AST, to: LitTypeVariant, op: ASTOperation) -> Option<AST> {
    let ltype: LitTypeVariant = node.result_type;
    let lsize: usize = node.result_type.size();
    let rsize: usize = to.size();
    if !ltype.is_ptr_type() && !to.is_ptr_type() {
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
    }
    if ltype.is_ptr_type() && ltype == to && (op == ASTOperation::AST_NONE) {
        return Some(node.clone());
    }
    // if we reach here, then types are incompatible
    None
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
