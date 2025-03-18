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

    /// First usize: Length of the array, second usize: Size of each 
    /// element in the array
    Array(LitTypeArray),

    Null, // null type
    None, // placeholder
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LitTypeArray {
    items_count: usize,
    item_size: usize,
    items_type: Box<LitTypeVariant>
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum LitTypeVariant {
    I32,
    I64,
    I16,
    U8,
    F64,
    F32,
    Void,
    Str,
    Array,
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
            Self::Array => write!(f, "array"),
            _ => write!(f, ""),
        }
    }
}

impl LitTypeVariant {
    pub fn size(&self) -> usize {
        match self {
            Self::I64 
            | Self::F64
            | Self::Str => 8,

            Self::F32 | 
            Self::I32 => 4,

            Self::U8 => 1,
            Self::I16 => 2,
            _ => 0,
        }
    }

    pub fn is_int_variant(&self) -> bool {
        matches!(self, LitTypeVariant::I32 | LitTypeVariant::I16 | LitTypeVariant::I64 | LitTypeVariant::U8)
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
            Self::Array(_) => LitTypeVariant::Array,
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

lazy_static! {
    pub static ref TYPE_PRECEDENCE: std::collections::HashMap<u8, u8> = {
        let mut typ: std::collections::HashMap<u8, u8> = HashMap::new();
        typ.insert(LitTypeVariant::I64 as u8, 3);
        typ.insert(LitTypeVariant::I32 as u8, 2);
        typ.insert(LitTypeVariant::I16 as u8, 1);
        typ.insert(LitTypeVariant::U8 as u8, 0);
        typ
    };
}

pub fn is_type_coalescing_possible(src: LitTypeVariant, dest: LitTypeVariant) -> bool {
    match src {
        LitTypeVariant::U8 => matches!(dest, LitTypeVariant::U8 | LitTypeVariant::I16 | LitTypeVariant::I32 | LitTypeVariant::I64),
        LitTypeVariant::I16 => matches!(dest, LitTypeVariant::I16 | LitTypeVariant::I32 | LitTypeVariant::I64),
        LitTypeVariant::I32 => matches!(dest, LitTypeVariant::I32 | LitTypeVariant::I64),
        _ => false
    }
}

// tests
#[cfg(test)]
mod tests {
    use crate::is_type_coalescing_possible;

    use super::LitTypeVariant;

    #[test]
    // Trying to conver 1223 into unsigned char. This should fail.
    fn test_type_coalescing() {
        assert!(is_type_coalescing_possible(LitTypeVariant::U8, LitTypeVariant::I32));
        assert!(is_type_coalescing_possible(LitTypeVariant::U8, LitTypeVariant::I16));
        assert!(is_type_coalescing_possible(LitTypeVariant::U8, LitTypeVariant::I64));
        assert!(!is_type_coalescing_possible(LitTypeVariant::I32, LitTypeVariant::U8));
        assert!(!is_type_coalescing_possible(LitTypeVariant::I32, LitTypeVariant::I16));
        assert!(is_type_coalescing_possible(LitTypeVariant::I32, LitTypeVariant::I32));
        assert!(is_type_coalescing_possible(LitTypeVariant::I32, LitTypeVariant::I64));
    }
}