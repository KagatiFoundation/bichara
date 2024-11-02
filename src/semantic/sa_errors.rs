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

use std::fmt;

use crate::{ast::ASTOperation, tokenizer::Token, types::LitTypeVariant};

pub enum SATypeError {
    AssignmentTypeMismatch {
        expected: LitTypeVariant, 
        found: LitTypeVariant
    },
    IncompatibleTypes {
        a: LitTypeVariant, 
        b: LitTypeVariant,
        operation: ASTOperation
    },
    TypeMismatch {
        expected: LitTypeVariant, 
        found: LitTypeVariant
    },
    NonCallable {
        sym_name: String
    }
}

pub enum SAError {
    TypeError(SATypeError),
    ArrayLengthError { 
        expected: usize, 
        found: usize 
    },
    UndefinedSymbol { 
        sym_name: String, 
        token: Token
    },
    None
}

impl fmt::Display for SATypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::AssignmentTypeMismatch { expected, found } => {
                write!(f, "Assignment type mismatch: expected type `{}`, found `{}`.", expected, found)
            }
            Self::IncompatibleTypes { a, b, operation } => {
                write!(f, "Incompatible types `{}` and `{}` for operation `{:?}`.", a, b, operation)
            }
            Self::TypeMismatch { expected, found } => {
                write!(f, "Type mismatch: `{}` is not compatible with `{}`.", expected, found)
            },
            Self::NonCallable{ sym_name} => {
                write!(f, "'{}' is not a function.", sym_name)
            }
        }
    }
}

impl fmt::Display for SAError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SAError::TypeError(type_error) => write!(f, "Type Error: {}", type_error),
            SAError::ArrayLengthError { expected, found } => {
                write!(f, "Array length mismatch: expected `{}`, found `{}`.", expected, found)
            },
            SAError::UndefinedSymbol{ sym_name, token} => {
                write!(f, "{}:{}: compile error: Undefined symbol '{}'", token.pos.line, token.pos.column, sym_name)
            },
            SAError::None => write!(f, "No error."),
        }
    }
}

impl SAError {
    pub fn dump(&self) {
        println!("{}", self);
        std::process::exit(1);
    }
}