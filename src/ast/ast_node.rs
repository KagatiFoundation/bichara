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

#![allow(non_camel_case_types)]

use crate::{tokenizer::TokenKind, types::{BTypeComparable, LitTypeVariant, TypeSized}};

use super::ASTKind;

#[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
pub enum ASTOperation {
    // below this are relational operators
    AST_EQEQ = 0,  // equal equal
    AST_NEQ,   // not equal
    AST_LTEQ,  // less than or equal to
    AST_GTEQ,  // greate than equal to
    AST_GTHAN, // greater than
    AST_LTHAN, // less than

    AST_NONE,     // used as a placeholder
    AST_ADD = 10, // an AST node with "+" as the root node
    AST_SUBTRACT, // an AST node with "-" as the root node
    AST_MULTIPLY, // an AST node with "*" as the root node
    AST_DIVIDE,   // an AST node with "/" as the root node
    // end of relational operators
    AST_INTLIT, // a leaf AST node with literal integer value
    AST_IDENT,  // a leaf AST node with an identifier name
    AST_LVIDENT,
    AST_ASSIGN,
    AST_GLUE,
    AST_IF,
    AST_WHILE,
    AST_LOOP,
    AST_BREAK,
    AST_FUNCTION,
    AST_FUNC_CALL,
    AST_RETURN,       // return statement AST node
    AST_ADDR,         // for address-of operator
    AST_DEREF,        // for dereferencing operator
    AST_WIDEN,        // need to widen the tree
    AST_ARRAY_ACCESS, // access array element
    AST_STRLIT, // string literal node
    AST_VAR_DECL,
    AST_ARR_VAR_DECL
}

impl ASTOperation {
    pub fn from_token_kind(kind: TokenKind) -> Self {
        match kind {
            TokenKind::T_PLUS => Self::AST_ADD,
            TokenKind::T_MINUS => Self::AST_SUBTRACT,
            TokenKind::T_STAR => Self::AST_MULTIPLY,
            TokenKind::T_SLASH => Self::AST_DIVIDE,
            TokenKind::T_EQEQ => Self::AST_EQEQ,
            TokenKind::T_NEQ => Self::AST_NEQ,
            TokenKind::T_GTHAN => Self::AST_GTHAN,
            TokenKind::T_LTHAN => Self::AST_LTHAN,
            TokenKind::T_GTEQ => Self::AST_GTEQ,
            TokenKind::T_LTEQ => Self::AST_LTEQ,
            _ => unimplemented!("Not implemented for now!"),
        }
    }
}

/// Represents a node in the Abstract Syntax Tree (AST).
///
/// The `AST` struct encapsulates various properties of an AST node, 
/// including its kind, operation, child nodes (left, mid, and right), 
/// value, and result type. Each node in the AST corresponds to a specific 
/// operation or expression in the source code.
/// 
/// # Fields
///
/// * `kind` - An enum representing the kind of AST node (`ASTKind`).
/// * `operation` - An enum representing the operation performed by the AST 
///     node (`ASTOperation`).
/// * `left` - A boxed optional child node representing the left subtree of 
///     the current node.
/// * `mid` - A boxed optional child node representing the middle subtree of 
///     the current node.
/// * `right` - A boxed optional child node representing the right subtree of 
///     the current node.
/// * `value` - An optional literal value associated with the AST node (`LitType`).
/// * `result_type` - The variant of literal type representing the result type of 
///     the AST node (`LitTypeVariant`).
///
/// # Examples
///
/// ```
/// // Create a new AST node representing an addition operation
/// let ast_node = AST {
///     kind: ASTKind::Expr(...),
///     operation: ASTOperation::Addition,
///     left: Box::new(Some(left_ast_node)),
///     mid: Box::new(None), // No middle subtree
///     right: Box::new(Some(right_ast_node)),
///     value: None, // No associated literal value
///     result_type: LitTypeVariant::Int, // Result type is integer
/// };
/// ```
#[derive(Clone, Debug)]
pub struct AST {
    pub kind: ASTKind,
    pub operation: ASTOperation,
    pub left: Option<Box<AST>>,
    pub mid: Option<Box<AST>>,
    pub right: Option<Box<AST>>,
    pub result_type: LitTypeVariant
}

impl AST {
    pub fn empty() -> Self {
        Self {
            kind: ASTKind::Empty,
            operation: ASTOperation::AST_NONE,
            left: None,
            right: None,
            mid: None,
            result_type: LitTypeVariant::None
        }
    }

    pub fn new(
        kind: ASTKind, 
        op: ASTOperation, 
        left: Option<AST>, 
        right: Option<AST>, 
        result_type: LitTypeVariant
    ) -> Self {
        Self {
            kind,
            operation: op,
            left: left.map(Box::new),
            mid: None,
            right: right.map(Box::new),
            result_type
        }
    }

    pub fn create_leaf(
        kind: ASTKind,
        operation: ASTOperation, 
        result_type: LitTypeVariant
    ) -> Self {
        Self {
            kind,
            operation,
            left: None,
            mid: None,
            right: None,
            result_type
        }
    }
    
    pub fn with_mid(
        kind: ASTKind, 
        op: ASTOperation, 
        left: Option<AST>, 
        mid: Option<AST>, 
        right: Option<AST>, 
        result_type: LitTypeVariant
    ) -> Self {
        Self {
            kind,
            operation: op,
            left: left.map(Box::new),
            mid: mid.map(Box::new),
            right: right.map(Box::new),
            result_type
        }
    }
}

impl BTypeComparable for AST {
    fn cmp(&self, other: &AST) -> bool {
        self.result_type == other.result_type
    }
    
    fn variant(&self) -> LitTypeVariant {
        self.result_type
    }
}

impl TypeSized for AST {
    fn type_size(&self) -> usize {
        self.result_type.size()
    }
}