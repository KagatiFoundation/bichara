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

use crate::types::{LitType, LitTypeVariant};

use super::ASTKind;

#[derive(Clone, Copy, PartialEq)]
pub enum ASTOperation {
    AST_NONE,     // used as a placeholder
    AST_ADD = 10, // an AST node with "+" as the root node
    AST_SUBTRACT, // an AST node with "-" as the root node
    AST_MULTIPLY, // an AST node with "*" as the root node
    AST_DIVIDE,   // an AST node with "/" as the root node
    // below this are relational operators
    AST_EQEQ,  // equal equal
    AST_NEQ,   // not equal
    AST_LTEQ,  // less than or equal to
    AST_GTEQ,  // greate than equal to
    AST_GTHAN, // greater than
    AST_LTHAN, // less than
    // end of relational operators
    AST_INTLIT, // a leaf AST node with literal integer value
    AST_IDENT,  // a leaf AST node with an identifier name
    AST_LVIDENT,
    AST_ASSIGN,
    AST_GLUE,
    AST_IF,
    AST_WHILE,
    AST_FUNCTION,
    AST_FUNC_CALL,
    AST_RETURN,       // return statement AST node
    AST_ADDR,         // for address-of operator
    AST_DEREF,        // for dereferencing operator
    AST_WIDEN,        // need to widen the tree
    AST_ARRAY_ACCESS, // access array element
    AST_STRLIT, // string literal node
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
pub struct AST {
    pub kind: ASTKind,
    pub operation: ASTOperation,
    pub left: Box<Option<AST>>,
    pub mid: Box<Option<AST>>,
    pub right: Box<Option<AST>>,
    pub value: Option<LitType>,
    pub result_type: LitTypeVariant
}