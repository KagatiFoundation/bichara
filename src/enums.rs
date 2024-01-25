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

use std::str::FromStr;

// Literal Value Type Of The Program
#[derive(Debug, Clone)]
pub enum LitType {
    Integer(i32),
    Long(i64),
    Short(i16),
    Char(u8),
    String(String),
    Double(f64),
    Float(f32),
}

impl PartialEq for LitType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Integer(l0), Self::Integer(r0)) => l0 == r0,
            (Self::Long(l0), Self::Long(r0)) => l0 == r0,
            (Self::Short(l0), Self::Short(r0)) => l0 == r0,
            (Self::Char(l0), Self::Char(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Double(l0), Self::Double(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            _ => false,
        }
    }
}

// Token Type Of The Program
#[derive(Copy, Clone, PartialEq, Hash, Eq, Debug)]
pub enum TokenKind {
    _T_LITERAL_START_, // literal types
    T_FLOAT_NUM, T_DOUBLE_NUM,
    T_INT_NUM, T_SHORT_NUM,
    T_STRING, T_CHAR, 
    T_IDENTIFIER, 
    _T_LITERAL_END_, // literal types end here

    _T_OPERATOR_START_, // operators
    T_PLUS, T_MINUS,
    T_STAR, T_SLASH,
    T_AMPERSAND, T_PIPE,
    T_LTEQ, T_GTEQ, 
    T_EQEQ, T_NEQ,
    T_AND, T_OR, 
    T_LSHIFT, T_RSHIFT,
    T_PLUSEQ, T_MINUSEQ,
    T_STAREQ, T_SLASHEQ,
    T_PERCENTEQ, T_AMPERSANDEQ,
    T_PIPEEQ, T_LSHIFTEQ, 
    T_RSHIFTEQ, T_CARETEQ,
    T_TILDEEQ, T_BANG,
    T_INCR, T_DECR,
    T_CARET, T_PERCENT, 
    T_GTHAN, T_LTHAN,
    L_TILDE, T_ARROW,
    _T_OPERATOR_END_, // operators end here

    // following are the keywords of the C programming language
    _KW_START_,
    KW_FOR, KW_WHILE, 
    KW_INT, KW_FLOAT,
    KW_DOUBLE, KW_CHAR,
    KW_VOID, KW_CONST,
    KW_RETURN, KW_DO,
    KW_CASE, KW_BREAK,
    KW_CONTINUE, KW_DEFAULT,
    KW_ENUM, KW_GOTO,
    KW_REGISTER, KW_SIZEOF,
    KW_TYPEDEF, KW_VOLATILE,
    KW_STRUCT, KW_UNION,
    KW_STATIC, KW_IF,
    KW_ELSE, KW_UNSIGNED,
    KW_SIGNED, KW_LONG,
    KW_SHORT, KW_AUTO,
    KW_SWITCH, KW_EXTERN,
    KW_INLINE,
    _KW_END_, // keywords end here

    // other tokens
    T_LBRACE, T_RBRACE, 
    T_LPAREN, T_RPAREN,
    T_LBRACKET, T_RBRACKET,
    T_DOT, T_COMMA,
    T_SEMICOLON, T_EQUAL, 
    T_HASH, T_COLON, 
    T_QMARK, T_EOF,
    T_NONE,
}

impl TokenKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::T_PLUS => "+",
            Self::T_INCR => "++",
            Self::T_PLUSEQ => "+=",
            Self::T_MINUS => "-",
            Self::T_DECR => "--",
            Self::T_MINUSEQ => "-=",
            Self::T_STAR => "*",
            Self::T_STAREQ => "*=",
            Self::T_GTHAN => ">",
            Self::T_RSHIFT => ">>",
            Self::T_GTEQ => ">=",
            Self::T_RSHIFTEQ => ">>=",
            Self::T_ARROW => "->",
            Self::T_AMPERSAND => "&",
            Self::T_AMPERSANDEQ => "&=",
            Self::T_AND => "&&",
            Self::T_PIPE => "|",
            Self::T_OR => "||",
            Self::T_PIPEEQ => "|=",
            Self::T_SLASH => "/",
            Self::T_SLASHEQ => "/=",
            Self::T_LPAREN => "(",
            Self::T_RPAREN => ")",
            Self::T_PERCENT => "%",
            Self::T_PERCENTEQ => "%=",
            Self::T_CARET => "^",
            Self::T_CARETEQ => "^=",
            Self::L_TILDE => "~",
            Self::T_TILDEEQ => "~=",
            Self::T_SEMICOLON => ";",
            Self::T_EQUAL => "=",
            Self::T_EQEQ => "==",
            Self::T_LBRACE => "{",
            Self::T_RBRACE => "}",
            Self::T_DOT => ".",
            _ => "",
        }
    }
}

impl FromStr for TokenKind {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "(" => Ok(Self::T_LPAREN),
            ")" => Ok(Self::T_RPAREN),
            "{" => Ok(Self::T_LBRACE),
            "}" => Ok(Self::T_RBRACE),
            "[" => Ok(Self::T_LBRACKET),
            "]" => Ok(Self::T_RBRACKET),
            "#" => Ok(Self::T_HASH),
            "." => Ok(Self::T_DOT),
            "?" => Ok(Self::T_QMARK),
            ":" => Ok(Self::T_COLON),
            "," => Ok(Self::T_COMMA),
            ";" => Ok(Self::T_SEMICOLON),
            _ => Ok(Self::T_NONE),
        }
    }
}

// AST Node Types
#[derive(Debug, Eq, PartialEq)]
pub enum ASTNodeKind {
    AST_ADD, // an AST node with "+" as the root node
    AST_SUBTRACT, // an AST node with "-" as the root node
    AST_MULTIPLY, // an AST node with "*" as the root node
    AST_DIVIDE, // an AST node with "/" as the root node
    AST_INTLIT, // a leaf AST node with literal integer value
    AST_IDENT, // a leaf AST node with an identifier name
    AST_LVIDENT, 
    AST_ASSIGN
}