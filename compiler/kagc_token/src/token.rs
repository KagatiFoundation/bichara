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

use super::TokenKind;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct TokenPos {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub pos: TokenPos,
}

impl Token {
    #[inline]
    pub fn new(kind: TokenKind, lexeme: String, pos: TokenPos) -> Token {
        Token { kind, lexeme, pos }
    }

    // to mark something as erronous token
    #[inline]
    pub fn none() -> Token {
        Token {
            kind: TokenKind::T_NONE,
            lexeme: String::from(""),
            pos: TokenPos { line: 0, column: 0 }
        }
    }
}