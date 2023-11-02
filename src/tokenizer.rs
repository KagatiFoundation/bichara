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

#![allow(unused)]
#![allow(non_camel_case_types)]

use std::collections::HashMap;

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
    L_TILDE,
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
    T_QMARK, T_NONE,
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
            _ => "",
        }
    }
}

extern crate lazy_static;
use lazy_static::lazy_static;

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenKind> = {
        let mut _keys: HashMap<&'static str, TokenKind> = HashMap::new();
        _keys.insert("for", TokenKind::KW_FOR);
        _keys.insert("while", TokenKind::KW_FOR);
        _keys.insert("int", TokenKind::KW_INT);
        _keys.insert("float", TokenKind::KW_FLOAT);
        _keys.insert("double", TokenKind::KW_DOUBLE);
        _keys.insert("char", TokenKind::KW_CHAR,);
        _keys.insert("void", TokenKind::KW_VOID,);
        _keys.insert("const", TokenKind::KW_CONST,);
        _keys.insert("return", TokenKind::KW_RETURN,);
        _keys.insert("do", TokenKind::KW_DO,);
        _keys.insert("case", TokenKind::KW_CASE,);
        _keys.insert("break", TokenKind::KW_BREAK,);
        _keys.insert("continue", TokenKind::KW_CONTINUE);
        _keys.insert("default", TokenKind::KW_DEFAULT);
        _keys.insert("enum", TokenKind::KW_ENUM);
        _keys.insert("goto", TokenKind::KW_GOTO);
        _keys.insert("register", TokenKind::KW_REGISTER);
        _keys.insert("sizeof", TokenKind::KW_SIZEOF);
        _keys.insert("typedef", TokenKind::KW_TYPEDEF);
        _keys.insert("volatile", TokenKind::KW_VOLATILE);
        _keys.insert("struct", TokenKind::KW_STRUCT);
        _keys.insert("union", TokenKind::KW_UNION);
        _keys.insert("static", TokenKind::KW_STATIC);
        _keys.insert("inline", TokenKind::KW_INLINE);
        _keys.insert("if", TokenKind::KW_IF);
        _keys.insert("else", TokenKind::KW_ELSE);
        _keys.insert("unsigned", TokenKind::KW_UNSIGNED);
        _keys.insert("signed", TokenKind::KW_SIGNED);
        _keys.insert("long", TokenKind::KW_LONG);
        _keys.insert("short", TokenKind::KW_SHORT);
        _keys.insert("auto", TokenKind::KW_AUTO);
        _keys.insert("switch", TokenKind::KW_SWITCH);
        _keys.insert("extern", TokenKind::KW_EXTERN);
        _keys
    };
}

#[derive(Debug)]
pub struct TokenPos {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
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

pub struct Tokenizer {
    line: usize,
    source: String,
    keywords: HashMap<String, TokenKind>
}

impl Tokenizer {
    pub fn new(source: String) -> Tokenizer {
        let mut keywords: HashMap<String, TokenKind> = HashMap::new();
        Tokenizer {
            line: 1, source,
            keywords
        }
    }
}

impl Tokenizer {
    pub fn start_scan(&mut self) -> Vec<Token> {
        let mut result: Vec<Token> = Vec::new();
        let cloned_source: String = self.source.clone();
        let lines = cloned_source.split('\n');
        for line in lines {
            let tokens: Vec<Token> = self.scan_line(line);
            result.extend(tokens);
        }
        result
    }

    fn scan_line(&mut self, line: &str) -> Vec<Token> {
        let mut tokens: Vec<Token> = vec![];
        let line_len: usize = line.len();
        let mut current: usize = 0;
        let line_copy: String = line.to_string();
        let line_bytes: &[u8] = line_copy.as_bytes();

        while current < line_len {
            let token_start: usize = current;
            let mut chr: char = line_bytes[current] as char;
            current += 1;
            let mut token: Token = Token::new(TokenKind::T_NONE, String::from(""), TokenPos{line: 0, column: 0});
            match chr {
                '+' => {
                    token.kind = TokenKind::T_PLUS;
                    chr = line_bytes[current] as char;
                    match chr {
                        '+' => token.kind = TokenKind::T_INCR,
                        '=' => token.kind = TokenKind::T_PLUSEQ,
                        _ => {}
                    }
                },
                '-' => {
                    token.kind = TokenKind::T_MINUS;
                    chr = line_bytes[current] as char;
                    match chr {
                        '-' => token.kind = TokenKind::T_DECR,
                        '=' => token.kind = TokenKind::T_MINUSEQ,
                        _ => {}
                    }
                },
                '*' => {
                    token.kind = TokenKind::T_STAR;
                    chr = line_bytes[current] as char;
                    if chr == '=' {
                        token.kind = TokenKind::T_STAREQ;
                    }
                },
                '/' => {
                    token.kind = TokenKind::T_SLASH;
                    chr = line_bytes[current] as char;
                    if chr == '=' {
                        token.kind = TokenKind::T_SLASHEQ;
                    }
                },
                '!' => {
                    token.kind = TokenKind::T_BANG;
                    chr = line_bytes[current] as char;
                    if chr == '=' {
                        token.kind = TokenKind::T_NEQ;
                    }
                },
                '%' => {
                    token.kind = TokenKind::T_PERCENT;
                    chr = line_bytes[current] as char;
                    if chr == '=' {
                        token.kind = TokenKind::T_PERCENTEQ;
                    }
                },
                '^' => {
                    token.kind = TokenKind::T_CARET;
                    chr = line_bytes[current] as char;
                    if chr == '=' {
                        token.kind = TokenKind::T_CARETEQ;
                    }
                },
                '&' => {
                    token.kind = TokenKind::T_AMPERSAND;
                    chr = line_bytes[current] as char;
                    if chr == '=' {
                        token.kind = TokenKind::T_AMPERSANDEQ;
                    }
                },
                '#' => token.kind = TokenKind::T_HASH,
                '(' => token.kind = TokenKind::T_LPAREN,
                _ => {}
            }
            token.lexeme = String::from(token.kind.as_str());
            token.pos = TokenPos{ line: self.line, column: token_start };
            tokens.push(token);
        }
        tokens
    }

    #[inline]
    fn construct_token(&self, kind: TokenKind, lexeme: &str, column: usize) -> Token {
        Token { kind, lexeme: String::from(lexeme), pos: TokenPos { line: self.line, column } }
    }
}