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

use core::num;
use std::{collections::HashMap, str::FromStr, cmp};

extern crate lazy_static;
use lazy_static::lazy_static;

use crate::error::*;
use crate::utils::*;
use crate::enums::*;

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

#[derive(Debug, PartialEq, Eq)]
enum ErrorType {
    UnterminatedString,
    InvalidNumericValue
}

#[derive(Debug, PartialEq)]
enum TokenizationResult {
    Success(Token),
    Error(ErrorType, TokenPos)
}

pub struct Tokenizer {
    line: usize,
    source: &'static [u8],
    keywords: HashMap<String, TokenKind>,
    curr_char: char, // current char
    next_char_pos: usize, // position from the start
    col_counter: usize, // column counter
}

impl Tokenizer {
    pub fn new(source: &'static str) -> Tokenizer {
        let mut keywords: HashMap<String, TokenKind> = HashMap::new();
        Tokenizer {
            line: 1, 
            source: source.as_bytes(),
            keywords,
            curr_char: ' ', // space 
            next_char_pos: 0,
            col_counter: 1
        }
    }
}

impl Tokenizer {
    pub fn start_scan(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        let source_len: usize = self.source.len();
        self.advance_to_next_char_pos();
        loop {
            let mut result: TokenizationResult = self.get_token();
            match result {
                TokenizationResult::Success(mut token) => {
                    if token.lexeme.is_empty() {
                        token.lexeme = String::from(token.kind.as_str());
                    }
                    if token.kind == TokenKind::T_EOF {
                        tokens.push(token);
                        break;
                    }
                    if token.kind != TokenKind::T_NONE {
                        tokens.push(token);
                    } 
                },
                TokenizationResult::Error(err_type, pos) => {
                    match err_type {
                        ErrorType::UnterminatedString => {
                            error(pos, "missing terminating '\"' character");
                        },
                        _ => {
                            error(pos, "just the error");
                        }
                    }
                }
            }
        }
        tokens
    }

    fn get_token(&mut self) -> TokenizationResult {
        let mut token: Token = Token::new(TokenKind::T_NONE, String::from(""), TokenPos{line: 0, column: 0});
        let col: usize = self.col_counter - 1;
        let line: usize = self.line;
        let token_pos: TokenPos = TokenPos { line, column: col };
        match self.curr_char {
            '+' => {
                token.kind = TokenKind::T_PLUS;
                self.advance_to_next_char_pos();
                match self.curr_char {
                    '+' => {
                        token.kind = TokenKind::T_INCR;
                        self.advance_to_next_char_pos();
                    },
                    '=' => {
                        token.kind = TokenKind::T_PLUSEQ;
                        self.advance_to_next_char_pos();
                    }
                    _ => {}
                }
            },
            '-' => {
                token.kind = TokenKind::T_MINUS;
                self.advance_to_next_char_pos();
                match self.curr_char {
                    '-' => {
                        token.kind = TokenKind::T_DECR;
                        self.advance_to_next_char_pos();
                    },
                    '=' => {
                        token.kind = TokenKind::T_MINUSEQ;
                        self.advance_to_next_char_pos();
                    }
                    '>' => {
                        token.kind = TokenKind::T_ARROW;
                        self.advance_to_next_char_pos();
                    }
                    _ => {}
                }
            },
            '*' => {
                token.kind = TokenKind::T_STAR;
                self.advance_to_next_char_pos();
                if self.curr_char == '=' {
                    token.kind = TokenKind::T_STAREQ;
                    self.advance_to_next_char_pos();
                }
            },
            '/' => {
                token.kind = TokenKind::T_SLASH;
                self.advance_to_next_char_pos();
                if self.curr_char == '=' {
                    token.kind = TokenKind::T_SLASHEQ;
                    self.advance_to_next_char_pos();
                }
            },
            '!' => {
                token.kind = TokenKind::T_BANG;
                self.advance_to_next_char_pos();
                if self.curr_char == '=' {
                    token.kind = TokenKind::T_NEQ;
                    self.advance_to_next_char_pos();
                }
            },
            '%' => {
                token.kind = TokenKind::T_PERCENT;
                self.advance_to_next_char_pos();
                if self.curr_char == '=' {
                    token.kind = TokenKind::T_PERCENTEQ;
                    self.advance_to_next_char_pos();
                }
            },
            '^' => {
                token.kind = TokenKind::T_CARET;
                self.advance_to_next_char_pos();
                if self.curr_char == '=' {
                    token.kind = TokenKind::T_CARETEQ;
                    self.advance_to_next_char_pos();
                }
            },
            '&' => {
                token.kind = TokenKind::T_AMPERSAND;
                self.advance_to_next_char_pos();
                if self.curr_char == '=' {
                    token.kind = TokenKind::T_AMPERSANDEQ;
                    self.advance_to_next_char_pos();
                }
            },
            '>' => {
                token.kind = TokenKind::T_GTHAN;
                self.advance_to_next_char_pos();
                match self.curr_char {
                    '>' => {
                        token.kind = TokenKind::T_RSHIFT;
                        self.advance_to_next_char_pos();
                        if self.curr_char == '=' {
                            self.advance_to_next_char_pos();
                            token.kind = TokenKind::T_RSHIFTEQ;
                        }
                    },
                    '=' => token.kind = TokenKind::T_GTEQ,
                    _ => {},
                }
            },
            '<' => {
                token.kind = TokenKind::T_LTHAN;
                self.advance_to_next_char_pos();
                match self.curr_char {
                    '<' => {
                        token.kind = TokenKind::T_LSHIFT;
                        self.advance_to_next_char_pos();
                        if self.curr_char == '=' {
                            self.advance_to_next_char_pos();
                            token.kind = TokenKind::T_LSHIFTEQ;
                        }
                    }
                    '=' => token.kind = TokenKind::T_LTEQ,
                    _ => {}
                }
            },
            '|' => {
                token.kind = TokenKind::T_PIPE;
                self.advance_to_next_char_pos();
                match self.curr_char {
                    '|' => {
                        token.kind = TokenKind::T_OR;
                        self.advance_to_next_char_pos();
                    },
                    '=' => {
                        self.advance_to_next_char_pos();
                        token.kind = TokenKind::T_PIPEEQ;
                    },
                    _ => {}
                }
            },
            '&' => {
                token.kind = TokenKind::T_AMPERSAND;
                self.advance_to_next_char_pos();
                match self.curr_char {
                    '&' => {
                        token.kind = TokenKind::T_AND;
                        self.advance_to_next_char_pos();
                    },
                    '=' => {
                        self.advance_to_next_char_pos();
                        token.kind = TokenKind::T_AMPERSANDEQ;
                    },
                    _ => {}
                }
            },
            '~' => {
                token.kind = TokenKind::L_TILDE;
                self.advance_to_next_char_pos();
                if self.curr_char == '=' {
                    self.advance_to_next_char_pos();
                    token.kind = TokenKind::T_TILDEEQ;
                }
            },
            '=' => {
                token.kind = TokenKind::T_EQUAL;
                self.advance_to_next_char_pos();
                if self.curr_char == '=' {
                    self.advance_to_next_char_pos();
                    token.kind = TokenKind::T_EQEQ;
                }
            },
            '0'..='9' => return self.parse_number_from(token_pos), 
            '_' | 'a'..='z' | 'A'..='Z' => {
                let __start: usize = self.next_char_pos - 1;
                let mut __end: usize = __start;
                while self.curr_char.is_alphanumeric() || self.curr_char == '_' {
                    self.advance_to_next_char_pos();
                    __end += 1;
                }
                token.kind = TokenKind::T_IDENTIFIER;
                let name: &str = std::str::from_utf8(&self.source[__start..__end]).unwrap();
                let keyword: Option<&TokenKind> = KEYWORDS.get(name);
                if let Some(key) = keyword {
                    token.kind = *key;
                } 
                token.lexeme = String::from(name);
            },
            '"' => {
                self.advance_to_next_char_pos(); // skip '"'
                let __start: usize = self.next_char_pos - 1;
                let mut __end: usize = __start;
                while self.curr_char != '"' && !self.is_at_end() {
                    self.advance_to_next_char_pos();
                    __end += 1;
                }
                if self.is_at_end() {
                    self.advance_to_next_char_pos();
                    return TokenizationResult::Error(ErrorType::UnterminatedString, token_pos);
                } else {
                    self.advance_to_next_char_pos();
                    let str_val: &str = std::str::from_utf8(&self.source[__start..__end]).unwrap();
                    token.kind = TokenKind::T_STRING;
                    token.lexeme = String::from(str_val);
                }
            },
            '(' | ')' | '{' | '}' | '[' | ']' | '#' | '.' | '?' | ':' | ',' | ';' => {
                token.kind = TokenKind::from_str(self.curr_char.to_string().as_str()).unwrap();
                self.advance_to_next_char_pos();
            },
            ' ' | '\n' | '\t' => self.advance_to_next_char_pos(),
            '\0' => token.kind = TokenKind::T_EOF,
            _ => {}
        }
        token.pos = TokenPos{ line, column: col };
        TokenizationResult::Success(token)
    }

    fn parse_number_from(&mut self, pos: TokenPos) -> TokenizationResult {
        let mut token: Token = Token::new(TokenKind::T_NONE, String::from(""), pos);
        token.kind = TokenKind::T_INT_NUM;
        let mut __start: usize = self.next_char_pos - 1;
        let mut __end: usize = __start;
        let mut period_detected: bool = false;
        while self.curr_char.is_ascii_digit() {
            self.advance_to_next_char_pos();
            __end += 1;
        }
        if self.curr_char == '.' {
            self.advance_to_next_char_pos(); // skip '.'
            if self.curr_char.is_ascii_digit() {
                period_detected = true;
                __end += 1;
                while self.curr_char.is_ascii_digit() {
                    self.advance_to_next_char_pos();
                    __end += 1;
                }
            } else {
                return TokenizationResult::Error(ErrorType::InvalidNumericValue, pos);
            }
        }
        let invalid_num_end: bool = self.curr_char.is_alphabetic() || self.curr_char == '_';
        if invalid_num_end {
            while self.curr_char.is_alphanumeric() || self.curr_char == '_' {
                self.advance_to_next_char_pos();
            }
            return TokenizationResult::Error(ErrorType::InvalidNumericValue, pos);
        }
        let number: &str = std::str::from_utf8(&self.source[__start..__end]).unwrap();
        let mut num_result: String = String::from("");
        if period_detected {
            token.kind = TokenKind::T_DOUBLE_NUM;
        }
        token.lexeme = String::from(number);
        TokenizationResult::Success(token)
    }

    #[inline]
    fn is_at_end(&self) -> bool {
        self.next_char_pos >= self.source.len()
    }

    #[inline]
    fn advance_to_next_char_pos(&mut self) {
        #[allow(clippy::comparison_chain)]
        if self.next_char_pos < self.source.len() {
            self.curr_char = self.source[self.next_char_pos] as char;
            if self.curr_char == '\n' {
                self.line += 1;
                self.col_counter = 0;
            }
            self.next_char_pos += 1;
            self.col_counter += 1;
        }
        else {
            self.curr_char = '\0';
        }
    }
}

// tests
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int_var_decl_tokenization() {
        let mut tok: Tokenizer = Tokenizer::new("int a = 4;");
        let tokens: Vec<Token> = tok.start_scan();
        assert!(tokens.len() == 6);
        assert_eq!(tokens[0].kind, TokenKind::KW_INT);
        assert_eq!(tokens[1].kind, TokenKind::T_IDENTIFIER);
        assert_eq!(tokens[2].kind, TokenKind::T_EQUAL);
        assert_eq!(tokens[3].kind, TokenKind::T_INT_NUM);
        assert_eq!(tokens[4].kind, TokenKind::T_SEMICOLON);
        assert_eq!(tokens[5].kind, TokenKind::T_EOF);
    }

    #[test]
    fn test_should_report_unterminated_string_error() {
        let mut tok: Tokenizer = Tokenizer::new("\"ramesh;");
        tok.advance_to_next_char_pos();
        assert!(matches!(tok.get_token(), TokenizationResult::Error(ErrorType::UnterminatedString, _)));
    }

    #[test]
    fn test_should_report_invalid_numeric_value_error() {
        let mut tok: Tokenizer = Tokenizer::new("2323.");
        tok.advance_to_next_char_pos();
        assert!(matches!(tok.get_token(), TokenizationResult::Error(ErrorType::InvalidNumericValue, _)));
    }
    
    #[test]
    fn test_should_report_invalid_numeric_value_error2() {
        let mut tok: Tokenizer = Tokenizer::new("2323sdsd");
        tok.advance_to_next_char_pos();
        assert!(matches!(tok.get_token(), TokenizationResult::Error(ErrorType::InvalidNumericValue, _)));
    }
    
    #[test]
    fn test_should_report_invalid_numeric_value_error3() {
        let mut tok: Tokenizer = Tokenizer::new(".98989");
        let tokens: Vec<Token> = tok.start_scan();
        assert_eq!(tokens[0].kind, TokenKind::T_DOT);
        assert_eq!(tokens[1].kind, TokenKind::T_INT_NUM);
    }
    
    #[test]
    fn test_int_var_decl_len_correct() {
        let mut tok: Tokenizer = Tokenizer::new("int a = 433434;");
        let tokens: Vec<Token> = tok.start_scan();
        assert!(tokens.len() == 6);
        assert_eq!(tokens[3].lexeme.len(), 6);
    }
    
    #[test]
    fn test_float_var_decl_len_correct() {
        let mut tok: Tokenizer = Tokenizer::new("double a = 4334.34;");
        let tokens: Vec<Token> = tok.start_scan();
        assert!(tokens.len() == 6);
        assert_eq!(tokens[3].lexeme, "4334.34");
        assert_eq!(tokens[3].lexeme.len(), 7);
    }
    
    #[test]
    fn test_float_var_decl_len_correct2() {
        let mut tok: Tokenizer = Tokenizer::new("double a = 4334.34ss;");
        let tokens: Vec<Token> = tok.start_scan();
        assert!(tokens.len() == 5);
    }

    #[test]
    fn test_char_ptr_var_decl_tokenization() {
        let mut tok: Tokenizer = Tokenizer::new("char* name = \"ram\";");
        let tokens: Vec<Token> = tok.start_scan();
        assert!(tokens.len() == 7);
        assert_eq!(tokens[0].kind, TokenKind::KW_CHAR);
        assert_eq!(tokens[1].kind, TokenKind::T_STAR);
        assert_eq!(tokens[2].kind, TokenKind::T_IDENTIFIER);
        assert_eq!(tokens[3].kind, TokenKind::T_EQUAL);
        assert_eq!(tokens[4].kind, TokenKind::T_STRING);
        assert_eq!(tokens[5].kind, TokenKind::T_SEMICOLON);
        assert_eq!(tokens[6].kind, TokenKind::T_EOF);
        assert_eq!(tokens[2].lexeme, "name"); // give identifier
        assert_eq!(tokens[4].lexeme, "ram"); // give string
    }

    #[test]
    fn test_func_decl_tokenization() {
        let mut tok: Tokenizer = Tokenizer::new("int main(void) { return 0; }");
        let tokens: Vec<Token> = tok.start_scan();
        assert!(tokens.len() == 11);
        assert_eq!(tokens[1].kind, TokenKind::T_IDENTIFIER);
        assert_eq!(tokens[1].lexeme, "main");
    }

    #[test]
    fn test_empty_source() {
        let mut tok: Tokenizer = Tokenizer::new("");
        let tokens: Vec<Token> = tok.start_scan();
        assert_eq!(tokens.len(), 1); // only EOF is present
        assert_eq!(tokens[0].kind, TokenKind::T_EOF); // only EOF is present
    }

    #[test]
    fn test_only_whitespace_source() {
        let mut tok: Tokenizer = Tokenizer::new("               ");
        let tokens: Vec<Token> = tok.start_scan();
        assert_eq!(tokens.len(), 1); // only EOF is present
        assert_eq!(tokens[0].kind, TokenKind::T_EOF); // only EOF is present
    }
}