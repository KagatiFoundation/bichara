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

use std::{collections::HashMap, str::FromStr};

extern crate lazy_static;
use lazy_static::lazy_static;

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
    keywords: HashMap<String, TokenKind>,
    curr_char: char, // current char
    read_curr: usize, // position from the start
    col_counter: usize, // column counter
}

impl Tokenizer {
    pub fn new(source: String) -> Tokenizer {
        let mut keywords: HashMap<String, TokenKind> = HashMap::new();
        Tokenizer {
            line: 1, 
            source,
            keywords,
            curr_char: ' ', // space 
            read_curr: 0,
            col_counter: 1
        }
    }
}

impl Tokenizer {
    pub fn start_scan(&mut self) -> Vec<Token> {
        let mut result: Vec<Token> = Vec::new();
        let source_len: usize = self.source.len();
        self.advance_to_next_char();
        loop {
            let mut token: Token = self.get_token();
            if token.lexeme.is_empty() {
                token.lexeme = String::from(token.kind.as_str());
            }
            if token.kind == TokenKind::T_EOF {
                result.push(token);
                break;
            }
            if token.kind != TokenKind::T_NONE {
                result.push(token);
            } 
        }
        result
    }

    fn get_token(&mut self) -> Token {
        let mut token: Token = Token::new(TokenKind::T_NONE, String::from(""), TokenPos{line: 0, column: 0});
        let col: usize = self.col_counter - 1;
        let line: usize = self.line;
        match self.curr_char {
            '+' => {
                token.kind = TokenKind::T_PLUS;
                self.advance_to_next_char();
                match self.curr_char {
                    '+' => {
                        token.kind = TokenKind::T_INCR;
                        self.advance_to_next_char();
                    },
                    '=' => {
                        token.kind = TokenKind::T_PLUSEQ;
                        self.advance_to_next_char();
                    }
                    _ => {}
                }
            },
            '-' => {
                token.kind = TokenKind::T_MINUS;
                self.advance_to_next_char();
                match self.curr_char {
                    '-' => {
                        token.kind = TokenKind::T_DECR;
                        self.advance_to_next_char();
                    },
                    '=' => {
                        token.kind = TokenKind::T_MINUSEQ;
                        self.advance_to_next_char();
                    }
                    '>' => {
                        token.kind = TokenKind::T_ARROW;
                        self.advance_to_next_char();
                    }
                    _ => {}
                }
            },
            '*' => {
                token.kind = TokenKind::T_STAR;
                self.advance_to_next_char();
                if self.curr_char == '=' {
                    token.kind = TokenKind::T_STAREQ;
                    self.advance_to_next_char();
                }
            },
            '/' => {
                token.kind = TokenKind::T_SLASH;
                self.advance_to_next_char();
                if self.curr_char == '=' {
                    token.kind = TokenKind::T_SLASHEQ;
                    self.advance_to_next_char();
                }
            },
            '!' => {
                token.kind = TokenKind::T_BANG;
                self.advance_to_next_char();
                if self.curr_char == '=' {
                    token.kind = TokenKind::T_NEQ;
                    self.advance_to_next_char();
                }
            },
            '%' => {
                token.kind = TokenKind::T_PERCENT;
                self.advance_to_next_char();
                if self.curr_char == '=' {
                    token.kind = TokenKind::T_PERCENTEQ;
                    self.advance_to_next_char();
                }
            },
            '^' => {
                token.kind = TokenKind::T_CARET;
                self.advance_to_next_char();
                if self.curr_char == '=' {
                    token.kind = TokenKind::T_CARETEQ;
                    self.advance_to_next_char();
                }
            },
            '&' => {
                token.kind = TokenKind::T_AMPERSAND;
                self.advance_to_next_char();
                if self.curr_char == '=' {
                    token.kind = TokenKind::T_AMPERSANDEQ;
                    self.advance_to_next_char();
                }
            },
            '>' => {
                token.kind = TokenKind::T_GTHAN;
                self.advance_to_next_char();
                match self.curr_char {
                    '>' => {
                        token.kind = TokenKind::T_RSHIFT;
                        self.advance_to_next_char();
                        if self.curr_char == '=' {
                            self.advance_to_next_char();
                            token.kind = TokenKind::T_RSHIFTEQ;
                        }
                    },
                    '=' => token.kind = TokenKind::T_GTEQ,
                    _ => {},
                }
            },
            '<' => {
                token.kind = TokenKind::T_LTHAN;
                self.advance_to_next_char();
                match self.curr_char {
                    '<' => {
                        token.kind = TokenKind::T_LSHIFT;
                        self.advance_to_next_char();
                        if self.curr_char == '=' {
                            self.advance_to_next_char();
                            token.kind = TokenKind::T_LSHIFTEQ;
                        }
                    }
                    '=' => token.kind = TokenKind::T_LTEQ,
                    _ => {}
                }
            },
            '|' => {
                token.kind = TokenKind::T_PIPE;
                self.advance_to_next_char();
                match self.curr_char {
                    '|' => {
                        token.kind = TokenKind::T_OR;
                        self.advance_to_next_char();
                    },
                    '=' => {
                        self.advance_to_next_char();
                        token.kind = TokenKind::T_PIPEEQ;
                    },
                    _ => {}
                }
            },
            '&' => {
                token.kind = TokenKind::T_AMPERSAND;
                self.advance_to_next_char();
                match self.curr_char {
                    '&' => {
                        token.kind = TokenKind::T_AND;
                        self.advance_to_next_char();
                    },
                    '=' => {
                        self.advance_to_next_char();
                        token.kind = TokenKind::T_AMPERSANDEQ;
                    },
                    _ => {}
                }
            },
            '~' => {
                token.kind = TokenKind::L_TILDE;
                self.advance_to_next_char();
                if self.curr_char == '=' {
                    self.advance_to_next_char();
                    token.kind = TokenKind::T_TILDEEQ;
                }
            },
            '=' => {
                token.kind = TokenKind::T_EQUAL;
                self.advance_to_next_char();
                if self.curr_char == '=' {
                    self.advance_to_next_char();
                    token.kind = TokenKind::T_EQEQ;
                }
            },
            '0'..='9' => {
                let __start: usize = self.read_curr - 1;
                let mut __end: usize = __start;
                while self.curr_char.is_ascii_digit() {
                    self.advance_to_next_char();
                    __end += 1;
                }
                let number: &str = &self.source[__start..__end];
                token.kind = TokenKind::T_INT_NUM;
                token.lexeme = String::from(number);
            }, 
            '_' | 'a'..='z' | 'A'..='Z' => {
                let __start: usize = self.read_curr - 1;
                let mut __end: usize = __start;
                while self.curr_char.is_alphanumeric() || self.curr_char == '_' {
                    self.advance_to_next_char();
                    __end += 1;
                }
                token.kind = TokenKind::T_IDENTIFIER;
                let name: &str = &self.source[__start..__end];
                let keyword: Option<&TokenKind> = KEYWORDS.get(name);
                if let Some(key) = keyword {
                    token.kind = *key;
                } 
                token.lexeme = String::from(name);
            },
            '"' => {
                self.advance_to_next_char(); // skip '"'
                let __start: usize = self.read_curr - 1;
                let mut __end: usize = __start;
                while self.curr_char != '"' && !self.is_at_end() {
                    self.advance_to_next_char();
                    __end += 1;
                }
                if self.is_at_end() {
                    println!("Unterminated string error...");
                } else {
                    self.advance_to_next_char();
                    let str_val: &str = &self.source[__start..__end];
                    token.kind = TokenKind::T_STRING;
                    token.lexeme = String::from(str_val);
                }
            },
            '(' | ')' | '{' | '}' | '[' | ']' | '#' | '.' | '?' | ':' | ',' | ';' => {
                token.kind = TokenKind::from_str(self.curr_char.to_string().as_str()).unwrap();
                self.advance_to_next_char();
            },
            ' ' | '\n' | '\t' => self.advance_to_next_char(),
            '\0' => token.kind = TokenKind::T_EOF,
            _ => {}
        }
        token.pos = TokenPos{ line, column: col };
        token
    }

    #[inline]
    fn is_at_end(&self) -> bool {
        self.read_curr >= self.source.len()
    }

    #[inline]
    fn advance_to_next_char(&mut self) {
        if self.read_curr < self.source.len() {
            self.curr_char = self.source.as_bytes()[self.read_curr] as char;
            if self.curr_char == '\n' {
                self.line += 1;
                self.col_counter = 0;
            }
            self.read_curr += 1;
            self.col_counter += 1;
        } else {
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
        let mut tok: Tokenizer = Tokenizer::new(String::from("int a = 4;"));
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
    fn test_int_var_decl_int_len_correct() {
        let mut tok: Tokenizer = Tokenizer::new(String::from("int a = 433434;"));
        let tokens: Vec<Token> = tok.start_scan();
        assert!(tokens.len() == 6);
        assert_eq!(tokens[3].lexeme.len(), 6);
    }

    #[test]
    fn test_char_ptr_var_decl_tokenization() {
        let mut tok: Tokenizer = Tokenizer::new(String::from("char* name = \"ram\";"));
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
        let mut tok: Tokenizer = Tokenizer::new(String::from("int main(void) { return 0; }"));
        let tokens: Vec<Token> = tok.start_scan();
        assert!(tokens.len() == 11);
        assert_eq!(tokens[1].kind, TokenKind::T_IDENTIFIER);
        assert_eq!(tokens[1].lexeme, "main");
    }

    #[test]
    fn test_empty_source() {
        let mut tok: Tokenizer = Tokenizer::new(String::from(""));
        let tokens: Vec<Token> = tok.start_scan();
        assert_eq!(tokens.len(), 1); // only EOF is present
        assert_eq!(tokens[0].kind, TokenKind::T_EOF); // only EOF is present
    }

    #[test]
    fn test_only_whitespace_source() {
        let mut tok: Tokenizer = Tokenizer::new(String::from("               "));
        let tokens: Vec<Token> = tok.start_scan();
        assert_eq!(tokens.len(), 1); // only EOF is present
        assert_eq!(tokens[0].kind, TokenKind::T_EOF); // only EOF is present
    }
}