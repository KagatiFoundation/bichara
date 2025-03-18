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

use kagc_token::Token;

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum ParseError {
    UnexpectedToken {
        expected: Token,
        found: Token
    }, // unexpected token was encountered
    SymbolNotFound(Token),  // symbol not found; symbol has not been defined before
    NotCallable(Token),     // if a token being called is not callable type
    GlobalInsideFunction(Token),
    UnsubscritableToken(Token),
    /// Placeholder indicating a condition where no AST node is needed, e.g., variable declaration without assignment
    None
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::NotCallable(token) => {
                kagc_errors::report_errornous_token_and_exit(token, &format!("'{}' is not callable", token.lexeme), 1);
                Ok(())
            }
            ParseError::GlobalInsideFunction(token) => {
                kagc_errors::report_errornous_token_and_exit(token, "global variable declaration inside a function", 1);
                Ok(())
            }
            ParseError::UnsubscritableToken(token) => {
                kagc_errors::report_errornous_token_and_exit(token, &format!("'{}' is not subscriptable", token.lexeme), 1);
                Ok(())
            }
            ParseError::SymbolNotFound(token) => {
                kagc_errors::report_errornous_token_and_exit(token, &format!("Undefined symbol: '{}'", token.lexeme), 1);
                Ok(())
            }
            _ => write!(f, "other"),
        }
    }
}