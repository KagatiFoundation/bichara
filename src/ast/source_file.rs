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

use std::{fs::File, io::{Error, Read}, path::Path};

use crate::tokenizer::Token;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ParsingFlag {
    CollectTokens = 1 // whether to save parsed tokens
}

pub enum ParsingStageError {
    FileNotFound,
    TokenizationError,
    ParsingError,
    FileReadingError,
    None
}

pub enum ParsingStage {
    Unprocessed, // nothing has been done till now
    Loaded, // file is loaded into memory
    Processed, // tokens has been generated
    Error(ParsingStageError), // some kind of error has occured
}

pub struct SourceFile {
    pub path: String,
    pub parsing_options: u8,
    pub tokens: Vec<Token>,
    pub stage: ParsingStage,
    pub source: String
}

impl SourceFile {
    pub fn new(path: String, parsing_opts: u8) -> Self {
        let file_path: &Path = Path::new(&path);
        let mut stage: ParsingStage = ParsingStage::Unprocessed;
        if !file_path.exists() {
            println!("File not found: {:?}", file_path);
            stage = ParsingStage::Error(ParsingStageError::FileNotFound)
        }
        if !file_path.is_file() {
            println!("{} is not a file!", path);
        }
        Self {
            path,
            parsing_options: parsing_opts,
            tokens: vec![],
            stage,
            source: String::from(""),
        }
    }

    pub fn read(&mut self) -> Result<i32, Error> {
        let mut file_res: Result<File, std::io::Error> = File::open(Path::new(&self.path));
        let mut file_size: i32 = -1;
        if let Ok(ref mut file) = file_res {
            let file_read_res: Result<usize, std::io::Error> = file.read_to_string(&mut self.source);
            if let Ok(fs) = file_read_res {
                file_size = fs as i32;
            } else {
                self.stage = ParsingStage::Error(ParsingStageError::FileReadingError);
                return Err(file_read_res.err().unwrap());
            }
        }
        if self.is_collect_tokens() {
            self.tokenize();
        }
        Ok(file_size)
    }

    pub fn is_collect_tokens(&self) -> bool {
        (self.parsing_options & 0x1) == 0x1
    }

    fn tokenize(&mut self) {
    }
}