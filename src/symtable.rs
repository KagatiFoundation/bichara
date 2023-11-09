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

pub struct Symtable {
    syms: Vec<String>, // tracks all the global symbols
    counter: usize, // next free slot in the table
}

// Maximum number of symbols in program
pub const NSYMBOLS: usize = 1024;

impl Symtable {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self { Self { syms: vec![], counter: 0 } }

    pub fn find(&self, name: &str) -> usize {
        for (idx, n) in self.syms.iter().enumerate() {
            if name == n { return idx; }
        }
        0xFFFFFFFF
    }

    fn next(&mut self) -> usize {
        self.counter += 1;
        if self.counter >= NSYMBOLS {
            panic!("Too many global symbols");
        }
        self.counter
    }

    pub fn add(&mut self, name: &str) -> usize {
        let mut pos: usize = self.find(name);
        if pos != 0xFFFFFFFF {
            return pos;
        }
        pos = self.next();
        self.syms.push(String::from(name));
        pos
    }
}