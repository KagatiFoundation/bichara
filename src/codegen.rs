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

extern crate lazy_static;
use lazy_static::lazy_static;

use crate::parser::*;
use crate::enums::*;

lazy_static! {
    // all available registers
    static ref REGISTERS: Vec<&'static str> = vec!["r8", "r9", "r10", "r11"];
}

pub struct ASTTraverser {
    free_regs: Vec<i32>
}

impl ASTTraverser {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self { free_regs: vec![1, 1, 1, 1] }
    }

    pub fn generate_code(&mut self, ast: &mut ASTNode) {
        self.gen_preamble();
        let reg: usize = self.gen_from_ast(ast);
        self.gen_printint(reg);
        self.gen_postamble();
    }

    fn gen_from_ast(&mut self, ast: &ASTNode) -> usize {
        let mut leftreg: usize = 0;
        let mut rightreg: usize = 0;
        if ast.left.is_some() {
            leftreg = self.gen_from_ast(ast.left.as_ref().unwrap());
        }
        if ast.right.is_some() {
            rightreg = self.gen_from_ast(ast.right.as_ref().unwrap());
        }
        match ast.operation {
            ASTNodeKind::AST_ADD => self.gen_add(leftreg, rightreg),
            ASTNodeKind::AST_SUBTRACT => self.gen_sub(leftreg, rightreg),
            ASTNodeKind::AST_INTLIT => self.gen_load(&ast.value),
            _ => panic!("unknown AST operator..."),
        }
    }

    fn gen_add(&mut self, r1: usize, r2: usize) -> usize {
        println!("\tadd\t{}, {}\n", REGISTERS[r1], REGISTERS[r2]);
        self.free_reg(r2);
        r1
    }
    
    fn gen_sub(&mut self, r1: usize, r2: usize) -> usize {
        println!("\tsub\t{}, {}\n", REGISTERS[r1], REGISTERS[r2]);
        self.free_reg(r2);
        r1
    }

    fn gen_load(&mut self, value: &LitType) -> usize {
        let r: usize = self.alloc_reg();
        println!("\tmov\t{}, {:?}", REGISTERS[r], value);
        r
    }

    fn gen_store_global(&self, reg: usize, id: &str) -> usize {
        println!("\tmovq\t[{}], {}\n", id, REGISTERS[reg]);
        reg
    }

    pub fn gen_preamble(&self) {
        println!("\tglobal\tmain\n
        \textern\tprintf\n \
        \tsection\t.text\n \
        LC0:\tdb\t\"%d\",10,0\n \
        printint:\n \
        \tpush\trbp\n \
        \tmov\trbp, rsp\n \
        \tsub\trsp, 16\n \
        \tmov\t[rbp-4], edi\n \
        \tmov\teax, [rbp-4]\n \
        \tmov\tesi, eax\n \
        \tlea	rdi, [rel LC0]\n \
        \tmov	eax, 0\n \
        \tcall	printf\n \
        \tnop\n \
        \tleave\n \
        \tret\n \
        \n \
        main:\n \
        \tpush\trbp\n \
        \tmov	rbp, rsp\n");
    }

    pub fn gen_postamble(&self) {
        println!("\tmov	eax, 0\n \
        \tpop	rbp\n \
        \tret\n");
    }
    
    fn gen_printint(&mut self, reg: usize) {
        println!("\tmov\trdi, {}\n", REGISTERS[reg]);
        println!("\tcall\tprintint\n");
        self.free_reg(reg);
    }
    
    fn alloc_reg(&mut self) -> usize {
        for (idx, i) in self.free_regs.iter().enumerate() {
            if *i == 1 { 
                self.free_regs[idx] = 0;
                return idx; 
            }
        }
        panic!("out of registers");
    }

    fn free_reg(&mut self, pos: usize) {
        if self.free_regs[pos] != 0 {
            panic!("error trying to free the register");
        }
        self.free_regs[pos] = 1;
    }
}