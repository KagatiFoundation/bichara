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

use kagc_symbol::StorageClass;
use kagc_target::reg::{RegIdx, NO_REG};

use crate::ir_instr::*;

pub trait IRToASM { 
    fn gen_asm(&mut self, irs: &Vec<IR>) -> Vec<String> {
        let mut output: Vec<String> = vec![];
        for ir in irs {
            output.push(self.gen_asm_from_ir_node(ir, NO_REG));
        }
        output
    }

    fn gen_asm_from_ir_node(&mut self, ir: &IR, reg: usize) -> String {
        match ir {
            IR::Func(irfunc) => {
                let fn_asm: String = self.gen_ir_fn_asm(irfunc, reg);
                fn_asm
            },
            IR::VarDecl(irassign) => {
                if irassign.class == StorageClass::LOCAL {
                    let assign_asm: String = self.gen_ir_local_var_decl_asm(irassign, reg);
                    assign_asm
                } 
                else {
                    "".to_string()
                }
            }
        }
    }

    fn gen_ir_fn_asm(&mut self, fn_ir: &IRFunc, reg: usize) -> String;
    
    fn gen_ir_local_var_decl_asm(&mut self, vdecl_ir: &IRVarDecl, reg: usize) -> String;

    fn gen_ir_instr(&mut self, instr: &IRInstr, reg: usize) -> (RegIdx, String);

    fn gen_leaf_fn_prol(&self, fn_label: &str, stack_size: usize) -> String;

    fn gen_non_leaf_fn_prol(&self, fn_label: &str, stack_size: usize) -> String;

    fn gen_leaf_fn_epl(&self, stack_size: usize) -> String;

    fn gen_non_leaf_fn_epl(&self, stack_size: usize) -> String;
}