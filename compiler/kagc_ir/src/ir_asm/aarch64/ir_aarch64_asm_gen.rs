use std::{cell::RefCell, rc::Rc};

use kagc_ctx::CompilerCtx;
use kagc_symbol::{FunctionInfo, StorageClass};
use kagc_target::reg::RegIdx;

use crate::{ir_asm::ir_asm_gen::*, ir_instr::*, ir_types::*};

pub struct Aarch64IRToASM<'irgen> {
    pub ctx: Rc<RefCell<CompilerCtx<'irgen>>>
}

impl<'irgen> Aarch64IRToASM<'irgen> {
    #[allow(clippy::new_without_default)]
    pub fn new(ctx: Rc<RefCell<CompilerCtx<'irgen>>>) -> Self {
        Self {
            ctx
        }
    }

    fn compute_stack_size(&self, func_name: &str, is_leaf: bool) -> usize {
        let ctx_borrow = self.ctx.borrow();

        let func_info: &FunctionInfo = ctx_borrow.func_table.get(func_name)
                .unwrap_or_else(|| panic!("Function '{}' not found in function table", func_name));

        let mut stack_size: usize = func_info.local_syms.count() * 16;

        if !is_leaf {
            stack_size += 2 * 16 * 2;
        }

        Aarch64IRToASM::align_to_16(stack_size)
    }

    fn align_to_16(value: usize) -> usize {
        (value + 16 - 1) & !15
    }
}

impl<'irgen> IRToASM for Aarch64IRToASM<'irgen> {
    fn gen_ir_fn_asm(&mut self, fn_ir: &IRFunc, reg: usize) -> String {
        let mut output_str: String = "".to_string();

        if fn_ir.class == StorageClass::EXTERN {
            output_str.push_str(&format!(".extern _{}\n", fn_ir.name));
            return  output_str;
        }

        let stack_size: usize = self.compute_stack_size(&fn_ir.name, fn_ir.is_leaf);

        if fn_ir.is_leaf {
            output_str.push_str(&format!("{}\n", self.gen_leaf_fn_prol(&fn_ir.name, stack_size)));
        }

        for body_ir in &fn_ir.body {
            let body_asm: String = self.gen_asm_from_ir_node(body_ir, reg);
            output_str.push_str(&format!("{}\n", &body_asm));
        }

        if fn_ir.is_leaf {
            output_str.push_str(&self.gen_leaf_fn_epl(stack_size));
        }

        output_str
    }
    
    fn gen_ir_local_var_decl_asm(&mut self, vdecl_ir: &IRVarDecl, reg: usize) -> String {
        let mut output_str: String = "".to_string();

        let (reg, code): (usize, String) = self.gen_ir_instr(&vdecl_ir.value, reg);

        output_str.push_str(&format!("{code}\n"));

        let stack_off: usize = vdecl_ir.offset.unwrap_or_else(|| panic!("Local variables must have stack offset!"));
        
        output_str.push_str(&format!("str x{reg}, [x29, #-{}]", (stack_off * 8) + 8));

        output_str
    }

    fn gen_ir_instr(&mut self, instr: &IRInstr, reg: usize) -> (RegIdx, String) {
        match instr {
            IRInstr::Mov(reg, value) => {
                let idx = if let IRLitType::Reg(reg_idx) = reg {
                    *reg_idx
                } 
                else {
                    panic!("MOV only accepts register index as the first literal type!")
                };
                
                (idx, format!("mov x{}, {}", idx, value.into_str()))
            },
            IRInstr::Add(irlit_type, irlit_type1, irlit_type2) => todo!(),
            IRInstr::Call(_, irlit_types, irlit_type) => todo!(),
        }
    }
    
    fn gen_leaf_fn_prol(&self, fn_label: &str, stack_size: usize) -> String {
        let mut output_str: String = "".to_string();
        output_str.push_str(&format!("\n.global _{fn_label}\n_{fn_label}:\n"));

        if stack_size != 0 {
            output_str.push_str(&format!("sub sp, sp, #{stack_size}"));
        }
        output_str
    }
    
    fn gen_leaf_fn_epl(&self, stack_size: usize) -> String {
        format!("add sp, sp, #{stack_size}\nret")
    }

    fn gen_non_leaf_fn_prol(&self, fn_label: &str, stack_size: usize) -> String {
        todo!()
    }
    
    fn gen_non_leaf_fn_epl(&self, stack_size: usize) -> String {
        todo!()
    }
}