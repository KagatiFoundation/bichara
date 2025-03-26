use std::{cell::RefCell, rc::Rc};

use kagc_ctx::CompilerCtx;
use kagc_symbol::StorageClass;
use kagc_target::{asm::aarch64::*, reg::*};

use crate::{ir_asm::ir_asm_gen::*, ir_instr::*, ir_types::*};

#[derive(Debug, Clone, Copy)]
struct ComptFnProps {
    pub is_leaf: bool,
    pub stack_size: usize
}

pub struct Aarch64IRToASM<'irgen> {
    pub ctx: Rc<RefCell<CompilerCtx<'irgen>>>,
    pub reg_manager: Rc<RefCell<Aarch64RegManager2>>,
    compt_fn_props: Option<ComptFnProps>
}

impl<'irgen> Aarch64IRToASM<'irgen> {
    #[allow(clippy::new_without_default)]
    pub fn new(ctx: Rc<RefCell<CompilerCtx<'irgen>>>, rm: Rc<RefCell<Aarch64RegManager2>>) -> Self {
        Self {
            ctx,
            reg_manager: rm,
            compt_fn_props: None
        }
    }

    fn switch_to_func_scope(&mut self, func_props: ComptFnProps) {
        self.compt_fn_props = Some(func_props);
    }

    fn switch_to_global_scope(&mut self) {
        self.compt_fn_props = None;
    }

    pub fn compute_stack_size_fn_ir(&self, func_ir: &IRFunc) -> Option<usize> {
        let mut stack_size: usize = func_ir.params.len() * 8; // each address is 8-bytes long

        if !func_ir.is_leaf {
            stack_size += 16;
        }

        for ir in &func_ir.body {
            if let IR::VarDecl(_) = ir {
                stack_size += 8;
            }
        }

        Some(Aarch64IRToASM::align_to_16(stack_size))
    }

    fn align_to_16(value: usize) -> usize {
        (value + 16 - 1) & !15
    }

    fn gen_fn_param_asm(&self, param: &IRLitType, stack_size: usize, is_leaf_fn: bool) -> String {
        match param {
            IRLitType::Reg(alloced_reg) => {
                let stack_off: usize = (alloced_reg.idx * 8) + 8;
                if is_leaf_fn {
                    format!("str x{}, [sp, #{}]", alloced_reg.idx, stack_size - stack_off)
                }
                else {
                    format!("str x{}, [x29, #-{}]", alloced_reg.idx, stack_off)
                }
            },
            _ => unimplemented!()
        }
    }
}

impl<'irgen> IRToASM for Aarch64IRToASM<'irgen> {
    fn gen_ir_fn_asm(&mut self, fn_ir: &IRFunc) -> String {
        let mut output_str: String = "".to_string();

        if fn_ir.class == StorageClass::EXTERN {
            output_str.push_str(&format!(".extern _{}\n", fn_ir.name));
            return  output_str;
        }

        let stack_size: usize = self.compute_stack_size_fn_ir(fn_ir).unwrap();

        if fn_ir.is_leaf {
            output_str.push_str(&format!("{}\n", self.gen_leaf_fn_prol(&fn_ir.name, stack_size)));
        }
        else {
            output_str.push_str(&format!("{}\n", self.gen_non_leaf_fn_prol(&fn_ir.name, stack_size)));
        }

        for param in &fn_ir.params {
            output_str.push_str(&format!("{}\n", self.gen_fn_param_asm(param, stack_size, fn_ir.is_leaf)));
        }

        // generate the code for function body
        self.switch_to_func_scope(
            ComptFnProps { 
                is_leaf: fn_ir.is_leaf, 
                stack_size
            }
        );

        for body_ir in &fn_ir.body {
            let body_asm: String = self.gen_asm_from_ir_node(body_ir);
            output_str.push_str(&format!("{}\n", &body_asm));
        }

        // revert back to global scope
        self.switch_to_global_scope();

        if fn_ir.is_leaf {
            output_str.push_str(&self.gen_leaf_fn_epl(stack_size));
        }
        else {
            output_str.push_str(&self.gen_non_leaf_fn_epl(stack_size));
        }

        output_str
    }
    
    fn gen_ir_local_var_decl_asm(&mut self, vdecl_ir: &IRVarDecl) -> String {
        let mut output_str: String = "".to_string();

        // since we are parsing a local variable, then compile-time function props is not None
        let fn_props: ComptFnProps = self.compt_fn_props.unwrap();
        
        let stack_off: usize = vdecl_ir.offset.unwrap_or_else(|| panic!("Local variables must have stack offset!"));

        if !fn_props.is_leaf {
            output_str.push_str(&format!("str x0, [x29, #-{}]", (stack_off * 8) + 8));
        }
        else {
            output_str.push_str(&format!("str x0, [sp, #{}]", fn_props.stack_size - ((stack_off * 8) + 8)));
        }

        output_str
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
    
    fn gen_asm_store(&mut self, idx: RegIdx, stack_off: usize) -> String {
        println!("spill {}", idx);
        "".to_string()
    }
    
    fn gen_asm_load(&mut self, idx: RegIdx, stack_off: usize) -> String {
        todo!()
    }
    
    fn gen_ir_mov_asm(&mut self, dest: &IRLitType, src: &IRLitType) -> String {
        format!("mov {}, {}", dest.into_str(), src.into_str())
    }
    
    fn gen_ir_add_asm(&mut self, dest: &IRLitType, op1: &IRLitType, op2: &IRLitType) -> String {
        format!("add {}, {}, {}", dest.into_str(), op1.into_str(), op2.into_str())
    }
}