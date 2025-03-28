use std::{cell::RefCell, collections::HashMap, hash::Hash, rc::Rc};

use kagc_ctx::CompilerCtx;
use kagc_symbol::StorageClass;
use kagc_target::{asm::aarch64::*, reg::*};

use crate::{
    ir_asm::ir_asm_gen::*, 
    ir_instr::*, 
    ir_liveness::*, 
    ir_types::*
};

#[derive(Debug)]
struct TempRegMap<KT: Eq + Hash> {
    pub reg_map: HashMap<KT, AllocedReg>
}

impl<KT: Eq + Hash> Default for TempRegMap<KT> {
    fn default() -> Self {
        Self {
            reg_map: HashMap::new()
        }
    }
}

impl<KT: Eq + Hash> TempRegMap<KT> {
    pub fn drop(&mut self, key: &KT) -> Option<AllocedReg> {
        self.reg_map.remove(key)
    }
}


/// Represents properties of a compiled function.
/// 
/// - `is_leaf`: Indicates whether the function is a leaf function 
///             (i.e., it makes no function calls).
/// - `stack_size`: The amount of stack space allocated for this function.
#[derive(Debug, Clone)]
struct ComptFnProps {
    pub is_leaf: bool,
    pub stack_size: usize,
    pub liveness_info: HashMap<usize, LiveRange>
}

/// Handles the translation of IR (Intermediate Representation) to 
/// AArch64 assembly.
///
/// - `ctx`: A reference-counted, mutable context for the compiler, 
///         which manages global state.
/// 
/// - `reg_manager`: A reference-counted, mutable register manager for 
///                 AArch64, handling register allocation.
/// 
/// - `compt_fn_props`: Optional function properties, used for tracking 
///                     whether a function is a leaf and its stack size.
pub struct Aarch64IRToASM<'asmgen> {
    pub ctx: Rc<RefCell<CompilerCtx<'asmgen>>>,

    pub reg_manager: Rc<RefCell<Aarch64RegManager2>>,

    compt_fn_props: Option<ComptFnProps>,

    temp_reg_map: TempRegMap<usize>,

    /// Instruction Pointer:
    /// IP is used to track the execution of instructions inside 
    /// a function.
    pub ip: usize
}

impl<'asmgen> Aarch64IRToASM<'asmgen> {
    #[allow(clippy::new_without_default)]
    pub fn new(ctx: Rc<RefCell<CompilerCtx<'asmgen>>>, rm: Rc<RefCell<Aarch64RegManager2>>) -> Self {
        Self {
            ctx,
            reg_manager: rm,
            compt_fn_props: None,
            temp_reg_map: TempRegMap { reg_map: HashMap::new() },
            ip: 0
        }
    }

    pub fn gen_asm(&mut self, irs: &mut [IR]) -> Vec<String> {
        let mut output: Vec<String> = vec![];
        for ir in irs {
            output.push(self.gen_asm_from_ir_node(ir));
        }
        output
    }

    /// Sets function-specific properties when entering a function scope.
    fn switch_to_func_scope(&mut self, func_props: ComptFnProps) {
        self.compt_fn_props = Some(func_props);
    }
    
    /// Clears function-specific properties when returning to the global scope.
    fn switch_to_global_scope(&mut self) {
        self.compt_fn_props = None;
    }
    
    /// Computes the aligned stack size required for a function.
    fn compute_stack_size_fn_ir(&self, func_ir: &IRFunc) -> Option<usize> {
        let mut stack_size: usize = func_ir.params.len() * 8; // Each parameter is 8 bytes
    
        if !func_ir.is_leaf {
            stack_size += 16; // Space for x29 and x30
        }
    
        for ir in &func_ir.body {
            if let IR::VarDecl(_) = ir {
                stack_size += 8; // Each variable takes 8 bytes
            }
        }
    
        Some(Aarch64IRToASM::align_to_16(stack_size))
    }
    

    /// Align the given address into an address divisible by 16.
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

    #[inline]
    fn advance_ip(&mut self) {
        self.ip += 1;
    }

    #[inline]
    fn reset(&mut self) {
        // reset IP before moving into into another function
        self.ip = 0;

        // stay on the global scope; no function available
        self.switch_to_global_scope();
    }

    fn drop_temp(&mut self, temp: usize) {
        let freed_reg: Option<AllocedReg> = self.temp_reg_map.drop(&temp);

        if let Some(reg) = freed_reg {
            self.reg_manager.borrow_mut().free_register(reg.idx);
        }
        else {
            panic!("Problem while freeing temporary!");
        }
    }

    fn try_dropping_temp(&mut self, temp: usize) {
        if let Some(compt_fn_info) = &self.compt_fn_props {
            if let Some((start, end)) = compt_fn_info.liveness_info.get(&temp) {
                if (*start + *end) == self.ip {
                    self.drop_temp(temp);
                }
            }
        }
        else {
            panic!("Compile time information not avaailable for function!");
        }
    }
}

impl<'irgen> IRToASM for Aarch64IRToASM<'irgen> {
    fn gen_ir_fn_asm(&mut self, fn_ir: &IRFunc) -> String {
        let mut output_str: String = "".to_string();

        // Temporary liveness information of this function
        let temp_liveness: HashMap<usize, LiveRange> = LivenessAnalyzer::analyze_fn_temps(fn_ir);

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
                stack_size,
                liveness_info: temp_liveness
            }
        );

        for body_ir in &fn_ir.body {
            let body_asm: String = self.gen_asm_from_ir_node(body_ir);

            // maybe I should increment the IP here???
            self.advance_ip();

            output_str.push_str(&format!("{}\n", &body_asm));
        }

        // reset IP and scope before moving into another function
        self.reset();

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

        let stack_off: usize = vdecl_ir.offset.unwrap_or_else(|| panic!("Local variables must have stack offset!"));

        let value_reg: AllocedReg = match &vdecl_ir.value {
            IRLitType::Temp(temp_value) => {
                let temp_reg: AllocedReg = self.temp_reg_map.reg_map.get(temp_value).unwrap().clone();
                
                self.try_dropping_temp(*temp_value);

                temp_reg
            },
            IRLitType::Reg(reg) => reg.clone(),
            _ => todo!()
        };

        // since we are parsing a local variable, then compile-time function props is not None
        let fn_props: &ComptFnProps = self.compt_fn_props.as_ref().unwrap_or_else(|| panic!("Compile time information not available for the function!"));
        
        if !fn_props.is_leaf {
            output_str.push_str(&format!("str {}, [x29, #-{}]", value_reg.name(), (stack_off * 8) + 8));
        }
        else {
            output_str.push_str(&format!("str {}, [sp, #{}]", value_reg.name(), fn_props.stack_size - ((stack_off * 8) + 8)));
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
        if stack_size != 0 {
            format!("add sp, sp, #{stack_size}\nret")
        }
        else {
            "ret".to_string()
        }
    }

    fn gen_non_leaf_fn_prol(&self, fn_label: &str, stack_size: usize) -> String {
        let mut output_str: String = "".to_string();
        
        output_str.push_str(&format!("\n.global _{fn_label}\n_{fn_label}:"));
        output_str.push_str(&format!("sub sp, sp, #{stack_size}"));
        output_str.push_str(&format!("stp x29, x30, [sp, #{}]", stack_size - 16));
        output_str.push_str(&format!("add x29, sp, #{}", stack_size - 16));

        output_str
    }
    
    fn gen_non_leaf_fn_epl(&self, stack_size: usize) -> String {
        let mut output_str: String = "".to_string();

        output_str.push_str(&format!("ldp x29, x30, [sp, #{}]", stack_size - 16));
        output_str.push_str(&format!("add sp, sp, #{}\nret\n", stack_size));

        output_str
    }
    
    fn gen_asm_load(&mut self, dest: &IRLitType, stack_off: usize) -> String {
        let dest_reg: AllocedReg = self.resolve_register(dest);
        
        let (stack_size, is_leaf_fn) = if let Some(func_props) = &self.compt_fn_props {
            (func_props.stack_size, func_props.is_leaf)
        }
        else {
            panic!("Trying to load value from the stack outside of a function!");
        };

        let soff: usize = (stack_off * 8) + 8;
        
        if is_leaf_fn {
            format!("LDR {}, [sp, #{}]", dest_reg.name(), stack_size - soff)
        }
        else {
            format!("LDR {}, [x29, #-{}]", dest_reg.name(), soff)
        }
    }
    
    fn gen_ir_mov_asm(&mut self, dest: &IRLitType, src: &IRLitType) -> String {
        let dest_reg: AllocedReg = self.resolve_register(dest);
        format!("MOV {}, {}", dest_reg.name(), self.extract_operand(src))
    }
    
    fn gen_ir_add_asm(&mut self, dest: &IRLitType, op1: &IRLitType, op2: &IRLitType) -> String {
        let dest_reg: AllocedReg = self.resolve_register(dest);
        format!(
            "ADD {}, {}, {}",
            dest_reg.name(), 
            self.extract_operand(op1), 
            self.extract_operand(op2)
        )
    }
}

impl<'asmgen> Aarch64IRToASM<'asmgen> {
    /// Extract the IRLitType as an operand(String)
    fn extract_operand(&mut self, irlit: &IRLitType) -> String {
        match irlit {
            IRLitType::Const(irlit_val) => {
                match irlit_val {
                    IRLitVal::Int32(value) => value.to_string(),
                    _ => todo!()
                }
            },
            
            IRLitType::Reg(src_reg) => src_reg.name(),
            
            IRLitType::Temp(temp_value) => {
                let src_reg: AllocedReg = self.temp_reg_map.reg_map.get(temp_value).unwrap().clone();
                self.try_dropping_temp(*temp_value);
                src_reg.name()
            },

            IRLitType::Var(var) => {
                
                var.name.clone()
            }
        }
    }

    /// Get the compile time register mapping of a IR literal type
    fn resolve_register(&mut self, irlit: &IRLitType) -> AllocedReg {
        match irlit {
            IRLitType::Temp(temp_value) => {
                let reg: AllocedReg = if self.temp_reg_map.reg_map.contains_key(temp_value) {
                    self.temp_reg_map.reg_map.get(temp_value).unwrap().clone()
                }
                else {
                    let mut reg_mgr = self.reg_manager.borrow_mut();
                    let alloced_reg: AllocedReg = reg_mgr.allocate_register(64);

                    self.temp_reg_map.reg_map.insert(*temp_value, alloced_reg.clone());
                    
                    alloced_reg
                };

                self.try_dropping_temp(*temp_value);
                
                reg
            },

            IRLitType::Reg(reg) => reg.clone(),
            
            _ => todo!()
        }
    }
}