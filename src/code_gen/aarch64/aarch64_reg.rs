use std::collections::HashMap;

use crate::{
    code_gen::{codegen::EARLY_RETURN, reg::{
        AllocedReg, 
        RegAllocError, 
        RegAllocResult, 
        RegIdx, 
        RegManager
    }}, 
    types::LitTypeVariant
};

pub const REG_64BIT: usize = 64;
pub const REG_32BIT: usize = 32;

const REG_64BIT_COUNT: usize = 28;
const REG_32BIT_COUNT: usize = 28;

const CALLER_SAVED_REG_COUNT: usize = 4;

impl AllocedReg {
    pub fn lit_type(&self) -> LitTypeVariant {
        match self.size {
            REG_32BIT => LitTypeVariant::I32,
            REG_64BIT => LitTypeVariant::I64,
            _ => LitTypeVariant::None
        }
    }

    pub fn early_return() -> Self {
        AllocedReg {
            idx: EARLY_RETURN,
            size: 0
        }
    }
}

pub struct Aarch64RegManager {
    /// 64-bit registers
    regs64: HashMap<String, u8>,
}

impl RegManager for Aarch64RegManager {
    fn allocate(&mut self, val_type: &LitTypeVariant) -> RegAllocResult {
        match val_type {
            LitTypeVariant::U8
            | LitTypeVariant::I16
            | LitTypeVariant::I32 => self.alllocate_32bit_reg(),
            LitTypeVariant::I64 => self.alllocate_64bit_reg(),
            _ => Err(RegAllocError)
        }
    }
    
    fn deallocate(&mut self, idx: RegIdx, val_type: &LitTypeVariant) {
        self.deallocate_64bit_reg(idx);
    }
    
    fn deallocate_all(&mut self) {
        self.deallocate_all_regs();
    }
    
    fn allocate_param_reg(&mut self, val_type: &LitTypeVariant) -> RegAllocResult {
        match val_type {
            LitTypeVariant::U8
            | LitTypeVariant::I16
            | LitTypeVariant::I32 => self.alllocate_32bit_param_reg(),
            LitTypeVariant::I64 => self.alllocate_64bit_param_reg(),
            _ => Err(RegAllocError)
        }
    }
    
    fn deallocate_param_reg(&mut self, idx: RegIdx) {
        self.deallocate_64bit_param_reg(idx)
    }
    
    fn name(&self, idx: RegIdx, val_type: &LitTypeVariant) -> String {
        match val_type  {
            LitTypeVariant::U8
            | LitTypeVariant::I16
            | LitTypeVariant::I32 => {
                if idx >= REG_32BIT_COUNT {
                    panic!("Not a valid 32-bit register");
                }
                format!("w{}", idx)
            },
            LitTypeVariant::I64 => {
                if idx >= REG_64BIT_COUNT {
                    panic!("Not a valid 64-bit register");
                }
                format!("x{}", idx)
            },
            _ => panic!("Invalid data type for 'name'")
        }
    }
}

impl Aarch64RegManager {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let mut regs64: HashMap<String, u8> = HashMap::new();

        for i in 0..=REG_64BIT_COUNT {
            regs64.insert(format!("x{}", i), 1);
        }

        Self {
            regs64
        }
    }

    pub fn alloc_reg(&mut self, idx: RegIdx) -> RegAllocResult {
        for i in 0..=REG_64BIT_COUNT {
            if i == idx {
                let reg_name: String = format!("x{}", i);
                if let Some(free_reg) = self.regs64.get(reg_name.clone().as_str()) {
                    if *free_reg == 1 {
                        self.regs64.insert(format!("x{i}"), 0);
                        return Ok(AllocedReg {
                            idx: i,
                            size: REG_32BIT
                        });
                    }
                }
            }
        }
        Err(RegAllocError) 
    }

    fn alllocate_32bit_reg(&mut self) -> RegAllocResult {
        for i in 0..=REG_64BIT_COUNT {
            let reg_name: String = format!("x{}", i);
            if let Some(free_reg) = self.regs64.get(reg_name.clone().as_str()) {
                if *free_reg == 1 {
                    self.regs64.insert(format!("x{i}"), 0);
                    return Ok(AllocedReg {
                        idx: i,
                        size: REG_32BIT
                    });
                }
            }
        }
        Err(RegAllocError)
    }

    fn alllocate_64bit_reg(&mut self) -> RegAllocResult {
        for i in 0..=REG_64BIT_COUNT {
            let reg_name: String = format!("x{}", i);
            if let Some(free_reg) = self.regs64.get(reg_name.clone().as_str()) {
                if *free_reg == 1 {
                    self.regs64.insert(format!("x{i}"), 0);
                    return Ok(AllocedReg {
                        idx: i,
                        size: REG_64BIT
                    });
                }
            }
        }
        Err(RegAllocError)
    }

    pub fn alloc_param_reg(&mut self, idx: RegIdx) -> RegAllocResult {
        if idx > 3 {
            panic!("index is greater than 3!");
        }
        for i in 0..=CALLER_SAVED_REG_COUNT {
            if i == idx {
                let reg_name: String = format!("x{}", i);
                if let Some(free_reg) = self.regs64.get(reg_name.clone().as_str()) {
                    if *free_reg == 1 {
                        self.regs64.insert(format!("x{i}"), 0);
                        return Ok(AllocedReg {
                            idx: i,
                            size: REG_32BIT
                        });
                    }
                }
            }
        }
        Err(RegAllocError) 
    }

    fn alllocate_32bit_param_reg(&mut self) -> RegAllocResult {
        for i in 0..=CALLER_SAVED_REG_COUNT {
            let reg_name: String = format!("x{}", i);
            if let Some(free_reg) = self.regs64.get(reg_name.clone().as_str()) {
                if *free_reg == 1 {
                    self.regs64.insert(format!("x{i}"), 0);
                    return Ok(AllocedReg {
                        idx: i,
                        size: REG_64BIT
                    });
                }
            }
        }
        Err(RegAllocError)
    } 

    fn alllocate_64bit_param_reg(&mut self) -> RegAllocResult {
        for i in 0..=CALLER_SAVED_REG_COUNT {
            let reg_name: String = format!("x{}", i);
            if let Some(free_reg) = self.regs64.get(reg_name.clone().as_str()) {
                if *free_reg == 1 {
                    self.regs64.insert(format!("x{i}"), 0);
                    return Ok(AllocedReg {
                        idx: i,
                        size: REG_64BIT
                    });
                }
            }
        }
        Err(RegAllocError)
    } 

    fn deallocate_64bit_reg(&mut self, index: RegIdx) {
        let mut dealloc_name: String = String::from("");
        for (dindex, (reg_name, _)) in self.regs64.iter().enumerate() {
            if dindex == index {
                dealloc_name.push_str(reg_name);
                break;
            }
        }
        self.regs64.insert(dealloc_name, 1);
    }

    pub fn deallocate_32bit_param_reg(&mut self, index: usize) {
        let mut dealloc_name: String = String::from("");
        for (dindex, (reg_name, _)) in self.regs64.iter().enumerate() {
            if dindex == index {
                dealloc_name.push_str(reg_name);
                break;
            }
        }
        self.regs64.insert(dealloc_name, 1);
    } 

    pub fn deallocate_64bit_param_reg(&mut self, index: usize) {
        let mut dealloc_name: String = String::from("");
        for (dindex, (reg_name, _)) in self.regs64.iter().enumerate() {
            if dindex == index {
                dealloc_name.push_str(reg_name);
                break;
            }
        }
        self.regs64.insert(dealloc_name, 1);
    } 

    fn deallocate_all_regs(&mut self) {
        for (_, reg_status) in self.regs64.iter_mut() {
            *reg_status = 1;
        }
        for (_, reg_status) in self.regs64.iter_mut() {
            *reg_status = 1;
        }
    }

    pub fn mark_alloced(&mut self, idx: usize) {
        self.regs64.insert(format!("x{}", idx), 0);
    }
    
    pub fn is_free(&self, idx: RegIdx) -> bool {
        matches!(self.regs64.get(&format!("x{}", idx)), Some(1))
    }
}
