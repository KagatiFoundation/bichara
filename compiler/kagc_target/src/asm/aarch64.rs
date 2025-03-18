use std::collections::HashMap;

use kagc_types::LitTypeVariant;

use crate::reg::{AllocedReg, RegAllocError, RegAllocResult, RegIdx, RegManager, RegState, RegStatus, EARLY_RETURN};

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
    regs64: HashMap<String, RegState>,
}

impl RegManager for Aarch64RegManager {
    fn allocate(&mut self, val_type: &LitTypeVariant, offset: usize) -> RegAllocResult {
        match val_type {
            LitTypeVariant::U8
            | LitTypeVariant::I16
            | LitTypeVariant::I32 => self.alllocate_32bit_reg(offset),
            LitTypeVariant::I64 => self.alllocate_64bit_reg(offset),
            _ => Err(RegAllocError)
        }
    }
    
    fn deallocate(&mut self, idx: RegIdx) {
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
    
    fn name(&self, idx: RegIdx) -> String {
        if let Some(reg) = self.regs64.get(&format!("x{}", idx)) {
            return match reg.curr_alloced_size {
                32 => format!("w{}", reg.idx),
                64 => format!("x{}", reg.idx),
                _ => panic!("Not a valid 64-bit register")
            };
        }
        panic!("Not a valid 64-bit register");
    }
    
    fn get(&self, idx: RegIdx) -> Option<&RegState> {
        self.regs64.get(&format!("x{}", idx))
    }
}

impl Aarch64RegManager {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let mut regs64: HashMap<String, RegState> = HashMap::new();

        for i in 0..=REG_64BIT_COUNT {
            regs64.insert(
                format!("x{}", i), 
                RegState::new(
                    i, 
                    REG_32BIT, 
                    RegStatus::Free
                )
            );
        }
        Self {
            regs64
        }
    }

    fn alllocate_32bit_reg(&mut self, offset: usize) -> RegAllocResult {
        for i in offset..=REG_64BIT_COUNT {
            let reg_name: String = format!("x{}", i);
            if let Some(free_reg) = self.regs64.get(reg_name.clone().as_str()) {
                if free_reg.status == RegStatus::Free {
                    self.regs64.insert(
                        format!("x{i}"), 
                        RegState::new(
                            i, 
                            REG_32BIT, 
                            RegStatus::Alloced
                        )
                    );
                    return Ok(AllocedReg {
                        idx: i,
                        size: REG_32BIT
                    });
                }
            }
        }
        Err(RegAllocError)
    }

    fn alllocate_64bit_reg(&mut self, offset: usize) -> RegAllocResult {
        for i in offset..=REG_64BIT_COUNT {
            let reg_name: String = format!("x{}", i);
            if let Some(free_reg) = self.regs64.get(reg_name.clone().as_str()) {
                if free_reg.status == RegStatus::Free {
                        self.regs64.insert(
                            format!("x{i}"), 
                            RegState::new(
                                i, 
                                REG_64BIT, 
                                RegStatus::Alloced
                            )
                        );
                    return Ok(AllocedReg {
                        idx: i,
                        size: REG_64BIT
                    });
                }
            }
        }
        Err(RegAllocError)
    }

    fn alllocate_32bit_param_reg(&mut self) -> RegAllocResult {
        for i in 0..=CALLER_SAVED_REG_COUNT {
            let reg_name: String = format!("x{}", i);
            if let Some(free_reg) = self.regs64.get(reg_name.clone().as_str()) {
                    if free_reg.status == RegStatus::Free {
                        self.regs64.insert(
                            format!("x{i}"), 
                            RegState::new(
                                i, 
                                REG_32BIT, 
                                RegStatus::Alloced
                            )
                        );
                    return Ok(AllocedReg {
                        idx: i,
                        size: REG_32BIT
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
                    if free_reg.status == RegStatus::Free {
                        self.regs64.insert(
                            format!("x{i}"), 
                            RegState::new(
                                i, 
                                REG_64BIT, 
                                RegStatus::Alloced
                            )
                        );
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
        self.regs64.insert(
            dealloc_name, 
            RegState::new(index, REG_64BIT, RegStatus::Free)
        );
    }

    pub fn deallocate_64bit_param_reg(&mut self, index: usize) {
        let mut dealloc_name: String = String::from("");
        for (dindex, (reg_name, _)) in self.regs64.iter().enumerate() {
            if dindex == index {
                dealloc_name.push_str(reg_name);
                break;
            }
        }
        self.regs64.insert(
            dealloc_name, 
            RegState::new(index, REG_64BIT, RegStatus::Free)
        );
    } 

    fn deallocate_all_regs(&mut self) {
        for (_, reg_state) in self.regs64.iter_mut() {
            reg_state.status = RegStatus::Free;
        }
    }

    /// Make sure the `alloc_size` is `8`, `16` , `32`, or `64`. This function will 
    /// fail otherwise.
    pub fn mark_alloced(&mut self, idx: usize, alloc_size: usize) {
        self.regs64.insert(
            format!("x{}", idx), 
            RegState::new(idx, alloc_size, RegStatus::Alloced)
        );
    }
    
    pub fn is_free(&self, idx: RegIdx) -> bool {
        match self.regs64.get(&format!("x{}", idx)) {
            Some(RegState { status, .. }) => {
                *status == RegStatus::Free
            },
            _ => false
        }
    }
}
