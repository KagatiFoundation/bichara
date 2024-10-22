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

use std::collections::HashMap;

/// Manages the allocation and status of registers.
/// 
/// This struct maintains a mapping between register names and their statuses,
/// where a status of '1' indicates that the register is free for allocation,
/// and a status of '0' indicates that the register is not available.
/// 
/// # Fields
/// 
/// * `registers` - A HashMap that maps register names (String) to their statuses (u8).
pub struct RegManager {
    registers: HashMap<String, u8>,
    param_regs: HashMap<String, u8>
}

impl RegManager {
    #[allow(clippy::new_without_default)]
    pub fn new(reg_names: Vec<String>, param_regs: Vec<String>) -> Self {
        Self {
            registers: {
                let mut regs: HashMap<String, u8> = HashMap::<String, u8>::new();
                for name in reg_names {
                    regs.insert(name, 1);
                }
                regs
            },
            param_regs: {
                let mut regs: HashMap<String, u8> = HashMap::<String, u8>::new();
                for name in param_regs {
                    regs.insert(name, 1);
                }
                regs
            }
        }
    }    

    pub fn allocate(&mut self) -> usize {
        for i in 0..29 {
            let reg_name = format!("x{}", i);
            if let Some(free_reg) = self.registers.get(reg_name.clone().as_str()) {
                if *free_reg == 1 {
                    self.registers.insert(reg_name, 0);
                    return i;
                }
            }
        }
        panic!("out of registers");
    }

    pub fn deallocate(&mut self, index: usize) {
        let mut dealloc_name: String = String::from("");
        for (dindex, (reg_name, _)) in self.registers.iter().enumerate() {
            if dindex == index {
                dealloc_name.push_str(reg_name);
                break;
            }
        }
        self.registers.insert(dealloc_name, 1);
    }

    pub fn allocate_param_reg(&mut self) -> usize {
        for i in 0..8 {
            let reg_name = format!("x{}", i);
            if let Some(free_reg) = self.param_regs.get(reg_name.clone().as_str()) {
                if *free_reg == 1 {
                    self.param_regs.insert(reg_name, 0);
                    return i;
                }
            }
        }
        panic!("out of parameter registers");
    }
    
    pub fn deallocate_param_reg(&mut self, index: usize) {
        let mut dealloc_name: String = String::from("");
        for (dindex, (reg_name, _)) in self.param_regs.iter().enumerate() {
            if dindex == index {
                dealloc_name.push_str(reg_name);
                break;
            }
        }
        self.param_regs.insert(dealloc_name, 1);
    }

    pub fn deallocate_all(&mut self) {
        for (_, reg_status) in self.registers.iter_mut() {
            *reg_status = 1;
        }
        self.deallocate_all_param_regs();
    }

    pub fn deallocate_all_param_regs(&mut self) {
        for (_, reg_status) in self.param_regs.iter_mut() {
            *reg_status = 1;
        }
    }

    pub fn name(&self, index: usize, offset: usize) -> String {
        if index > 56 {
            return String::from("");
        }
        let mut reg_name_prefix = "x";
        if offset != 0 {
            reg_name_prefix = "w";
        }
        format!("{}{}", reg_name_prefix, index)
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use super::RegManager;

    #[test]
    fn test_allocation_of_one_register() {
        let rm: RefCell<RegManager> = RefCell::new(
            RegManager::new(
                {
                    let mut regs: Vec<String> = vec![];
                    for i in 0..28 {
                        regs.push(format!("x{}", i));
                        regs.push(format!("w{}", i));
                    }
                    regs
                },
                {
                    let mut regs: Vec<String> = vec![];
                    for i in 0..7 {
                        regs.push(format!("x{}", i));
                        regs.push(format!("w{}", i));
                    }
                    regs
                }
            )
        );
        let reg: usize = rm.borrow_mut().allocate();
        assert!(reg != 0xFFFFFFFF);
    }
}