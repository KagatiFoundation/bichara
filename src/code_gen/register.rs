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
    registers: HashMap<String, u8>
}

impl RegManager {
    #[allow(clippy::new_without_default)]
    pub fn new(reg_names: Vec<String>) -> Self {
        Self {
            registers: {
                let mut regs: HashMap<String, u8> = HashMap::<String, u8>::new();
                for name in reg_names {
                    regs.insert(name, 1);
                }
                regs
            }
        }
    }    

    pub fn allocate(&mut self) -> usize {
        for (index, (reg_name, status)) in self.registers.iter().enumerate() {
            if *status == 1 {
                self.registers.insert(reg_name.clone(), 0);
                return index;
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

    pub fn deallocate_all(&mut self) {
        for (_, reg_status) in self.registers.iter_mut() {
            *reg_status = 1;
        }
    }

    pub fn name(&self, index: usize) -> String {
        let mut dealloc_name: String = String::from("");
        for (dindex, (reg_name, _)) in self.registers.iter().enumerate() {
            if dindex == index {
                dealloc_name = reg_name.clone();
                break;
            }
        }
        dealloc_name 
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use super::RegManager;

    #[test]
    fn test_allocation_of_one_register() {
        let rm: RefCell<RegManager> = RefCell::new(RegManager::new({
            let mut regs: Vec<String> = vec![];
            for i in 0..8 {
                regs.push(format!("x{}", i));
                regs.push(format!("w{}", i));
            }
            regs
        }));
        let reg: usize = rm.borrow_mut().allocate();
        assert!(reg != 0xFFFFFFFF);
    }
}