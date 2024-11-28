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

use crate::types::LitTypeVariant;

use super::reg_error::RegAllocError;

/// Register size
pub type RegSize = usize;

/// Register index
pub type RegIdx = usize;

pub const INVALID_REG_IDX: usize = 0xFFFFFFFF;

#[derive(Debug)]
pub struct AllocedReg {
    pub size: RegSize,
    pub idx: RegIdx
}

impl AllocedReg {
    pub fn no_reg() -> Self {
        Self {
            size: 0,
            idx: INVALID_REG_IDX
        }
    }

    pub fn is_valid(&self) -> bool {
        self.idx != INVALID_REG_IDX
    }

    pub fn name(&self) -> String {
        match self.size {
            32 => format!("w{}", self.idx),
            64 => format!("x{}", self.idx),
            _ => String::from("")
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub enum RegStatus {
    Alloced,
    Free
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub struct RegState {
    pub idx: usize,
    pub curr_alloced_size: usize,
    pub status: RegStatus
}

impl RegState {
    pub fn new(idx: usize, size: usize, status: RegStatus) -> Self {
        Self {
            idx,
            curr_alloced_size: size,
            status
        }
    }
}

pub type RegAllocResult = Result<AllocedReg, RegAllocError>;

pub trait RegManager {
    fn allocate(&mut self, var_type: &LitTypeVariant) -> RegAllocResult;
    fn deallocate(&mut self, index: RegIdx);
    fn deallocate_all(&mut self);

    fn allocate_param_reg(&mut self, var_type: &LitTypeVariant) -> RegAllocResult;
    fn deallocate_param_reg(&mut self, idx: RegIdx);
    // fn deallocate_all_param_regs(&mut self);

    fn name(&self, idx: RegIdx) -> String;

    fn get(&self, idx: RegIdx) -> Option<&RegState>;
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_allocation_of_one_register() {
        
    }
}