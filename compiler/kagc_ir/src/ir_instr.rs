use kagc_symbol::StorageClass;
use kagc_target::reg::{AllocedReg, RegIdx};

use crate::ir_types::*;

#[derive(Debug, Clone)]
pub enum IRInstr {
    Mov(IRLitType, IRLitType),
    
    Add(
        IRLitType, 
        IRLitType, 
        IRLitType
    ),
    
    Call(String, Vec<IRLitType>, IRLitType),
    
    Store {
        source_reg: RegIdx,
        stack_off: usize
    },
    
    /// Restore value from stack onto the register.
    Load {
        target_reg: RegIdx,
        stack_off: usize
    }
}

impl IRInstr {
    pub fn dest(&self) -> Option<IRLitType> {
        match self {
            IRInstr::Mov(dst, _) => Some(dst.clone()),
            IRInstr::Add(dst, _, _) => Some(dst.clone()),
            IRInstr::Call(_, _, _) => Some(IRLitType::Reg(AllocedReg { idx: 0, size: 64 })),
            IRInstr::Load { target_reg, .. } => {
                Some(IRLitType::Reg(AllocedReg { size: 64, idx: *target_reg }))
            }
            IRInstr::Store { .. } => None, // No destination
        }
    }
}

#[derive(Debug)]
pub struct IRFunc {
    pub name: String,
    pub params: Vec<IRLitType>,
    pub body: Vec<IR>,
    pub class: StorageClass,
    pub is_leaf: bool
}

#[derive(Debug)]
pub struct IRVarDecl {
    pub sym_name: String,
    pub class: StorageClass,
    pub value: IRLitType,
    pub offset: Option<usize>
}

#[derive(Debug)]
pub enum IR {
    Func(IRFunc),
    VarDecl(IRVarDecl),
    Instr(IRInstr)
}