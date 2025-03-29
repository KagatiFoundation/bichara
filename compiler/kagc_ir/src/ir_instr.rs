use kagc_symbol::StorageClass;
use kagc_target::reg::*;

use crate::ir_types::*;

#[derive(Debug, Clone)]
pub enum IRInstr {
    Mov(IRLitType, IRLitType),
    
    Add(
        IRLitType, 
        IRLitType, 
        IRLitType
    ),

    Call {
        fn_name: String,

        params: Vec<IRLitType>,

        return_type: IRLitType
    },
    
    Load {
        /// Destination to load to 
        dest: IRLitType,

        /// Stack offset to load value from
        stack_off: usize
    },
}

impl IRInstr {
    pub fn dest(&self) -> Option<IRLitType> {
        match self {
            IRInstr::Mov(dst, _) => Some(dst.clone()),

            IRInstr::Add(dst, _, _) => Some(dst.clone()),

            IRInstr::Call { .. } => Some(IRLitType::Reg(AllocedReg { idx: 0, size: 64 })),

            IRInstr::Load { dest, .. } => Some(dest.clone())
        }
    }

    pub fn mov_into_temp(temp: usize, value: IRLitType) -> Self {
        Self::Mov(
            IRLitType::Temp(temp), 
            value
        )
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