use kagc_ir::ir_instr::{IRInstr, IR};
use kagc_target::reg::AllocedReg;

use crate::errors::CodeGenErr;

pub type CodeGenResult = Result<AllocedReg, CodeGenErr>;

pub type CGRes = Result<Vec<IR>, CodeGenErr>;

pub type CGExprEvalRes = Result<Vec<IRInstr>, CodeGenErr>;

pub type TempCounter = usize;

pub type StackOffset = usize;