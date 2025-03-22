use kagc_ir::ir_instr::{IRInstr, IR};
use kagc_target::reg::AllocedReg;

use crate::errors::CodeGenErr;

pub type CodeGenResult = Result<AllocedReg, CodeGenErr>;

pub type CGRes = Result<IR, CodeGenErr>;

pub type CGInstrRes = Result<IRInstr, CodeGenErr>;