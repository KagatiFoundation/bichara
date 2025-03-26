use kagc_target::reg::AllocedReg;

#[derive(Debug, Clone)]
pub enum IRLitVal {
    Str(String),
    Int64(i64),
    Int32(i32)
}

impl IRLitVal {
    pub fn into_str(&self) -> String {
        match self {
            IRLitVal::Str(value) => value.clone(),
            IRLitVal::Int64(value) => format!("{}", value),
            IRLitVal::Int32(value) => format!("{}", value),
        }
    }
}

#[derive(Debug, Clone)]
pub enum IRLitType {
    Var(String),
    Const(IRLitVal),
    Reg(AllocedReg),
    Temp(usize)
}

impl IRLitType {
    pub fn into_str(&self) -> String {
        match self {
            Self::Var(var) => var.clone(),
            Self::Const(irlit_val) => irlit_val.into_str(),
            Self::Reg(reg) => reg.idx.to_string(),
            Self::Temp(tmp) => tmp.to_string()
        }
    }
}