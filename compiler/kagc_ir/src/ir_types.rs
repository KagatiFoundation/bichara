#[derive(Debug)]
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

#[derive(Debug)]
pub enum IRLitType {
    Var(String),
    Const(IRLitVal),
    Reg(usize)
}

impl IRLitType {
    pub fn into_str(&self) -> String {
        match self {
            IRLitType::Var(var) => var.clone(),
            IRLitType::Const(irlit_val) => irlit_val.into_str(),
            IRLitType::Reg(reg) => format!("x{}", reg),
        }
    }
}