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

macro_rules! check_instr_type {
    ($fn_name:ident, $variant:ident) => {
        pub fn $fn_name(&self) -> bool {
            matches!(self, Self::$variant(..))
        }
    };
}

macro_rules! impl_as_irlit_type {
    ($fn_name:ident, $self_type:ident, $value_type:ident) => {
        pub fn $fn_name(&self) -> Option<$value_type> {
            match self {
                Self::$self_type(variant_value) => Some(variant_value.clone()),
                _ => None
            }
        }
    };
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

    check_instr_type!(is_temp, Temp);
    check_instr_type!(is_var, Var);
    check_instr_type!(is_const, Const);
    check_instr_type!(is_reg, Reg);

    impl_as_irlit_type!(as_temp, Temp, usize);
    impl_as_irlit_type!(as_reg, Reg, AllocedReg);
    impl_as_irlit_type!(as_var, Var, String);
    impl_as_irlit_type!(as_const, Const, IRLitVal);
}