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

// Literal Value Type Of The Program
#[derive(Debug, Clone)]
pub enum LitType {
    I64(i64),
    U64(u64),
    I32(i32),
    U32(u32),
    I16(i16), // two byte
    U16(u16), // two byte unsigned
    I8(i8), // one byte
    U8(u8), // one byte unsigned
    F64(f64),
    F32(f32),
    String(String),
    Void,
    None
}

impl PartialEq for LitType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::I32(l0), Self::I32(r0)) => l0 == r0,
            (Self::I64(l0), Self::I64(r0)) => l0 == r0,
            (Self::I16(l0), Self::I16(r0)) => l0 == r0,
            (Self::F64(l0), Self::F64(r0)) => l0 == r0,
            (Self::F32(l0), Self::F32(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl LitType {
    /// Verify the types' compatibility. The information on two kinds' compatibility is 
    /// contained in the three-value tuple that this method produces. The first value of 
    /// the tuple is `true` if they are compatible, and `false` otherwise. The purpose of 
    /// the other two integer values is to see whether any of them need to be expanded. 
    /// It indicates that the first value has to be broadened if the first integer value is `1`. 
    /// `0` if it is not required. The second value is also subject to the same constraints.
    pub fn compatible(&self, other: &Self) -> (bool, i32, i32) {
        match (self, other) {
            (LitType::Void, LitType::Void) => (false, 0, 0),
            (LitType::I32(_), LitType::U8(_)) => (true, 0, 1),
            (LitType::U8(_), LitType::I32(_)) => (true, 1, 0),
            // anything remaining is compatible
            _ => (true, 1, 1)
        }
    }

    pub fn convert(&self, to: LitType) -> Self {
        match (self, to.clone()) {
            // converting char into integer
            (LitType::U8(u8_value), LitType::I32(_)) => LitType::I32(*u8_value as i32),
            // converting integer into char
            (LitType::I32(i32_value), LitType::U8(_)) => {
                if *i32_value >= 0 && *i32_value < 256 { LitType::U8(*i32_value as u8) }
                else { panic!("Error: Can't convert {:?} to unsigned char. Value overflows.", self) }
            },
            _ => panic!("Can't be converted from {:?} to {:?}", self, to)
        }
    }
}