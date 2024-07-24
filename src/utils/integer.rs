use std::fmt::LowerHex;

pub fn to_hex<T: LowerHex>(x: T) -> String {
    format!("0x{:x}", x)
}

pub fn split_i64_hex_half_each(x: String) -> Vec<String> {
    if !x.starts_with("0x") {
        panic!("{} is not a hex string", x);
    }
    let only_hex = x.trim_start_matches("0x");
    if only_hex.len() > 16 {
        panic!("{} exceeds the length of a 64-bit hex string", x);
    }
    let padded_hex = format!("{:0>16}", only_hex);
    let (first_half, second_half) = padded_hex.split_at(8);
    vec![String::from(first_half), String::from(second_half)]
}