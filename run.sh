#!/bin/sh

BICH_STD_LIB_PATH=.

RUST_BACKTRACE=1 cargo run --bin kagc 1> main.S
gcc  -o out main.S -L$BICH_STD_LIB_PATH -lbich

export DYLD_LIBRARY_PATH=./:$DYLD_LIBRARY_PATH
./out