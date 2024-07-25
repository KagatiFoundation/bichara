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

use core::panic;
use std::cell::RefMut;
use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::Stmt;
use crate::ast::{
    ASTOperation, 
    AST
};
use crate::code_gen::CodeGen;
use crate::code_gen::register::RegManager;
use crate::context::CompilerCtx;
use crate::symbol::{
    StorageClass, 
    Symbol, 
    FunctionInfo,
};
use crate::types::{
    LitType, 
    LitTypeVariant
};
use crate::utils::integer::split_i64_hex_half_each;
use crate::utils::integer::to_hex;
use crate::SymbolType;

lazy_static::lazy_static! {
    static ref CMP_CONDS_LIST: Vec<&'static str> = vec!["neq", "eq", "ge", "le", "lt", "gt"];
}

pub struct Aarch64CodeGen<'aarch64> {
    reg_manager: RefCell<RegManager>,
    ctx: Option<Rc<RefCell<CompilerCtx<'aarch64>>>>,
    label_id: usize
}

impl<'aarch64> CodeGen for Aarch64CodeGen<'aarch64> {
    fn gen_global_symbols(&self) {
        if self.ctx.is_none() {
            panic!("Error dumping global variables. Make sure the context is set for this CodeGen.");
        }
        if let Some(ctx_rc) = &self.ctx {
            let ctx_borrow = ctx_rc.borrow();
            for symbol in ctx_borrow.sym_table.iter() {
                // symbol information is not generated if any of the following conditions matches
                if symbol.lit_type == LitTypeVariant::None || symbol.sym_type == SymbolType::Function || symbol.class != StorageClass::GLOBAL {
                    continue;
                }
                if symbol.lit_type == LitTypeVariant::Str && symbol.sym_type == SymbolType::Constant {
                    let str_const_name_and_val: Vec<&str> = symbol.name.split("---").collect::<Vec<&str>>();
                    let str_const_name: String = String::from(str_const_name_and_val[0]);
                    println!(".data\n.global {str_const_name}");
                    println!("{str_const_name}:\n\t.asciz \"{}\"", str_const_name_and_val[1]);
                    continue;
                }
                println!(".data\n.global {}", symbol.name);
                if symbol.sym_type == SymbolType::Variable {
                    Aarch64CodeGen::dump_global_with_alignment(symbol);
                } else if symbol.sym_type == SymbolType::Array {
                    let array_data_size: usize = symbol.lit_type.size();
                    println!("{}:", symbol.name);
                    for _ in 0..symbol.size {
                        Aarch64CodeGen::alloc_data_space(array_data_size);
                    }
                }
            }
        }
    }
    
    fn gen_if_stmt(&mut self, ast: &AST) -> usize {
        let label_if_false: usize = self.get_next_label(); // label id to jump to if condition turns out to be false
        let label_end: usize = self.get_next_label(); // this label is put after the end of entire if-else block
        let cond_result_reg: usize = self.gen_code_from_ast(ast.left.as_ref().unwrap(), label_if_false, ast.operation);
        self.reg_manager.borrow_mut().deallocate_all();
        self.gen_code_from_ast(ast.mid.as_ref().unwrap(), cond_result_reg, ast.operation);
        self.reg_manager.borrow_mut().deallocate_all();
        // if there is an 'else' block
        if ast.right.is_some() {
            self.gen_jump(label_end);
        }
        // false label
        self.gen_label(label_if_false);
        if let Some(ref right_ast) = ast.right {
            self.gen_code_from_ast(right_ast, 0xFFFFFFFF, ast.operation);
            self.reg_manager.borrow_mut().deallocate_all();
            self.gen_label(label_end);
        }
        0xFFFFFFFF
    }
    
    fn gen_cmp_and_set(&self, operation: ASTOperation, r1: usize, r2: usize) -> usize {
        let r1name: String = self.reg_manager.borrow().name(r1);
        let r2name: String = self.reg_manager.borrow().name(r2);
        println!("cmp {}, {}", r1name, r2name);
        println!(
            "cset {}, {}",
            r2name,
            CMP_CONDS_LIST[operation as usize - ASTOperation::AST_EQEQ as usize]
        );
        println!("and {}, {}, 255", r2name, r2name);
        r2
    }

    fn gen_cmp_and_jmp(&self, operation: ASTOperation, r1: usize, r2: usize, label: usize) -> usize {
        let r1name: String = self.reg_manager.borrow().name(r1);
        let r2name: String = self.reg_manager.borrow().name(r2);
        println!("cmp {}, {}", r1name, r2name);
        println!(
            "b{} _L{}",
            CMP_CONDS_LIST[operation as usize - ASTOperation::AST_EQEQ as usize],
            label
        );
        self.reg_manager.borrow_mut().deallocate_all();
        0xFFFFFFFF
    }

    fn gen_function_stmt(&mut self, ast: &AST) -> usize {
        let possible_func_decl: Stmt = ast.kind.clone().unwrap_stmt();
        let index: usize = match possible_func_decl {
            Stmt::FuncDecl(func_decl) => func_decl.func_id,
            _ => panic!("Not a valid symbol table indexing method"),
        };
        let func_name: String = if let Some(ctx_rc) = &mut self.ctx {
            let ctx_borrow: std::cell::Ref<CompilerCtx<'aarch64>> = ctx_rc.borrow();
            ctx_borrow.sym_table.get_symbol(index).unwrap().name.clone()
        } else {
            panic!("Please provide a context to work on!");
        };
        let func_info: FunctionInfo = if let Some(ctx_rc) = &mut self.ctx {
            let ctx_borrow = ctx_rc.borrow();
            ctx_borrow.func_table.get(&func_name).unwrap().clone()
        } else {
            panic!("Please provide a context to work on!");
        };
        // function preamble
        if func_info.storage_class == StorageClass::EXTERN {
            println!(".extern _{}", func_name);
            return 0xFFFFFFFF;
        }
        println!(".global _{}\n_{}:", func_name, func_name);
        println!("sub sp, sp, {}", func_info.stack_size);
        if let Some(ref body) = ast.left {
            self.gen_code_from_ast(body, 0xFFFFFFFF, ast.operation);
        }
        // function postamble
        println!("add sp, sp, {}\nret", func_info.stack_size);
        0xFFFFFFFF
    }

    fn gen_while_stmt(&mut self, ast: &AST) -> usize {
        let label_start: usize = self.get_next_label();
        let label_end: usize = self.get_next_label();
        self.gen_label(label_start); // start of loop body
        self.gen_code_from_ast(ast.left.as_ref().unwrap(), label_end, ast.operation);
        self.reg_manager.borrow_mut().deallocate_all();
        self.gen_code_from_ast(ast.right.as_ref().unwrap(), 0xFFFFFFFF, ast.operation);
        self.reg_manager.borrow_mut().deallocate_all();
        self.gen_jump(label_start);
        self.gen_label(label_end);
        0xFFFFFFFF
    }

    fn gen_load_id_into_reg(&mut self, id: usize) -> usize {
        let value_containing_reg: usize = self.reg_manager.borrow_mut().allocate();
        let value_reg_name: String = self.reg_manager.borrow().name(value_containing_reg);
        let symbol: Symbol = if let Some(ctx_rc) = &mut self.ctx {
            let ctx_borrow = ctx_rc.borrow();
            ctx_borrow.sym_table.get_symbol(id).unwrap().clone()
        } else {
            panic!("Please provide a context to work on!");
        };
        if symbol.class == StorageClass::GLOBAL {
            let reg: usize = self.reg_manager.borrow_mut().allocate();
            let reg_name: String = self.reg_manager.borrow().name(reg);
            self.dump_gid_address_load_code_from_name(&reg_name, id);
            println!("ldr {}, [{}] // load {}", value_reg_name, reg_name, symbol.name);
        } else {
            println!("ldr {}, [sp, {}] // load {}", value_reg_name, symbol.local_offset, symbol.name);
        } 
        value_containing_reg
    }

    // Refer to this page for explanation on '@PAGE' and '@PAGEOFF': https://stackoverflow.com/questions/65351533/apple-clang12-llvm-unknown-aarch64-fixup-kind
    fn gen_load_reg_into_id(&mut self, reg: usize, id: usize) -> usize {
        let reg_name: String = self.reg_manager.borrow().name(reg);
        let symbol: Symbol = if let Some(ctx_rc) = &mut self.ctx {
            let ctx_borrow = ctx_rc.borrow();
            ctx_borrow.sym_table.get_symbol(id).unwrap().clone()
        } else {
            panic!("Please provide a context to work on!");
        };
        if symbol.class == StorageClass::GLOBAL {
            let addr_reg: usize = self.reg_manager.borrow_mut().allocate();
            let addr_reg_name: String = self.reg_manager.borrow().name(addr_reg);
            self.dump_gid_address_load_code_from_name(&addr_reg_name, id);
            println!("str {}, [{}] // store into {}", reg_name, addr_reg_name, symbol.name);
            addr_reg
        } else {
            println!("str {}, [sp, {}] // store into {}", reg_name, symbol.local_offset, symbol.name);
            0xFFFFFFFF
        }
    }

    // Load an integer literal into a register
    fn gen_load_intlit_into_reg(&mut self, value: &LitType) -> usize {
        let reg: usize = self.reg_manager.borrow_mut().allocate();
        let reg_name: String = self.reg_manager.borrow().name(reg);
        match value {
            LitType::I64(int_val) => {
                let splitted_i64 = split_i64_hex_half_each(to_hex(*int_val));
                println!("movz {}, 0x{}", reg_name, splitted_i64[0]);
                println!("movk {}, 0x{}", reg_name, splitted_i64[1]);
            },
            LitType::I32(int_val) => {
                let splitted_i64 = split_i64_hex_half_each(to_hex(*int_val));
                println!("movk {}, 0x{}, lsl #16", reg_name, splitted_i64[0]);
                println!("movk {}, 0x{}", reg_name, splitted_i64[1]);
            }
            LitType::I16(int_val) => {
                println!("mov {}, {}", reg_name, int_val);
            }
            LitType::U8(u8_val) => {
                println!("mov {}, {}", reg_name, *u8_val);
            }
            _ => panic!("Not a recognized type of integer: {:?}", value),
        };
        reg
    }

    // Generally speaking, loading one variable's address into another variable
    fn gen_id_address_into_another_id(&mut self, id: usize) -> usize {
        let reg_alloced: usize = self.reg_manager.borrow_mut().allocate();
        let reg_name: String = self.reg_manager.borrow().name(reg_alloced);
        self.dump_gid_address_load_code_from_name(&reg_name, id);
        reg_alloced
    }

    fn gen_deref_pointer_id(&mut self, id: usize) -> usize {
        let reg_alloced: usize = self.reg_manager.borrow_mut().allocate();
        let reg_name: String = self.reg_manager.borrow().name(reg_alloced);
        let value_reg: usize = self.reg_manager.borrow_mut().allocate();
        let value_reg_name: String = self.reg_manager.borrow().name(value_reg);
        self.dump_gid_address_load_code_from_name(&reg_name, id);
        println!("ldr {}, [{}]", value_reg_name, reg_name);
        value_reg
    }

    // id --> label information of the string
    fn gen_load_global_strlit(&mut self, id: &LitType) -> usize {
        let str_addr_reg: usize = self.reg_manager.borrow_mut().allocate();
        let str_addr_name: String = self.reg_manager.borrow().name(str_addr_reg);
        self.dump_gid_address_load_code_from_label_id(&str_addr_name, id);
        str_addr_reg
    }

    fn gen_add(&mut self, r1: usize, r2: usize) -> usize {
        println!(
            "add {}, {}, {}",
            self.reg_manager.borrow().name(r1),
            self.reg_manager.borrow().name(r1),
            self.reg_manager.borrow().name(r2)
        );
        self.reg_manager.borrow_mut().deallocate(r2);
        r1
    }

    fn gen_sub(&mut self, r1: usize, r2: usize) -> usize {
        println!(
            "sub {}, {}, {}",
            self.reg_manager.borrow().name(r1),
            self.reg_manager.borrow().name(r1),
            self.reg_manager.borrow().name(r2)
        );
        self.reg_manager.borrow_mut().deallocate(r2);
        r1
    }

    fn gen_return_stmt(&mut self, result_reg: usize, _func_id: usize) -> usize {
        // NOTE: Generate code depending on the function's type. i.e. use w0 for i32, x0 for i64 etc.
        // let func_ret_type: LitTypeVariant = self.sym_table.get_symbol(func_id).lit_type;
        if result_reg != 0xFFFFFFFF {
            println!(
                "mov x0, {}",
                self.reg_manager.borrow().name(result_reg)
            );
        }
        0xFFFFFFFF
    }

    fn gen_array_access(&mut self, id: usize, expr: &AST) -> usize {
        let expr_res_reg: usize = self.gen_code_from_ast(expr, 0xFFFFFFFF, ASTOperation::AST_ARRAY_ACCESS);
        let mut reg_mgr: RefMut<RegManager> = self.reg_manager.borrow_mut();
        let expr_res_reg_name: String = reg_mgr.name(expr_res_reg);
        let symbol: Symbol = if let Some(ctx_rc) = &mut self.ctx {
            let ctx_borrow = ctx_rc.borrow();
            ctx_borrow.sym_table.get_symbol(id).unwrap().clone()
        } else {
            panic!("Please provide to retrieve symbol from!");
        };
        let offset_shift: usize = match symbol.lit_type {
            LitTypeVariant::I32 | // as of now, this compiler does not know how to index 32-bit int array
            LitTypeVariant::I64 => 3, // so I am using offset of 8 bytes to calculate array indexes even though
                                        // items are 32-bit
            _ => 0,
        };
        // this will contain the address + offset of an array
        let addr_reg: usize = reg_mgr.allocate();
        let addr_reg_name: String = reg_mgr.name(addr_reg);
        let off_addr_reg: usize = reg_mgr.allocate();
        let off_addr_reg_name: String = reg_mgr.name(off_addr_reg);
        std::mem::drop(reg_mgr);
        self.dump_gid_address_load_code_from_name(&addr_reg_name, id);
        println!(
            "ldr {}, [{}, {}, lsl {}]",
            off_addr_reg_name, addr_reg_name, expr_res_reg_name, offset_shift
        );
        off_addr_reg
    }
    
    fn gen_array_access2(&mut self, symbol_id: usize, index: usize) -> usize {
        let mut reg_mgr: RefMut<RegManager> = self.reg_manager.borrow_mut();
        let expr_res_reg_name: String = reg_mgr.name(index);
        let symbol: Symbol = if let Some(ctx_rc) = &mut self.ctx {
            let ctx_borrow = ctx_rc.borrow();
            ctx_borrow.sym_table.get_symbol(symbol_id).unwrap().clone()
        } else {
            panic!("Please provide to retrieve symbol from!");
        };
        let offset_shift: usize = match symbol.lit_type {
            LitTypeVariant::I32 => 2,
            LitTypeVariant::I64 => 3,
            _ => 0,
        };
        // this will contain the address + offset of an array
        let addr_reg: usize = reg_mgr.allocate();
        let addr_reg_name: String = reg_mgr.name(addr_reg);
        let off_addr_reg: usize = reg_mgr.allocate();
        let off_addr_reg_name: String = reg_mgr.name(off_addr_reg);
        drop(reg_mgr);
        self.dump_gid_address_load_code_from_name(&addr_reg_name, symbol_id);
        println!(
            "ldr {}, [{}, {}, lsl {}]",
            off_addr_reg_name, addr_reg_name, expr_res_reg_name, offset_shift
        );
        off_addr_reg
    }
    
    fn gen_jump(&self, label_id: usize) {
        println!("b _L{}", label_id);
    }

    fn gen_label(&mut self, label: usize) {
        println!("_L{}:", label);
    }
    
    fn reg_manager(&self) -> RefMut<RegManager> {
        self.reg_manager.borrow_mut()
    }
    
    fn gen_func_call_stmt(&mut self, symbol_id: usize) {
        if let Some(ctx_rc) = &self.ctx {
            let ctx_borrow = ctx_rc.borrow_mut();
            if let Some(symbol) = ctx_borrow.sym_table.get_symbol(symbol_id) {
                println!("bl _{}", symbol.name);
            }
        }
    }
}

impl<'aarch64> Aarch64CodeGen<'aarch64> {
    pub fn new(
        reg_manager: RefCell<RegManager>,
    ) -> Self {
        Self {
            reg_manager,
            ctx: None,
            label_id: 0
        }
    }

    pub fn gen_with_ctx(&mut self, ctx: Rc<RefCell<CompilerCtx<'aarch64>>>, nodes:Vec<AST>) {
        self.label_id = ctx.borrow().label_id + 10;
        self.ctx = Some(ctx);
        self.start_gen(nodes);
    }

    fn get_next_label(&mut self) -> usize {
        let lbl: usize = self.label_id;
        self.label_id += 1;
        lbl
    }

    fn dump_gid_address_load_code_from_label_id(&self, reg_name: &str, id: &LitType) {
        let symbol_label_id: usize = match id {
            LitType::I32(_idx) => *_idx as usize,
            _ => panic!("Can't index symtable with this type: {:?}", id),
        };
        println!("adrp {}, _L{}@PAGE", reg_name, symbol_label_id);
        println!("add {}, {}, _L{}@PAGEOFF", reg_name, reg_name, symbol_label_id);
    }

    fn dump_gid_address_load_code_from_name(&mut self, reg_name: &str, id: usize) {
        if let Some(ctx_rc) = &mut self.ctx {
            let ctx_borrow = ctx_rc.borrow();
            let symbol: Symbol = ctx_borrow.sym_table.get_symbol(id).unwrap().clone();
            if symbol.class == StorageClass::GLOBAL {
                let sym_name: &str = &symbol.name;
                println!("adrp {}, {}@PAGE", reg_name, sym_name);
                println!("add {}, {}, {}@PAGEOFF", reg_name, reg_name, sym_name);
            }
        }
    }

    fn alloc_data_space(size: usize) {
        match size {
            1 => println!(".byte 0"),
            4 => println!(".word 0"),
            8 => println!(".quad 0"),
            _ => panic!("Not possible to generate space for size: {}", size),
        }
    }

    fn dump_global_with_alignment(symbol: &Symbol) {
        let def_val: String = if let Some(dv) = &symbol.default_value {
            dv.to_string()
        } else { "0".to_string() };
        match symbol.lit_type {
            LitTypeVariant::I32 => println!("{}: .align 4\n\t.word {}", symbol.name, def_val),
            LitTypeVariant::U8 => println!("{}:\t.byte {}", symbol.name, def_val),
            LitTypeVariant::Str => {
                let label_id: i32 = if let Some(lit_val) = &symbol.default_value {
                    match lit_val {
                        LitType::I32(__id) => *__id,
                        _ => panic!("Not a valid label id for string literal '{}'", symbol.default_value.as_ref().unwrap())
                    }
                } else {
                    panic!("No label id provided for string literal");
                };
                println!("{}:\t.quad ._L{:?}", symbol.name, label_id);
            }
            _ => panic!("Symbol's size is not supported right now: '{:?}'", symbol),
        }
    }
}