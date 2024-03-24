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

use std::cell::RefMut;
use std::cell::RefCell;

use crate::ast::{
    ASTOperation, 
    AST
};
use crate::code_gen::CodeGen;
use crate::code_gen::register::RegManager;
use crate::symbol::{
    StorageClass, 
    Symbol, 
    Symtable,
    FunctionInfo,
    FunctionInfoTable
};
use crate::types::{
    LitType, 
    LitTypeVariant
};
use crate::SymbolType;

lazy_static::lazy_static! {
    static ref CMP_CONDS_LIST: Vec<&'static str> = vec!["neq", "eq", "ge", "le", "lt", "gt"];
}

pub struct Aarch64CodeGen<'a> {
    _label_id: &'static mut usize,
    reg_manager: RefCell<RegManager>,
    func_info_table: &'a mut FunctionInfoTable,
    sym_table: &'a mut Symtable,
}

impl<'a> CodeGen for Aarch64CodeGen<'a> {
    fn gen_global_symbol(&self) {
        for symbol in self.sym_table.iter() {
            // symbol information is not generated if any of the following conditions matches
            let not_process_cond: Vec<bool> = vec![symbol.sym_type == SymbolType::Function, symbol.lit_type == LitTypeVariant::None, symbol.class == StorageClass::LOCAL];
            if not_process_cond.iter().any(|item| *item) {
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
    
    fn gen_if_stmt(&mut self, ast: &crate::ast::AST) -> usize {
        let label_if_false: usize = self.get_next_label(); // label id to jump to if condition turns out to be false
        let mut label_end: usize = 0xFFFFFFFF; // this label is put after the end of entire if-else block
        if ast.right.is_some() {
            label_end = self.get_next_label();
        }
        self.gen_code_from_ast(ast.left.as_ref().unwrap(), label_if_false, ast.operation);
        self.reg_manager.borrow_mut().deallocate_all();
        self.gen_code_from_ast(ast.mid.as_ref().unwrap(), 0xFFFFFFFF, ast.operation);
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
    
    fn gen_jump(&self, label_id: usize) {
        println!("b _L{}", label_id);
    }

    fn gen_label(&mut self, label: usize) {
        println!("_L{}:", label);
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
        let index: usize = match ast.value.as_ref().unwrap() {
            LitType::I32(int_idx) => *int_idx as usize,
            _ => panic!("Not a valid symbol table indexing method"),
        };
        let func_name: String = self.sym_table.get_symbol(index).unwrap().name.clone();
        let func_info: FunctionInfo = self.func_info_table.get(&func_name).unwrap().clone();
        // function preamble
        println!(".global _{}\n_{}:", func_name, func_name);
        println!("sub sp, sp, {}", func_info.stack_size);
        if let Some(ref body) = ast.left {
            self.gen_code_from_ast(body, 0xFFFFFFFF, ast.operation);
        }
        // function postamble
        println!("add sp, sp, {}", func_info.stack_size);
        // in case the function has a void return type, we manually insert a 
        // "ret" instruction at the end of the function
        if func_info.return_type == LitTypeVariant::Void {
            println!("ret");
        }
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
        let symbol: Option<&Symbol> = self.sym_table.get_symbol(id);
        if symbol.is_none() {
            return 0xFFFFFFFF;
        }
        let symbol: &Symbol = symbol.unwrap();
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
        let symbol: &Symbol = self.sym_table.get_symbol(id).unwrap();
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

    // Load a integer literal into a register
    fn gen_load_intlit_into_reg(&mut self, value: &LitType) -> usize {
        let r: usize = self.reg_manager.borrow_mut().allocate();
        let reg_name: String = self.reg_manager.borrow().name(r);
        let int_value: i64 = match value {
            LitType::I32(int_val) => *int_val as i64,
            LitType::U8(u8_val) => *u8_val as i64,
            _ => panic!("Not a recognized type of integer: {:?}", value),
        };
        println!("mov {}, {}", reg_name, int_value);
        r
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
                "mov x0, {}\nret",
                self.reg_manager.borrow().name(result_reg)
            );
        }
        0xFFFFFFFF
    }

    fn gen_array_access(&mut self, id: usize, expr: &AST) -> usize {
        let expr_res_reg: usize = self.gen_code_from_ast(expr, 0xFFFFFFFF, ASTOperation::AST_ARRAY_ACCESS);
        let mut reg_mgr = self.reg_manager.borrow_mut();
        let expr_res_reg_name: String = reg_mgr.name(expr_res_reg);
        let symbol: &Symbol = self.sym_table.get_symbol(id).unwrap();
        let offset_shift: usize = match symbol.lit_type {
            LitTypeVariant::I32 | // as of now, this compiler does not know how to index 32-bit int array
            LitTypeVariant::I64 => 3, // so I am using offset of 8 to calculate array indexes even though
                                        // items are 32-bit
            _ => 0,
        };
        // this will contain the address + offset of an array
        let addr_reg: usize = reg_mgr.allocate();
        let addr_reg_name: String = reg_mgr.name(addr_reg);
        self.dump_gid_address_load_code_from_name(&addr_reg_name, id);
        let off_addr_reg: usize = reg_mgr.allocate();
        let off_addr_reg_name: String = reg_mgr.name(off_addr_reg);
        println!(
            "ldr {}, [{}, {}, lsl {}]",
            off_addr_reg_name, addr_reg_name, expr_res_reg_name, offset_shift
        );
        off_addr_reg
    }
    
    fn reg_manager(&self) -> RefMut<RegManager> {
        self.reg_manager.borrow_mut()
    }
}

impl<'a> Aarch64CodeGen<'a> {
    pub fn new(
        reg_manager: RefCell<RegManager>,
        sym_table: &'a mut Symtable,
        func_info_table: &'a mut FunctionInfoTable,
        label_id: &'static mut usize
    ) -> Self {
        Self {
            reg_manager,
            sym_table,
            func_info_table,
            _label_id: label_id,
        }
    }

    fn get_next_label(&mut self) -> usize {
        let label: usize = *self._label_id;
        (*self._label_id) += 1;
        label
    }

    fn dump_gid_address_load_code_from_label_id(&self, reg_name: &str, id: &LitType) {
        let symbol_label_id: usize = match id {
            LitType::I32(_idx) => *_idx as usize,
            _ => panic!("Can't index symtable with this type: {:?}", id),
        };
        println!("adrp {}, _L{}@PAGE", reg_name, symbol_label_id);
        println!("add {}, {}, _L{}@PAGEOFF", reg_name, reg_name, symbol_label_id);
    }

    fn dump_gid_address_load_code_from_name(&self, reg_name: &str, id: usize) {
        let symbol: &Symbol = self.sym_table.get_symbol(id).unwrap();
        if symbol.class == StorageClass::GLOBAL {
            let sym_name: &str = &symbol.name;
            println!("adrp {}, {}@PAGE", reg_name, sym_name);
            println!("add {}, {}, {}@PAGEOFF", reg_name, reg_name, sym_name);
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
        match symbol.lit_type {
            LitTypeVariant::F32Ptr
            | LitTypeVariant::F64Ptr
            | LitTypeVariant::I16Ptr
            | LitTypeVariant::I32Ptr
            | LitTypeVariant::I64Ptr
            | LitTypeVariant::U8Ptr
            | LitTypeVariant::VoidPtr => println!("{}: .align 8\n\t.quad 0", symbol.name),
            LitTypeVariant::I32 => println!("{}: .align 4\n\t.word 0", symbol.name),
            LitTypeVariant::U8 => println!("{}:\t.byte 0", symbol.name),
            _ => panic!("Symbol's size is not supported right now: '{:?}'", symbol),
        }
    }
}