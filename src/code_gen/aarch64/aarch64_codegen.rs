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
use std::cell::RefCell;
use std::cell::RefMut;
use std::rc::Rc;

use crate::ast::Expr;
use crate::ast::FuncCallStmt;
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
use crate::utils::integer::i32_hex_split_half;
use crate::utils::integer::i64_hex_split_quarter;
use crate::utils::integer::to_hex;
use crate::SymbolType;

lazy_static::lazy_static! {
    static ref CMP_CONDS_LIST: Vec<&'static str> = vec!["ne", "eq", "ge", "le", "lt", "gt"];
}

pub struct Aarch64CodeGen<'aarch64> {
    reg_manager: RefCell<RegManager>,
    ctx: Option<Rc<RefCell<CompilerCtx<'aarch64>>>>,
    label_id: usize,
    function_id: usize
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
    
    fn gen_if_stmt(&mut self, ast: &AST, reg: usize) -> usize {
        // Label for jumping to the 'else' block if the condition is false
        let label_if_false: usize = self.get_next_label();
    
        // Label marking the end of the entire if-else block
        let label_end: usize = self.get_next_label();
    
        // Evaluate the condition and store the result in a register
        let _cond_result_reg: usize = self.gen_code_from_ast(ast.left.as_ref().unwrap(), label_if_false, ast.operation);
    
        // Free registers to allow usage within the if-else body
        self.reg_manager.borrow_mut().deallocate_all();
    
        // Generate code for the 'if-true' block
        self.gen_code_from_ast(ast.mid.as_ref().unwrap(), reg, ast.operation);
    
        // Free registers again to prepare for any subsequent operations
        self.reg_manager.borrow_mut().deallocate_all();
    
        // Jump to the end of the if-else block to skip the 'else' block if present
        if ast.right.is_some() {
            self.gen_jump(label_end);
        }
    
        // Label for the start of the 'else' block
        self.gen_label(label_if_false);
    
        // Generate code for the 'else' block, if it exists
        if let Some(ref right_ast) = ast.right {
            self.gen_code_from_ast(right_ast, reg, ast.operation);
            self.reg_manager.borrow_mut().deallocate_all();
            self.gen_label(label_end); // Mark the end of the if-else block
        }
        0xFFFFFFFF
    }
    
    fn gen_cmp_and_set(&self, operation: ASTOperation, r1: usize, r2: usize) -> usize {
        let r1name: String = self.reg_manager.borrow().name(r1);
        let r2name: String = self.reg_manager.borrow().name(r2);
        println!("cmp {}, {}", r1name, r2name);
        let compare_operator: &str = match operation {
            ASTOperation::AST_LTHAN => "bge",
            ASTOperation::AST_GTHAN => "ble",
            ASTOperation::AST_EQEQ => "bne",
            ASTOperation::AST_NEQ => "beq",
            ASTOperation::AST_GTEQ => "blt",
            ASTOperation::AST_LTEQ => "bgt",
            _ => panic!("Not a valid ASTOperation for cmp and set")
        };
        println!(
            "cset {}, {}",
            r2name,
            compare_operator
        );
        println!("and {}, {}, 255", r2name, r2name);
        r2
    }

    fn gen_cmp_and_jmp(&self, operation: ASTOperation, r1: usize, r2: usize, label: usize) -> usize {
        let r1name: String = self.reg_manager.borrow().name(r1);
        let r2name: String = self.reg_manager.borrow().name(r2);
        println!("cmp {}, {}", r1name, r2name);
        let compare_operator: &str = match operation {
            ASTOperation::AST_LTHAN => "bhs",
            ASTOperation::AST_GTHAN => "bls",
            ASTOperation::AST_EQEQ => "bne",
            ASTOperation::AST_NEQ => "beq",
            ASTOperation::AST_GTEQ => "blo",
            ASTOperation::AST_LTEQ => "bhi",
            _ => panic!("Not a valid ASTOperation for cmp and jmp")
        };
        println!(
            "{} _L{}",
            compare_operator,
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

        // If the function is declared as extern, print its external linkage
        // declaration and return a placeholder value indicating an unresolved
        // address (0xFFFFFFFF).
        if func_info.storage_class == StorageClass::EXTERN {
            println!(".extern _{}", func_name);
            return 0xFFFFFFFF;
        }

        // Function preamble
        println!(".global _{}\n_{}:", func_name, func_name);
        println!("sub sp, sp, {}", func_info.stack_size);

        // STP -> Store Pair of Registers
        // Save the current frame pointer (x29) and link register (x30) onto the stack.
        println!("stp x29, x30, [sp, #16]");

        // generating code for function parameters
        for param_sym in func_info.params.iter() {
            if param_sym.lit_type != LitTypeVariant::None {
                let param_reg = self.reg_manager.borrow_mut().allocate_param_reg();
                println!("str {}, [sp, #{}]", self.reg_manager.borrow().name(param_reg), param_sym.offset);
            }
        }

        // deallocate all of the registers after manging function parameters
        if func_info.params.iter().len() > 0 {
            self.reg_manager.borrow_mut().deallocate_all_param_regs();
        }

        if let Some(ref body) = ast.left {
            self.gen_code_from_ast(body, 0xFFFFFFFF, ast.operation);
        }
        // ldp -> Load Pair of Registers
        // Restore the saved frame pointer (x29) and link register (x30) from the stack.
        println!("ldp x29, x30, [sp, #16]");

        println!("add sp, sp, {}\nret", func_info.stack_size);
        0xFFFFFFFF
    }

    fn gen_while_stmt(&mut self, ast: &AST) -> usize {
        let label_start: usize = self.get_next_label();
        let label_end: usize = self.get_next_label();
        self.gen_label(label_start); // start of loop body
        self.gen_code_from_ast(ast.left.as_ref().unwrap(), label_end, ast.operation);
        self.reg_manager.borrow_mut().deallocate_all();
        self.gen_code_from_ast(ast.right.as_ref().unwrap(), label_end, ast.operation);
        self.reg_manager.borrow_mut().deallocate_all();
        self.gen_jump(label_start);
        self.gen_label(label_end);
        0xFFFFFFFF
    }

    fn gen_loop_stmt(&mut self, ast: &AST) {
        // loop start label
        let label_start: usize = self.get_next_label();

        // loop end label
        let label_end: usize = self.get_next_label();

        self.gen_label(label_start);
        self.gen_code_from_ast(ast.left.as_ref().unwrap(), label_end, ast.operation);
        self.reg_manager.borrow_mut().deallocate_all();
        self.gen_jump(label_start);
        self.gen_label(label_end);
    }

    fn gen_break_stmt(&mut self, break_label: usize) -> usize {
        println!("b _L{}", break_label);
        0xFFFFFFFF
    }

    fn gen_load_id_into_reg(&mut self, id: usize) -> usize {
        let mut reg: usize = 0xFFFFFFFF;
        let inside_func: bool = self.function_id != reg;
        if inside_func {
            reg = self.reg_manager.borrow_mut().allocate_param_reg();
        } else {
            reg = self.reg_manager.borrow_mut().allocate();
        }
        let value_reg_name: String = self.reg_manager.borrow().name(reg);
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
            println!("ldr {}, [sp, #{}] // load {}", value_reg_name, symbol.local_offset, symbol.name);
        } 
        reg
    }

    // Refer to this page for explanation on '@PAGE' and '@PAGEOFF': https://stackoverflow.com/questions/65351533/apple-clang12-llvm-unknown-aarch64-fixup-kind
    fn gen_store_reg_value_into_id(&mut self, reg: usize, id: usize) -> usize {
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
            println!("str {}, [sp, #{}] // store into {}", reg_name, symbol.local_offset, symbol.name);
            0xFFFFFFFF
        }
    }

    // Load an integer literal into a register
    fn gen_load_intlit_into_reg(&mut self, value: &LitType) -> usize {
        let mut reg: usize = 0xFFFFFFFF;
        let inside_func: bool = self.function_id != reg;
        if inside_func {
            reg = self.reg_manager.borrow_mut().allocate_param_reg();
        } else {
            reg = self.reg_manager.borrow_mut().allocate();
        }
        let reg_name: String = self.reg_manager.borrow().name(reg);
        let result: Result<String, ()> = Aarch64CodeGen::gen_int_value_load_code(value, &reg_name);
        if let Ok(code) = result {
            println!("{}", code);
        } else {
            panic!("Was that supposed to be an integer: {:?}", value);
        }
        reg
    }

    // id --> label information of the string
    fn gen_load_global_strlit(&mut self, id: &LitType) -> usize {
        let mut reg: usize = 0xFFFFFFFF;
        let inside_func: bool = self.function_id != reg;
        if inside_func {
            reg = self.reg_manager.borrow_mut().allocate_param_reg();
        } else {
            reg = self.reg_manager.borrow_mut().allocate();
        }
        let str_addr_name: String = self.reg_manager.borrow().name(reg);
        self.dump_gid_address_load_code_from_label_id(&str_addr_name, id);
        reg
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

   fn gen_mul(&mut self, r1: usize, r2: usize) -> usize {
        println!(
            "mul {}, {}, {}",
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
        // if result_reg != 0xFFFFFFFF {
        //     println!(
        //         "mov x0, {}",
        //         self.reg_manager.borrow().name(result_reg)
        //     );
        // }
        0xFFFFFFFF
    }

    fn gen_array_access(&mut self, id: usize, expr: &AST) -> usize {
        let expr_res_reg: usize = self.gen_code_from_ast(expr, 0xFFFFFFFF, ASTOperation::AST_ARRAY_ACCESS);
        let mut reg_mgr = self.reg_manager.borrow_mut();
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
    
    fn gen_func_call_stmt(&mut self, func_call_stmt: &FuncCallStmt) {
        let func_info: FunctionInfo = if let Some(ctx_rc) = &self.ctx {
            let ctx_borrow = ctx_rc.borrow_mut();
            if let Some(symbol) = ctx_borrow.sym_table.get_symbol(func_call_stmt.symtbl_pos) {
                if let Some(func_info) = ctx_borrow.func_table.get(&symbol.name) {
                    func_info.clone()
                } else {
                    panic!("function info not found");
                }
            } else {
                panic!("undefined symbol");
            }
        } else {
            panic!("No context provided for code generation");
        };
        // Assign the function id whenever generating code for function
        self.function_id = func_info.func_id;
        let args: &Vec<crate::ast::Expr> = &func_call_stmt.args;
        for expr in args {
            _ = self.gen_expr(expr, ASTOperation::AST_FUNC_CALL, 0xFFFFFFFF, ASTOperation::AST_NONE);
            // let reg_name: String = self.reg_manager.borrow().name(reg);
            // let param_reg: usize = self.reg_manager.borrow_mut().allocate_param_reg();
            // let param_reg_name: String = self.reg_manager.borrow().name(param_reg);
            // println!("mov {}, {}", param_reg_name, reg_name);
        }
        println!("bl _{}", func_info.name);
        self.function_id = 0xFFFFFFFF;
    }
    
    fn gen_local_var_decl_stmt(&mut self, var_decl_stmt: &crate::ast::VarDeclStmt, expr_ast: &Expr) {
        let symbol: Symbol = if let Some(ctx_rc) = &self.ctx {
            let ctx_borrow = ctx_rc.borrow_mut();
            if let Some(symbol) = ctx_borrow.sym_table.get_symbol(var_decl_stmt.symtbl_pos) {
                symbol.clone()
            } else {
                panic!("not possible to reach here");
            }
        } else {
            panic!("no context provided");
        };
        let expr_reg = self.gen_expr(expr_ast, ASTOperation::AST_VAR_DECL, 0xFFFFFFFF, ASTOperation::AST_NONE);
        let reg_name: String = self.reg_manager.borrow().name(expr_reg);
        println!("str {}, [sp, #{}] // store into {}", reg_name, symbol.local_offset, symbol.name);
    }
    
    fn gen_func_call_expr(&mut self, func_call_expr: &crate::ast::FuncCallExpr) -> usize {
        let func_info: FunctionInfo = if let Some(ctx_rc) = &self.ctx {
            let ctx_borrow = ctx_rc.borrow_mut();
            if let Some(symbol) = ctx_borrow.sym_table.get_symbol(func_call_expr.symtbl_pos) {
                if let Some(func_info) = ctx_borrow.func_table.get(&symbol.name) {
                    func_info.clone()
                } else {
                    panic!("function info not found");
                }
            } else {
                panic!("undefined symbol");
            }
        } else {
            panic!("No context provided for code generation");
        };
        let args: &Vec<crate::ast::Expr> = &func_call_expr.args;
        for expr in args {
            _ = self.gen_expr(expr, ASTOperation::AST_FUNC_CALL, 0xFFFFFFFF, ASTOperation::AST_NONE);
            // let reg_name: String = self.reg_manager.borrow().name(reg);
            // let param_reg: usize = self.reg_manager.borrow_mut().allocate_param_reg();
            // let param_reg_name: String = self.reg_manager.borrow().name(param_reg);
            // println!("mov {}, {}", param_reg_name, reg_name);
        }
        let mut reg_mgr = self.reg_manager.borrow_mut();
        let alloced_reg: usize = reg_mgr.allocate();
        let alloced_reg_name: String = reg_mgr.name(alloced_reg);
        println!("bl _{}", func_info.name);
        println!("mov {}, x0", alloced_reg_name);
        self.function_id = 0xFFFFFFFF;
        alloced_reg
    }

    fn gen_var_assignment_stmt(&mut self, assign_stmt: &crate::ast::AssignStmt, expr_ast: &Expr) {
        // self.gen_load_id_into_reg(assign_stmt.symtbl_pos);
        let expr_reg = self.gen_expr(expr_ast, ASTOperation::AST_ASSIGN, 0xFFFFFFFF, ASTOperation::AST_NONE);
        self.gen_store_reg_value_into_id(expr_reg, assign_stmt.symtbl_pos);
        // let symbol: Symbol = if let Some(ctx_rc) = &self.ctx {
            // let ctx_borrow = ctx_rc.borrow_mut();
            // if let Some(symbol) = ctx_borrow.sym_table.get_symbol(assign_stmt.symtbl_pos) {
                // symbol.clone()
            // } else {
                // panic!("not possible to reach here");
            // }
        // } else {
            // panic!("no context provided");
        // };

    }
}

impl<'aarch64> Aarch64CodeGen<'aarch64> {
    pub fn new(
        reg_manager: RefCell<RegManager>,
    ) -> Self {
        Self {
            reg_manager,
            ctx: None,
            label_id: 0,
            function_id: 0xFFFFFFFF
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

    // ADRP loads the address of the page of given variables. In other words, 
    // ADRP loads the address of the page where the given global identifier lies 
    // on. Doing this alone doesn't give us the address of the global identifier. 
    // We need to add the offset of the global into the page address. 
    // For example, if the page address is 40 and our global's address is 44, 
    // we first load the page address (40) into the register and add the global's 
    // offset (4) into it, creating an PC relative address.
    fn dump_gid_address_load_code_from_label_id(&self, reg_name: &str, id: &LitType) {
        let symbol_label_id: usize = match id {
            LitType::I32(_idx) => *_idx as usize,
            LitType::Str(_, label) => *label,
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
        } 
        else { 
            "0".to_string() 
        };
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

    fn gen_int_value_load_code(value: &LitType, reg_name: &str) -> Result<String, ()> {
        let mut result: String = "".to_string();
        match value {
            LitType::I64(int_val) => {
                let splitted_i64: Vec<String> = i64_hex_split_quarter(to_hex(*int_val));
                if splitted_i64.is_empty() {
                    return Err(());
                }
                result.push_str(&format!("movz {}, 0x{}, lsl #48\n", reg_name, splitted_i64[0]));
                result.push_str(&format!("movk {}, 0x{}, lsl #32\n", reg_name, splitted_i64[1]));
                result.push_str(&format!("movk {}, 0x{}, lsl #16\n", reg_name, splitted_i64[2]));
                result.push_str(&format!("movk {}, 0x{}", reg_name, splitted_i64[3]));
            },
            LitType::I32(int_val) => {
                let splitted_i32: Vec<String> = i32_hex_split_half(to_hex(*int_val));
                if splitted_i32.is_empty() {
                    return Err(());
                }
                result.push_str(&format!("movz {}, 0x{}, lsl #16\n", reg_name, splitted_i32[0]));
                result.push_str(&format!("movk {}, 0x{}", reg_name, splitted_i32[1]));
            }
            LitType::I16(int_val) => {
                result.push_str(&format!("movz {}, {}", reg_name, to_hex(int_val)));
            }
            LitType::U8(u8_val) => {
                result.push_str(&format!("movz {}, {}", reg_name, to_hex(u8_val)));
            }
            _ => {
                return Err(());
            },
        };
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use core::f64;
    use std::cell::RefCell;

    use crate::{code_gen::RegManager, types::LitType};

    use super::Aarch64CodeGen;

    fn create_reg_mgr() -> RegManager {
        let rm: RegManager = RegManager::new(
            {
                let mut regs: Vec<String> = vec![];
                for i in 0..=28 {
                    regs.push(format!("x{}", i));
                }
                regs
            },
            {
                let mut regs: Vec<String> = vec![];
                for i in 0..=7 {
                    regs.push(format!("x{}", i));
                }
                regs
            }
        );
        rm
    }

    #[test]
    fn test_gen_int_value_load_code_i64() {
        // let rm = create_reg_mgr();
        // let cg = Aarch64CodeGen::new(RefCell::new(rm));
        let value = LitType::I64(0x123456789ABCDEF0);
        let result = Aarch64CodeGen::gen_int_value_load_code(&value, "x0").unwrap();
        let expected = 
            "movz x0, 0x1234, lsl #48\n\
            movk x0, 0x5678, lsl #32\n\
            movk x0, 0x9abc, lsl #16\n\
            movk x0, 0xdef0";
        assert_eq!(result, expected);
    }

    #[test]
    fn test_gen_int_value_load_code_i32() {
        let value = LitType::I32(0x12345678);
        let result = Aarch64CodeGen::gen_int_value_load_code(&value, "x0").unwrap();
        let expected = 
            "movz x0, 0x1234, lsl #16\n\
             movk x0, 0x5678";
        assert_eq!(result, expected);
    }

    #[test]
    fn test_gen_int_value_load_code_i16() {
        let value = LitType::I16(0x1234);
        let result = Aarch64CodeGen::gen_int_value_load_code(&value, "w0").unwrap();
        let expected = "movz w0, 0x1234";
        assert_eq!(result, expected);
    }

    #[test]
    fn test_gen_int_value_load_code_u8() {
        let value = LitType::U8(0x12);
        let result = Aarch64CodeGen::gen_int_value_load_code(&value, "w0").unwrap();
        let expected = "movz w0, 0x12";
        assert_eq!(result, expected);
    }

    #[test]
    fn test_gen_int_value_load_code_unsupported_type() {
        let value = LitType::F64(f64::consts::PI);
        let result = Aarch64CodeGen::gen_int_value_load_code(&value, "x0");
        assert!(result.is_err()); // The function should return an error for unsupported types
    }
}