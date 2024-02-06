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

extern crate lazy_static;
use std::{cell::RefCell, rc::Rc};

use crate::{enums::*, types::*, register::{self, RegisterManager}, symtable};

lazy_static::lazy_static! {
    static ref CMP_CONDS_LIST: Vec<&'static str> = vec!["neq", "eq", "ge", "le", "lt", "gt"];
}

#[derive(Debug, Clone)]
pub struct ASTNode {
    pub operation: ASTNodeKind, // what type of operation to be performed on this node
    pub left: Box<Option<ASTNode>>,
    pub right: Box<Option<ASTNode>>,
    pub mid: Box<Option<ASTNode>>,
    pub value: Option<LitType>, // on what value the operation is going to be performed
    pub result_type: LitType, // what type of result the evaulation of this ASTNode will produce
}

impl ASTNode {
    pub fn new(op: ASTNodeKind, left: Option<ASTNode>, right: Option<ASTNode>, value: Option<LitType>, result_type: LitType) -> Self {
        Self { 
            operation: op, 
            left: Box::new(left), 
            right: Box::new(right), 
            mid: Box::new(None), 
            value,
            result_type
        }
    }

    pub fn make_leaf(op: ASTNodeKind, value: LitType, result_type: LitType) -> Self {
        Self { 
            operation: op, 
            left: Box::new(None), 
            right: Box::new(None), 
            mid: Box::new(None), 
            value: Some(value),
            result_type
        }
    }

    pub fn with_mid(op: ASTNodeKind, left: Option<ASTNode>, right: Option<ASTNode>, mid: Option<ASTNode>, value: Option<LitType>, result_type: LitType) -> Self {
        Self { 
            operation: op, 
            left: Box::new(left), 
            right: Box::new(right), 
            mid: Box::new(mid), 
            value,
            result_type
        }
    }
}

pub struct ASTTraverser {
    reg_manager: Rc<RefCell<register::RegisterManager>>,
    sym_table: Rc<RefCell<symtable::Symtable>>,
    label_id_count: usize,
}

impl ASTTraverser {
    #[allow(clippy::new_without_default)]
    pub fn new(reg_manager: Rc<RefCell<RegisterManager>>, sym_table: Rc<RefCell<symtable::Symtable>>) -> Self {
        Self { reg_manager, sym_table, label_id_count: 0 }
    }

    pub fn traverse(&mut self, ast: &ASTNode) -> usize {
        self.gen_ast(ast, 0xFFFFFFFF, ast.operation)
    }

    fn gen_ast(&mut self, ast: &ASTNode, _reg: usize, parent_ast_kind: ASTNodeKind) -> usize {
        if ast.operation == ASTNodeKind::AST_IF {
            return self.gen_ifstmt_ast(ast);
        } else if ast.operation == ASTNodeKind::AST_WHILE {
            return self.gen_while_stmt(ast);
        } else if ast.operation == ASTNodeKind::AST_FUNCTION {
            return self.gen_function_stmt(ast); 
        } else if ast.operation == ASTNodeKind::AST_GLUE {
            if let Some(left) = ast.left.as_ref() {
                self.gen_ast(left, 0xFFFFFFFF, ast.operation);
                self.reg_manager.borrow_mut().deallocate_all();
            }
            if let Some(right) = ast.right.as_ref() {
                self.gen_ast(right, 0xFFFFFFFF, ast.operation);
                self.reg_manager.borrow_mut().deallocate_all();
            }
            return 0xFFFFFFFF;
        }
        let mut leftreg: usize = 0;
        let mut rightreg: usize = 0;
        if let Some(leftt) = &*ast.left { // take reference to Option<T> which is inside the Box<Option<T>>
            leftreg = self.gen_ast(leftt, 0xFFFFFFFF, ast.operation);
        }
        if let Some(rightt) = ast.right.as_ref() {
            rightreg = self.gen_ast(rightt, leftreg, ast.operation);
        }
        match ast.operation {
            ASTNodeKind::AST_ADD => self.gen_add(leftreg, rightreg),
            ASTNodeKind::AST_SUBTRACT => self.gen_sub(leftreg, rightreg),
            ASTNodeKind::AST_INTLIT => self.gen_load_intlit_into_reg(ast.value.as_ref().unwrap()),
            ASTNodeKind::AST_IDENT => self.gen_load_gid_into_reg(ast.value.as_ref().unwrap()),
            ASTNodeKind::AST_LVIDENT => self.gen_load_reg_into_gid(_reg, ast.value.as_ref().unwrap()),
            ASTNodeKind::AST_ASSIGN => rightreg,
            ASTNodeKind::AST_GTHAN | 
            ASTNodeKind::AST_LTHAN | 
            ASTNodeKind::AST_LTEQ |
            ASTNodeKind::AST_GTEQ |
            ASTNodeKind::AST_NEQ |
            ASTNodeKind::AST_EQEQ => {
                if (parent_ast_kind == ASTNodeKind::AST_IF) || (parent_ast_kind == ASTNodeKind::AST_WHILE) {
                    self.gen_cmp_and_jmp(ast.operation, leftreg, rightreg, _reg)
                } else {
                    self.gen_cmp_and_set(ast.operation, leftreg, rightreg)
                }
            },
            _ => panic!("unknown AST operator '{:?}'", ast.operation),
        }
    }

    fn gen_function_stmt(&mut self, ast: &ASTNode) -> usize {
        let index: usize = match ast.value.as_ref().unwrap() {
            LitType::I32(int_idx) => *int_idx as usize,
            _ => panic!("Not a valid symbol table indexing method")
        };
        println!("{}:", self.sym_table.borrow().get_symbol(index).name);
        if let Some(body) = &*ast.left {
            self.gen_ast(body, 0xFFFFFFFF, ast.operation);
        }
        println!("mov x0, 0");
        0xFFFFFFFF
    }

    fn gen_while_stmt(&mut self, ast: &ASTNode) -> usize {
        let label_start: usize = self.get_next_label();
        let label_end: usize = self.get_next_label();
        self.gen_label(label_start); // start of loop body
        self.gen_ast((*ast.left).as_ref().unwrap(), label_end, ast.operation);
        self.reg_manager.borrow_mut().deallocate_all();
        self.gen_ast((*ast.right).as_ref().unwrap(), 0xFFFFFFFF, ast.operation);
        self.reg_manager.borrow_mut().deallocate_all();
        self.gen_jump(label_start);
        self.gen_label(label_end);
        0xFFFFFFFF
    }

    fn gen_ifstmt_ast(&mut self, ast: &ASTNode) -> usize {
        let label_if_false: usize = self.get_next_label(); // label id to jump to if condition turns out to be false
        let mut label_end: usize = 0xFFFFFFFF; // this label is put after the end of entire if-else block
        if ast.right.is_some() { label_end = self.get_next_label(); }
        self.gen_ast((*ast.left).as_ref().unwrap(), label_if_false, ast.operation);
        self.reg_manager.borrow_mut().deallocate_all();
        self.gen_ast((*ast.mid).as_ref().unwrap(), 0xFFFFFFFF, ast.operation);
        self.reg_manager.borrow_mut().deallocate_all();
        // if there is an 'else' block
        if ast.right.is_some() { self.gen_jump(label_end); }
        // false label
        self.gen_label(label_if_false);
        if let Some(right_ast) = &*ast.right {
            self.gen_ast(right_ast, 0xFFFFFFFF, ast.operation);
            self.reg_manager.borrow_mut().deallocate_all();
            self.gen_label(label_end);
        }
        0xFFFFFFFF
    }

    fn gen_cmp_and_set(&self, operation: ASTNodeKind, r1: usize, r2: usize) -> usize {
        let r1name: String = self.reg_manager.borrow().name(r1);
        let r2name: String = self.reg_manager.borrow().name(r2);
        println!("cmp {}, {}", r1name, r2name);
        println!("cset {}, {}", r2name, CMP_CONDS_LIST[operation as usize - ASTNodeKind::AST_EQEQ as usize]);
        println!("and {}, {}, 255", r2name, r2name);
        r2
    }

    fn gen_cmp_and_jmp(&self, operation: ASTNodeKind, r1: usize, r2: usize, label: usize) -> usize {
        let r1name: String = self.reg_manager.borrow().name(r1);
        let r2name: String = self.reg_manager.borrow().name(r2);
        println!("cmp {}, {}", r1name, r2name);
        println!("b{} _L{}", CMP_CONDS_LIST[operation as usize - ASTNodeKind::AST_EQEQ as usize], label);
        self.reg_manager.borrow_mut().deallocate_all();
        0xFFFFFFFF
    }

    fn gen_add(&mut self, r1: usize, r2: usize) -> usize {
        println!("add {}, {}, {}", self.reg_manager.borrow().name(r1), self.reg_manager.borrow().name(r1), self.reg_manager.borrow().name(r2));
        self.reg_manager.borrow_mut().deallocate(r2);
        r1
    }
    
    fn gen_sub(&mut self, r1: usize, r2: usize) -> usize {
        println!("sub {}, {}, {}", self.reg_manager.borrow().name(r1), self.reg_manager.borrow().name(r1), self.reg_manager.borrow().name(r2));
        self.reg_manager.borrow_mut().deallocate(r2);
        r1
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

    // Load value from a variable into a register.
    // Return the number of the register
    fn gen_load_gid_into_reg(&mut self, id: &LitType) -> usize {
        let reg: usize = self.reg_manager.borrow_mut().allocate();
        let reg_name: String = self.reg_manager.borrow().name(reg);
        let value_containing_reg: usize = self.reg_manager.borrow_mut().allocate();
        let value_containing_reg_name: String = self.reg_manager.borrow().name(value_containing_reg);
        let sym: String = match id {
            LitType::I32(int_id) => self.sym_table.borrow().get_symbol(*int_id as usize).name.clone(),
            LitType::String(_id) => _id.clone(),
            _ => String::from(""),
        };
        println!("ldr {}, ={}\nldr {}, [{}]\n", reg_name, sym, value_containing_reg_name, reg_name);
        value_containing_reg
    }

    fn gen_load_reg_into_gid(&mut self, reg: usize, id: &LitType) -> usize {
        let reg_name: String = self.reg_manager.borrow().name(reg);
        let mut offset: usize = 0;
        match id {
            LitType::I32(index) => {
                for (idx, symbol) in self.sym_table.borrow().iter().enumerate() {
                    if idx == *index as usize { break; }
                    if symbol.sym_type == SymbolType::Variable {
                        offset += 4;
                    }
                }
            },
            _ => panic!("Not supported indexing type!")
        };
        let addr_reg: usize = self.reg_manager.borrow_mut().allocate();
        let addr_reg_name: String = self.reg_manager.borrow().name(addr_reg);
        println!("adrp {}, .L2+{}@PAGE", addr_reg_name, offset);
        println!("add {}, {}, .L2+{}@PAGEOFF", &addr_reg_name, addr_reg_name, offset);
        println!("str {}, [{}]", reg_name, addr_reg_name);
        addr_reg
    }

    fn get_next_label(&mut self) -> usize {
        let label: usize = self.label_id_count;
        self.label_id_count += 1;
        label
    }

    fn gen_jump(&self, label_id: usize) {
        println!("b _L{}", label_id);
    }

    fn gen_label(&mut self, label: usize) {
        println!("_L{}:", label);
    }
}