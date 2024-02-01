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

use crate::{enums::*, register::{self, RegisterManager}, symtable};

lazy_static::lazy_static! {
    static ref CMP_CONDS_LIST: Vec<&'static str> = vec!["neq", "eq", "ge", "le", "lt", "gt"];
}

#[derive(Debug, Clone)]
pub struct ASTNode {
    pub operation: ASTNodeKind, // operation to be performed on this AST node
    pub left: Option<Box<ASTNode>>,
    pub right: Option<Box<ASTNode>>,
    pub mid: Option<Box<ASTNode>>,
    pub value: LitType,
}

impl ASTNode {
    pub fn new(op: ASTNodeKind, left: ASTNode, right: ASTNode, value: LitType) -> Self {
        Self { operation: op, left: Some(Box::new(left)), right: Some(Box::new(right)), mid: None, value }
    }

    pub fn make_leaf(op: ASTNodeKind, value: LitType) -> Self {
        Self { operation: op, left: None, right: None, mid: None, value }
    }

    pub fn make_with_mid(op: ASTNodeKind, left: ASTNode, right: ASTNode, mid: ASTNode, value: LitType) -> Self {
        Self { operation: op, left: Some(Box::new(left)), right: Some(Box::new(right)), mid: Some(Box::new(mid)), value }
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
        } else if ast.operation == ASTNodeKind::AST_GLUE {
            if let Some(left) = &ast.left {
                self.gen_ast(left, 0xFFFFFFFF, ast.operation);
                self.reg_manager.borrow_mut().deallocate_all();
            }
            if let Some(right) = &ast.right {
                self.gen_ast(right, 0xFFFFFFFF, ast.operation);
                self.reg_manager.borrow_mut().deallocate_all();
            }
        }

        let mut leftreg: usize = 0;
        let mut rightreg: usize = 0;
        if ast.left.is_some() {
            leftreg = self.gen_ast(ast.left.as_ref().unwrap(), 0xFFFFFFFF, ast.operation);
        }
        if ast.right.is_some() {
            rightreg = self.gen_ast(ast.right.as_ref().unwrap(), leftreg, ast.operation);
        }
        match ast.operation {
            ASTNodeKind::AST_ADD => self.gen_add(leftreg, rightreg),
            ASTNodeKind::AST_SUBTRACT => self.gen_sub(leftreg, rightreg),
            ASTNodeKind::AST_INTLIT => self.gen_load_intlit_into_reg(&ast.value),
            ASTNodeKind::AST_IDENT => self.gen_load_gid_into_reg(&ast.value),
            ASTNodeKind::AST_ASSIGN => rightreg,
            ASTNodeKind::AST_GTHAN | 
            ASTNodeKind::AST_LTHAN | 
            ASTNodeKind::AST_LTEQ |
            ASTNodeKind::AST_GTEQ |
            ASTNodeKind::AST_NEQ |
            ASTNodeKind::AST_EQEQ => {
                if parent_ast_kind == ASTNodeKind::AST_IF {
                    self.gen_cmp_and_jmp(ast.operation, leftreg, rightreg, _reg)
                } else {
                    0xFFFFFFFF
                }
            },
            _ => panic!("unknown AST operator '{:?}'", ast.operation),
        }
    }

    fn gen_ifstmt_ast(&mut self, ast: &ASTNode) -> usize {
        let label_if_false: usize = self.get_next_label(); // label id to jump to if condition turns out to be false
        let mut label_end: usize = 0xFFFFFFFF; // this label is put after the end of entire if-else block
        if ast.right.is_some() { label_end = self.get_next_label(); }
        self.gen_ast(ast.left.as_ref().unwrap(), label_if_false, ast.operation);
        self.reg_manager.borrow_mut().deallocate_all();
        self.gen_ast(ast.mid.as_ref().unwrap(), 0xFFFFFFFF, ast.operation);
        self.reg_manager.borrow_mut().deallocate_all();
        // if there is an 'else' block
        if ast.right.is_some() { self.gen_jump(label_end); }
        // false label
        self.gen_label(label_if_false);
        if let Some(right_ast) = &ast.right {
            self.gen_ast(right_ast, 0xFFFFFFFF, ast.operation);
            self.reg_manager.borrow_mut().deallocate_all();
            self.gen_label(label_end);
        }
        0xFFFFFFFF
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
        println!("add {}, {}, {}\n", self.reg_manager.borrow().name(r1), self.reg_manager.borrow().name(r1), self.reg_manager.borrow().name(r2));
        self.reg_manager.borrow_mut().deallocate(r2);
        r1
    }
    
    fn gen_sub(&mut self, r1: usize, r2: usize) -> usize {
        println!("sub {}, {}, {}\n", self.reg_manager.borrow().name(r1), self.reg_manager.borrow().name(r1), self.reg_manager.borrow().name(r2));
        self.reg_manager.borrow_mut().deallocate(r2);
        r1
    }

    // Load a integer literal into a register
    fn gen_load_intlit_into_reg(&mut self, value: &LitType) -> usize {
        let r: usize = self.reg_manager.borrow_mut().allocate();
        match value {
            LitType::Integer(int_val) => println!("mov {}, {}", self.reg_manager.borrow().name(r), int_val),
            _ => println!("mov {}, {:?}", self.reg_manager.borrow().name(r), value),
        }
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
            LitType::Integer(int_id) => String::from(self.sym_table.borrow().get(*int_id as usize)),
            LitType::String(_id) => _id.clone(),
            _ => String::from(""),
        };
        println!("ldr {}, ={}\nldr {}, [{}]\n", reg_name, sym, value_containing_reg_name, reg_name);
        value_containing_reg
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