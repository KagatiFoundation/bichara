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

use lazy_static::lazy_static;

use crate::{enums::*, register::{self, RegisterManager}};

lazy_static! {
    // All available non-extended registers of ARM64
    static ref REGISTERS: Vec<&'static str> = vec!["w8", "w9", "w10", "w11", "w12", "w13", "w14", "w15", "w16"];
}

#[derive(Debug)]
pub struct ASTNode {
    pub operation: ASTNodeKind, // operation to be performed on this AST node
    pub left: Option<Box<ASTNode>>,
    pub right: Option<Box<ASTNode>>,
    pub value: LitType
}

impl ASTNode {
    pub fn new(op: ASTNodeKind, left: ASTNode, right: ASTNode, value: LitType) -> Self {
        Self { operation: op, left: Some(Box::new(left)), right: Some(Box::new(right)), value }
    }

    pub fn make_leaf(op: ASTNodeKind, value: LitType) -> Self {
        Self { operation: op, left: None, right: None, value }
    }
}

pub struct ASTTraverser {
    reg_manager: Rc<RefCell<register::RegisterManager>>
}

impl<'a> ASTTraverser {
    #[allow(clippy::new_without_default)]
    pub fn new(reg_manager: Rc<RefCell<RegisterManager>>) -> Self {
        Self { reg_manager }
    }

    pub fn traverse(&mut self, ast: &ASTNode) -> usize {
        self.gen_from_ast(ast, 0xFFFFFFFF)
    }

    fn gen_from_ast(&mut self, ast: &ASTNode, _reg: usize) -> usize {
        let mut leftreg: usize = 0;
        let mut rightreg: usize = 0;
        if ast.left.is_some() {
            leftreg = self.gen_from_ast(ast.left.as_ref().unwrap(), 0xFFFFFFFF);
        }
        if ast.right.is_some() {
            rightreg = self.gen_from_ast(ast.right.as_ref().unwrap(), leftreg);
        }
        match ast.operation {
            ASTNodeKind::AST_ADD => self.gen_add(leftreg, rightreg),
            ASTNodeKind::AST_SUBTRACT => self.gen_sub(leftreg, rightreg),
            ASTNodeKind::AST_INTLIT => self.gen_load_intlit_into_reg(&ast.value),
            _ => panic!("unknown AST operator '{:?}'", ast.operation),
        }
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
    fn _gen_load_from_global(&mut self, id: &LitType) -> usize {
        let reg: usize = self.reg_manager.borrow_mut().allocate();
        let reg_name = self.reg_manager.borrow().name(reg);
        let sym: String = match id {
            LitType::String(_id) => _id.clone(),
            _ => String::from(""),
        };
        println!("adr {}, ={}\nmov {}, [{}]\n", reg_name, sym, reg_name, reg_name);
        reg
    }
}