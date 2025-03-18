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

use kagc_ast::{ASTOperation, BinExpr, Expr, TYPE_PRECEDENCE_EXPR};
use kagc_symbol::{Symbol, SymbolType};
use kagc_types::{is_type_coalescing_possible, LitTypeVariant};

use crate::{errors::{SAError, SATypeError}, typedefs::SAResult};

pub struct TypeChecker;

impl TypeChecker {
    /// Performs type checking for variable declaration statements.
    /// 
    /// This function infers the type of an expression assigned to a variable,
    /// updates the variable's symbol if it was declared without an explicit 
    /// type, and ensures the assigned expression's type matches the declared or 
    /// inferred type. If there's a type mismatch that cannot be reconciled, an 
    /// error is returned.
    pub fn type_check_var_decl_stmt(var_decl_sym: &mut Symbol, expr: &Expr) -> SAResult {
        let expr_res_type: LitTypeVariant = Self::infer_type(expr)?;

        // This has do be done because variables might be defined without 
        // explicit type annotation. For example, 'let a = 5;'. Thus, the 
        // symbol table has to be updated with this new type information.
        if var_decl_sym.lit_type == LitTypeVariant::None {
            var_decl_sym.lit_type = {
                match expr_res_type {
                    // implicitly convert no-type-annotated byte-type into an integer
                    LitTypeVariant::U8 => LitTypeVariant::I32,
                    _ => expr_res_type
                }
            }
        }
        if 
            var_decl_sym.lit_type != expr_res_type 
            && !is_type_coalescing_possible(expr_res_type, var_decl_sym.lit_type) 
        {
            return Err(SAError::TypeError(
                SATypeError::AssignmentTypeMismatch { 
                    expected: var_decl_sym.lit_type, 
                    found: expr.result_type() 
                }
            ));
        }
        Ok(())
    }

    /// this fucktion is bullshit. do not use
    /// IGNORE
    pub fn type_check_arr_var_decl_stmt(sym: &mut Symbol, vals: &Vec<Expr>) -> SAResult {
        for expr in vals {
            if expr.result_type() != sym.lit_type 
                && !is_type_coalescing_possible(expr.result_type(), sym.lit_type) {
                    return Err(SAError::TypeError(
                        SATypeError::AssignmentTypeMismatch { 
                            expected: sym.lit_type, 
                            found: expr.result_type() 
                        })
                    );
                }
        }
        if sym.lit_type == LitTypeVariant::None && !vals.is_empty() {
            let array_type: LitTypeVariant = Self::infer_type(&vals[0])?;
            sym.lit_type = array_type;
        }
        Ok(())
    }

    pub fn is_bin_expr_type_compatibile(bin_expr: &BinExpr) -> bool {
        let left_type: LitTypeVariant = bin_expr.left.result_type();
        let right_type: LitTypeVariant = bin_expr.right.result_type();
        match bin_expr.operation {
            ASTOperation::AST_ADD 
            | ASTOperation::AST_SUBTRACT
            | ASTOperation::AST_MULTIPLY => {
                if left_type.is_int_variant() && right_type.is_int_variant() {
                    return TypeChecker::is_type_coalesciable(left_type, right_type) 
                            || TypeChecker::is_type_coalesciable(right_type, left_type);
                }
                false
            },
            _ => false
        }
    }

    pub fn is_type_coalesciable(src: LitTypeVariant, dest: LitTypeVariant) -> bool {
        match src {
            LitTypeVariant::U8 => matches!(dest, LitTypeVariant::U8 | LitTypeVariant::I16 | LitTypeVariant::I32 | LitTypeVariant::I64),
            LitTypeVariant::I16 => matches!(dest, LitTypeVariant::I16 | LitTypeVariant::I32 | LitTypeVariant::I64),
            LitTypeVariant::I32 => matches!(dest, LitTypeVariant::I32 | LitTypeVariant::I64),
            _ => false
        }
    }

    pub fn is_callable(sym: &Symbol) -> bool {
        sym.sym_type == SymbolType::Function
    }

    pub fn infer_type(expr: &Expr) -> Result<LitTypeVariant, SAError> {
        return match expr {
            Expr::LitVal(lit_val_expr) => Ok(lit_val_expr.result_type),
            Expr::Ident(ident_expr) => Ok(ident_expr.result_type),
            Expr::FuncCall(func_call_expr) => Ok(func_call_expr.result_type),
            Expr::Binary(bin_expr) => {
                let left_type: LitTypeVariant = TypeChecker::infer_type(&bin_expr.left)?;
                let right_type: LitTypeVariant = TypeChecker::infer_type(&bin_expr.right)?;
                for typ in [&left_type, &right_type] {
                    match typ {
                        LitTypeVariant::Str 
                        | LitTypeVariant::Array
                        | LitTypeVariant::Null
                        | LitTypeVariant::Void => {
                            return Err(
                                SAError::TypeError(SATypeError::IncompatibleTypes { 
                                    a: left_type, 
                                    b: right_type,
                                    operation: bin_expr.operation 
                                })
                            )
                        },
                        _ => ()
                    };
                }
                if left_type != right_type {
                    let lprec: Option<&u8> = TYPE_PRECEDENCE_EXPR.get(&(left_type as u8));
                    let rprec: Option<&u8> = TYPE_PRECEDENCE_EXPR.get(&(right_type as u8));
                    let lp: u8 = if let Some(lp) = lprec {
                        *lp
                    } else {
                        panic!("Type precedence not defined for operation {:?}", left_type);
                    };
                    let rp: u8 = if let Some(rp) = rprec {
                        *rp
                    } else {
                        panic!("Type precedence not defined for operation {:?}", right_type);
                    };
                    if lp > rp {
                        return Ok(left_type);
                    } else {
                        return Ok(right_type);
                    }
                }
                Ok(left_type)
            }
            _ => Err(SAError::None)
        };
    }
}