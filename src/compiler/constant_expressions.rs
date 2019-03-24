//MIT License
//
//Copyright (c) 2019 basusamit
//
//Permission is hereby granted, free of charge, to any person obtaining a copy
//of this software and associated documentation files (the "Software"), to deal
//in the Software without restriction, including without limitation the rights
//to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//copies of the Software, and to permit persons to whom the Software is
//furnished to do so, subject to the following conditions:
//
//The above copyright notice and this permission notice shall be included in all
//copies or substantial portions of the Software.
//
//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//SOFTWARE.

// - Here, we want to analyze/evaluate constant expressions.
// Based on LucidExtractor.java, constant expressions are required:
//  Constant Declarations
//

use super::ast::*;
use super::error::ProgramError;
use super::semantic_analysis::*;
use super::cvalue::*;
use super::ast::Expression::*;
use super::token::*;
use num_bigint::*;
use std::str::FromStr;

pub type ConstantExpressionResult = Result<ConstantValue, ProgramError>;

pub struct ConstantExpressionContext<'a> {
    input: &'a str,
    symbols: &'a mut SymbolTable,
    prefix: String,
}

impl<'a> ConstantExpressionContext<'a> {
    pub fn from(input: &'a str, symbols: &'a mut SymbolTable) -> ConstantExpressionContext<'a> {
        ConstantExpressionContext {
            input,
            symbols,
            prefix: String::new(),
        }
    }

    fn integer(&self, n: &Token) -> ConstantExpressionResult {
        // Convert the token into a string
        let number_text = n.text(self.input);
        println!("Number text: {}", number_text);
        // Convert this into a big integer
        let big_int = match number_text.parse::<BigInt>() {
            Ok(n) => n,
            Err(_) => return Err(ProgramError::of("bad parse","biguint")),
        };
        println!("Number as bigint {}", big_int);
        Ok(ConstantValue::from_bigint(&big_int))
    }

    fn number(&self, n: &Number) -> ConstantExpressionResult {
        println!("number: {:?}", n);
        match n {
            Number::IntegerNumber(v) => self.integer(v),
            _ => Err(ProgramError::of("Unsupported cexpr","cexpr unsupported")),
        }
    }

    fn binary_op(&self, lhs: &ConstantValue, rhs: &ConstantValue, func: &Fn(BigInt,BigInt)->BigInt) -> ConstantExpressionResult {
        if !lhs.is_number() || !rhs.is_number() {
            return Err(ProgramError::of("dimension test","Can only add arrays that are 1-dimensional (numbers)"));
        }
        let lhs_val = lhs.as_int();
        let rhs_val = rhs.as_int();
        let result : BigInt = func(lhs_val,rhs_val);
        Ok(ConstantValue::from_bigint(&result))
    }

    fn compare_op(&self, lhs: &ConstantValue, rhs: &ConstantValue, func: &Fn(BigInt,BigInt)->bool) -> ConstantExpressionResult {
        if !lhs.is_number() || !rhs.is_number() {
            return Err(ProgramError::of("dimension test", "Can only compare arrays that are 1-dimensional (numbers)"));
        }
        let lhs_val = lhs.as_int();
        let rhs_val = rhs.as_int();
        Ok(ConstantValue::from_bool(func(lhs_val, rhs_val)))
    }

    fn bitwise_op(&self, lhs: &ConstantValue, rhs: &ConstantValue, func: &Fn(&Bit, &Bit) -> Bit) -> ConstantExpressionResult {
        if lhs.value.shape() != rhs.value.shape() {
            return Err(ProgramError::of("shape mismatch", "Can only apply bitwise operators to arrays of the same dimensional size"));
        }
        Ok(lhs.bitwise(rhs,func))
    }

    fn infix(&self, lhs: &Expression, operator: &Token, rhs: &Expression) -> ConstantExpressionResult {
        let lhs_value = self.constant_expression(lhs)?;
        let rhs_value = self.constant_expression(rhs)?;
        match &operator.kind {
            TokenKind::PLUS => self.binary_op(&lhs_value, &rhs_value, &std::ops::Add::add),
            TokenKind::MINUS => self.binary_op(&lhs_value, &rhs_value, &std::ops::Sub::sub),
            TokenKind::MULTIPLY => self.binary_op(&lhs_value, &rhs_value, &std::ops::Mul::mul),
            TokenKind::DIVIDE => self.binary_op(&lhs_value, &rhs_value, &std::ops::Div::div),
            TokenKind::GT => self.compare_op(&lhs_value, &rhs_value, &|a,b| a>b),
            TokenKind::GE => self.compare_op(&lhs_value, &rhs_value, &|a,b| a>=b),
            TokenKind::EQ => self.compare_op(&lhs_value, &rhs_value, &|a,b| a==b),
            TokenKind::LT => self.compare_op(&lhs_value, &rhs_value, &|a,b| a<b),
            TokenKind::LE => self.compare_op(&lhs_value, &rhs_value, &|a,b| a<=b),
            TokenKind::NEQ => self.compare_op(&lhs_value, &rhs_value, &|a, b|a!=b),
            TokenKind::BITAND => self.bitwise_op(&lhs_value, &rhs_value, &bit_op_and),
            TokenKind::BITOR => self.bitwise_op(&lhs_value, &rhs_value, &bit_op_or),
            TokenKind::BITXNOR => self.bitwise_op(&lhs_value, &rhs_value, &bit_op_xnor),
            TokenKind::BITXOR => self.bitwise_op(&lhs_value, &rhs_value, &bit_op_xor),
            _ => Err(ProgramError::of("unsupported_operation", "infix operator is unsupported")),
        }
    }

    pub fn constant_expression(&self, expr: &Expression) -> ConstantExpressionResult {
        match expr {
            NumberExpression(n) => self.number(n),
            ExpressionGroup(g) => self.constant_expression(g),
            InfixExpression {lhs, operator, rhs} => self.infix(lhs, operator, rhs),

            _ => Err(ProgramError::of("Foo", "Bar"))
        }
    }

    fn module(&mut self, _m: &ModuleBlock) -> SemanticAnalysisResult {
        Ok(())
    }


    fn prefixed_name(& self, name: &String) -> String {
        if self.prefix.len() == 0 {
            name.clone()
        } else {
            self.prefix.clone() + "." + name
        }
    }

    fn constant_declaration(&mut self, c: &ConstantDeclaration) -> SemanticAnalysisResult {
        let value = self.constant_expression(&c.value)?;
        let name = self.prefixed_name(&c.name.text(self.input));
        self.symbols.insert(name, SymbolKind::Constant(value).clone());
        Ok(())
    }

    fn global_statement(&mut self, g: &GlobalStatement) -> SemanticAnalysisResult {
        match g {
            GlobalStatement::StructureDeclaration(s) => Ok(()),
            GlobalStatement::ConstantDeclaration(c) => self.constant_declaration(c),
        }
    }

    fn global(&mut self, g: &GlobalBlock) -> SemanticAnalysisResult{
        self.prefix = g.name.text(self.input);
        for x in &g.statements {
            self.global_statement(x)?;
        }
        self.prefix = String::new();
        Ok(())
    }

    fn source_block(&mut self, b: &SourceBlock) -> SemanticAnalysisResult {
        match b {
            SourceBlock::GlobalBlock(g) => self.global(g),
            SourceBlock::ModuleBlock(m) => self.module(m),
        }
    }

    pub fn source(&mut self, blk: &[SourceBlock]) -> SemanticAnalysisResult {
        for x in blk {
            self.source_block(x)?;
        }
        Ok(())
    }

}