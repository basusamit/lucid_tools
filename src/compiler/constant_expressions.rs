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
use ndarray::prelude::*;
use ndarray::*;
use num_traits::cast::ToPrimitive;
use num_traits::Num;

pub type ConstantExpressionResult = Result<ConstantValue, ProgramError>;

pub struct ConstantExpressionContext<'a> {
    input: &'a str,
    symbols: &'a mut SymbolTable,
    prefix: String,
}

fn split_token_at_base_spec(number_text: String, base_spec: char) -> (usize, Vec<u8>) {
    let number_bytes = number_text.as_bytes();
    let mut width = String::new();
    let mut ptr = 0;
    let mut number_data = vec![];
    while number_bytes[ptr].is_ascii_digit() {
        width.push(number_bytes[ptr] as char);
        ptr = ptr + 1;
    }
    if (number_bytes[ptr] as char).eq(&base_spec) {
        ptr = ptr + 1;
    }
    while ptr < number_bytes.len() {
        number_data.push(number_bytes[ptr]);
        ptr = ptr + 1;
    }
    if width.is_empty() {
        (0, number_data)
    } else {
        (width.parse::<usize>().unwrap(), number_data)
    }
}

fn hex_digit_to_bit_pattern(x: char) -> Vec<Bit> {
    let mut ret = vec![];
    if let Ok(n) = BigInt::from_str_radix(&x.to_string(), 16) {
        let (_, bin_rep) = n.to_radix_be(2);
        for bit in bin_rep {
            match bit {
                0 => ret.push(Bit::Zero),
                1 => ret.push(Bit::One),
                _ => ret.push(Bit::DontCare),
            }
        }
    };
    while ret.len() < 4 {
        ret.insert(0,Bit::Zero);
    }
    ret
}

fn trim_msb_zeros(x: &[Bit]) -> Vec<Bit> {
    x.iter()
        .map(|x| x.clone())
        .skip_while(|x| *x == Bit::Zero)
        .collect::<Vec<Bit>>()
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

    fn adjust_width(&self, data_bits: &[Bit], bit_width: usize) -> ConstantExpressionResult {
        // Trim off the leading zeros
        let mut data_bits_trimmed = trim_msb_zeros(&data_bits);
        if data_bits_trimmed.is_empty() {
            data_bits_trimmed.push(Bit::Zero);
        }
        if data_bits_trimmed.len() > bit_width {
            return Err(ProgramError::of("bit width exceeded","requested constant is larger than given bit width"));
        }
        for _cnt in data_bits_trimmed.len() .. bit_width {
            data_bits_trimmed.push(Bit::Zero);
        }
        Ok(ConstantValue::from_bitvec(&data_bits_trimmed))
    }

    fn hex_number(&self, n: &Token) -> ConstantExpressionResult {
        let number_text = n.text(self.input);
        let (width, number_data) = split_token_at_base_spec(number_text, 'h');
        let mut data_bits = vec![];
        // Loop through the digits in forward order (so the MSB is first)
        for x in number_data.iter() {
            match *x as char {
                // These bit patterns are LSB first
                '0' ... '9' | 'A' ... 'F' | 'a' ... 'f' =>
                    data_bits.extend_from_slice(&hex_digit_to_bit_pattern(*x as char)),
                'X' | 'x' => { data_bits.extend_from_slice(&[Bit::DontCare, Bit::DontCare, Bit::DontCare, Bit::DontCare]); },
                'Z' | 'z' => { data_bits.extend_from_slice(&[Bit::HiZ, Bit::HiZ, Bit::HiZ, Bit::HiZ]); },
                _ => return Err(ProgramError::of("parse error for hex literal","Hex literal is invalid")),
            }
        }
        println!("Width: {}, number_data: {}, data: {:?}", width, String::from_utf8(number_data).unwrap(), data_bits);
        self.adjust_width(&data_bits, width)
    }

    fn binary_number(&self, n: &Token) -> ConstantExpressionResult {
        let number_text = n.text(self.input);
        let (width, number_data) = split_token_at_base_spec(number_text, 'b');
        let mut data_bits = vec![];
        for x in number_data.iter() {
            match *x as char {
                '0' => data_bits.push(Bit::Zero),
                '1' => data_bits.push(Bit::One),
                'X'|'x' => data_bits.push(Bit::DontCare),
                'Z'|'z' => data_bits.push(Bit::HiZ),
                _ => return Err(ProgramError::of("parse error for binary literal", "Binary literal is invalid")),
            }
        }
        self.adjust_width(&data_bits, width)
    }

    fn decimal_number(&self, n: &Token) -> ConstantExpressionResult {
        let number_text = n.text(self.input);
        let (width, number_data) = split_token_at_base_spec(number_text, 'd');
        let data_bits = String::from_utf8(number_data).unwrap().parse::<BigInt>().unwrap();
        let (_, raw_bits) = data_bits.to_radix_be(2);
        let bits = raw_bits.iter()
            .map(|x| match *x {
                0 => Bit::Zero,
                1 => Bit::One,
                _ => Bit::DontCare,
            }).collect::<Vec<Bit>>();
        self.adjust_width(&bits, width)
    }

    fn number(&self, n: &Number) -> ConstantExpressionResult {
        println!("number: {:?}", n);
        match n {
            Number::IntegerNumber(v) => self.integer(v),
            Number::HexNumber(v) => self.hex_number(v),
            Number::BinaryNumber(v) => self.binary_number(v),
            Number::DecimalNumber(v) => self.decimal_number(v),
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

    fn bitwise_op(&self, lhs: &ConstantValue, rhs: &ConstantValue, func: &Fn(Bit, &Bit) -> Bit) -> ConstantExpressionResult {
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

    fn prefix(&self, operator: &Token, operand: &Expression) -> ConstantExpressionResult {
        let op_value = self.constant_expression(operand)?;
        match &operator.kind {
            TokenKind::BITAND => Ok(op_value.fold(Bit::One,&bit_op_and)),
            TokenKind::BITOR => Ok(op_value.fold(Bit::Zero, &bit_op_or)),
            TokenKind::BITXOR => Ok(op_value.fold(Bit::Zero, &bit_op_xor)),
            TokenKind::BITNOT => Ok(op_value.map(bit_op_not)),
            TokenKind::BITNAND => Ok(op_value.fold(Bit::One, &bit_op_and).map(bit_op_not)),
            TokenKind::BITNOR => Ok(op_value.fold(Bit::Zero, &bit_op_or).map(bit_op_not)),
            TokenKind::BITXNOR => Ok(op_value.fold(Bit::Zero, &bit_op_xor).map(bit_op_not)),
            _ => Err(ProgramError::of("unsupported_prefix", "prefix operator is unsupported")),
        }
    }

    fn array(&self, args: &[Box<Expression>]) -> ConstantExpressionResult {
        let mut exprs = vec![];
        for x in args {
            exprs.push(self.constant_expression(&*x)?.value.insert_axis(Axis(0)));
        }
        let mut views = vec![];
        for x in &exprs {
            views.push(x.view());
        }
        let value =  stack(Axis(0),&views);
        if !value.is_ok() {
            return Err(ProgramError::of("Unable to concatenate arrays","{A,B,C} must all have equal size"));
        }
        Ok(ConstantValue {
            value: value.unwrap(),
            signed: Sign::NoSign,
        })
    }

    fn dup(&self, multiplier: &Expression, base: &Expression) -> ConstantExpressionResult {
        let mulv = self.constant_expression(multiplier)?;
        let basev = self.constant_expression(base)?;
        if !mulv.is_number() {
            return Err(ProgramError::of("multiplier must be a number",
                                        "The first argument to the concatenation operator must be a number"));
        }
        let copies = mulv.as_int().to_i32();
        if copies.is_none() {
            return Err(ProgramError::of("multiplier is too big",
                                        "The first argument to the concatenation operator cannot be large"));
        }
        let mut views = vec![];
        for _x in 0..copies.unwrap() {
            views.push(basev.value.view());
        }
        let value = stack(Axis(0),&views);
        if !value.is_ok() {
            return Err(ProgramError::of("Concatenation failed","Unable to duplicate argument"));
        }
        Ok(ConstantValue {
            value: value.unwrap(),
            signed: basev.signed,
        })
    }

    fn concat(&self, expr: &[Box<Expression>]) -> ConstantExpressionResult {
        let mut args = vec![];
        for x in expr {
            args.push(self.constant_expression(x)?);
        }
        let mut views = vec![];
        for x in &args {
            views.push(x.value.view());
        }
        let value = stack(Axis(0), &views);
        if !value.is_ok() {
            return Err(ProgramError::of("Concatenation failed", "Unable to concatenate arguments"));
        }
        Ok(ConstantValue {
            value: value.unwrap(),
            signed: Sign::NoSign,
        })
    }

    fn signal(&self, s: &[NamedBitSelector]) -> ConstantExpressionResult {
        if s.len() == 1 && s[0].selector.arrays.is_empty() && s[0].selector.selector.is_none() {
            let prefixed_name = self.prefixed_name(&s[0].name.text(self.input));
            if let Some(SymbolKind::Constant(val)) = self.symbols.get(&prefixed_name) {
                return Ok(val.clone())
            }
        }
        Err(ProgramError::of("Unsupported signal expression","Constant expression selection unsupported"))
    }

    fn flatten(&self, arguments: &[Box<Expression>]) -> ConstantExpressionResult {
        if arguments.len() != 1 {
            return Err(ProgramError::of("Too many arguments to flatten","flatten only takes one argument"));
        }
        let arg = self.constant_expression(&arguments[0])?;
        Ok(ConstantValue::flatten(&arg))
    }

    fn assert_eq(&self, arguments: &[Box<Expression>]) -> ConstantExpressionResult {
        if arguments.len() != 2 {
            return Err(ProgramError::of("Two arguments to assert_eq","wrong number of arguments"));
        }
        let arg_a = self.constant_expression(&arguments[0])?;
        let arg_b = self.constant_expression(&arguments[1])?;
        if arg_a.ne(&arg_b) {
            println!("{:?} vs {:?}", arg_a, arg_b);
            return Err(ProgramError::of("Assert failed","Assert_eq arguments do not match"));
        }
        Ok(arg_a)
    }

    fn function(&self, name: &Token, arguments: &[Box<Expression>]) -> ConstantExpressionResult {
        let name_string = name.text(&self.input);
        match name_string.as_str() {
            "$flatten" => self.flatten(arguments),
            "$assert_eq" => self.assert_eq(arguments),
            _ => Err(ProgramError::of("unknown function","Unknown function")),
        }
    }

    pub fn constant_expression(&self, expr: &Expression) -> ConstantExpressionResult {
        match expr {
            SignalExpression(s) => self.signal(s),
            NumberExpression(n) => self.number(n),
            ExpressionGroup(g) => self.constant_expression(g),
            InfixExpression {lhs, operator, rhs} => self.infix(lhs, operator, rhs),
            PrefixExpression {operator, operand} => self.prefix(operator,operand),
            ArrayExpression {elements} => self.array(elements),
            DuplicateExpression {multiplier, base} => self.dup(multiplier, base),
            FunctionExpression {name, arguments} => self.function(name, arguments),
            ConcatenateExpression {expressions} => self.concat(expressions),
            _ => {
                println!("{:?}",expr);
                Err(ProgramError::of("Foo", "Bar"))
            }
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
            GlobalStatement::StructureDeclaration(_s) => Ok(()),
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
