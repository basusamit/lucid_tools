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
use num_traits::identities::Zero;

pub type ConstantExpressionResult = Result<ConstantValue, ProgramError>;

pub struct ConstantExpressionContext<'a> {
    input: &'a str,
    symbols: &'a mut SymbolTable,
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
        // Assume that integers are unsigned unless they are negative
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
            data_bits_trimmed.insert(0,Bit::Zero);
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
                '0' ..= '9' | 'A' ..= 'F' | 'a' ..= 'f' =>
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

    fn binary_op(&self, lhs: &ConstantValue, rhs: &ConstantValue, func: &dyn Fn(BigInt, BigInt) -> BigInt) -> ConstantExpressionResult {
        if !lhs.is_number() || !rhs.is_number() {
            return Err(ProgramError::of("dimension test","Can only add arrays that are 1-dimensional (numbers)"));
        }
        let lhs_val = lhs.as_int();
        let rhs_val = rhs.as_int();
        let result : BigInt = func(lhs_val,rhs_val);
        Ok(ConstantValue::from_bigint(&result))
    }

    fn compare_op(&self, lhs: &ConstantValue, rhs: &ConstantValue, func: &dyn Fn(BigInt, BigInt) -> bool) -> ConstantExpressionResult {
        if !lhs.is_number() || !rhs.is_number() {
            return Err(ProgramError::of("dimension test", "Can only compare arrays that are 1-dimensional (numbers)"));
        }
        let lhs_val = lhs.as_int();
        let rhs_val = rhs.as_int();
        Ok(ConstantValue::from_bool(func(lhs_val, rhs_val)))
    }

    fn bitwise_op(&self, lhs: &ConstantValue, rhs: &ConstantValue, func: &dyn Fn(Bit, &Bit) -> Bit) -> ConstantExpressionResult {
        if lhs.value.shape() != rhs.value.shape() {
            return Err(ProgramError::of("shape mismatch", "Can only apply bitwise operators to arrays of the same dimensional size"));
        }
        Ok(lhs.bitwise(rhs,func))
    }

    fn infix(&self, lhs: &Expression, operator: &InfixOperator, rhs: &Expression) -> ConstantExpressionResult {
        let lhs_value = self.constant_expression(lhs)?;
        let rhs_value = self.constant_expression(rhs)?;
        match operator {
            InfixOperator::Add => self.binary_op(&lhs_value, &rhs_value, &std::ops::Add::add),
            InfixOperator::Subtract => self.binary_op(&lhs_value, &rhs_value, &std::ops::Sub::sub),
            InfixOperator::Multiply => self.binary_op(&lhs_value, &rhs_value, &std::ops::Mul::mul),
            InfixOperator::Divide => self.binary_op(&lhs_value, &rhs_value, &std::ops::Div::div),
            InfixOperator::GreaterThan => self.compare_op(&lhs_value, &rhs_value, &|a, b| a > b),
            InfixOperator::GreaterEquals => self.compare_op(&lhs_value, &rhs_value, &|a, b| a >= b),
            InfixOperator::Equals => self.compare_op(&lhs_value, &rhs_value, &|a, b| a == b),
            InfixOperator::LessThan => self.compare_op(&lhs_value, &rhs_value, &|a, b| a < b),
            InfixOperator::LessEquals => self.compare_op(&lhs_value, &rhs_value, &|a, b| a <= b),
            InfixOperator::NotEquals => self.compare_op(&lhs_value, &rhs_value, &|a, b| a != b),
            InfixOperator::BitAND => self.bitwise_op(&lhs_value, &rhs_value, &bit_op_and),
            InfixOperator::BitOR => self.bitwise_op(&lhs_value, &rhs_value, &bit_op_or),
            InfixOperator::BitXNOR => self.bitwise_op(&lhs_value, &rhs_value, &bit_op_xnor),
            InfixOperator::BitXOR => self.bitwise_op(&lhs_value, &rhs_value, &bit_op_xor),
            InfixOperator::LeftShift => Ok(lhs_value.left_shift(rhs_value.as_int().to_usize().unwrap())),
            InfixOperator::RightShift => Ok(lhs_value.right_shift(rhs_value.as_int().to_usize().unwrap())),
            InfixOperator::LeftSignedShift => Ok(lhs_value.left_shift(rhs_value.as_int().to_usize().unwrap())),
            InfixOperator::RightSignedShift => Ok(lhs_value.right_shift(rhs_value.as_int().to_usize().unwrap())),
            InfixOperator::LogicalAnd => self.bitwise_op(&lhs_value.fold(Bit::Zero, &bit_op_or),
                                                         &rhs_value.fold(Bit::Zero, &bit_op_or),
                                                         &bit_op_and),
            InfixOperator::LogicalOr => self.bitwise_op(&lhs_value.fold(Bit::Zero, &bit_op_or),
                                                        &rhs_value.fold(Bit::Zero, &bit_op_or),
                                                        &bit_op_or),
        }
    }

    fn prefix(&self, operator: &PrefixOperator, operand: &Expression) -> ConstantExpressionResult {
        let op_value = self.constant_expression(operand)?;
        match operator {
            PrefixOperator::And => Ok(op_value.fold(Bit::One,&bit_op_and)),
            PrefixOperator::Or => Ok(op_value.fold(Bit::Zero, &bit_op_or)),
            PrefixOperator::Xor => Ok(op_value.fold(Bit::Zero, &bit_op_xor)),
            PrefixOperator::Not => Ok(op_value.map(bit_op_not)),
            PrefixOperator::Nand => Ok(op_value.fold(Bit::One, &bit_op_and).map(bit_op_not)),
            PrefixOperator::Nor | PrefixOperator::LogicalNot => Ok(op_value.fold(Bit::Zero, &bit_op_or).map(bit_op_not)),
            PrefixOperator::Xnor => Ok(op_value.fold(Bit::Zero, &bit_op_xor).map(bit_op_not)),
            PrefixOperator::Negate => Ok(op_value.negate()),
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
            signed: Sign::Plus,
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
            signed: Sign::Plus,
        })
    }

    fn ternary(&self, selector: &Expression, first: &Expression, second: &Expression) -> ConstantExpressionResult {
        let select = self.constant_expression(selector)?;
        if select.as_int().is_zero() {
            self.constant_expression(second)
        } else {
            self.constant_expression(first)
        }

    }

    fn signal_base(&self, name: &String) -> ConstantExpressionResult {
        let prefixed_name = self.symbols.prefixed_name(name);
        if let Some(SymbolKind::Constant(val)) = self.symbols.get(&prefixed_name) {
            return Ok(val.clone());
        } else {
            return Err(ProgramError::of("No signal found", &prefixed_name));
        }
    }

    fn signal_apply_arrays(&self, base: &ConstantValue, arrays: &[Box<Expression>]) -> ConstantExpressionResult {
        let mut cv = base.clone();
        for index in arrays {
            let ndx = self.constant_expression(index)?;
            cv = cv.slice(ndx.as_int());
        }
        println!("cv: {:?}\n", cv);
        Ok(cv)
    }

    fn signal_apply_const_bit_selector(&self, base: &ConstantValue, start: &Expression, stop: &Expression) -> ConstantExpressionResult {
        let mut cv = base.clone();
        let start_val = self.constant_expression(start)?.as_int().to_u32().unwrap();
        let stop_val = self.constant_expression(stop)?.as_int().to_u32().unwrap();
        cv = cv.range(start_val, stop_val);
        Ok(cv)
    }

    fn signal_apply_fixed_width(&self, base: &ConstantValue, start: &Expression, sign: &Token, width: &Expression) -> ConstantExpressionResult {
        let mut cv = base.clone();
        let start_val = self.constant_expression(start)?.as_int().to_u32().unwrap();
        let width_val = self.constant_expression(width)?.as_int().to_u32().unwrap();
        println!("{:?}", sign);
        match sign.kind {
            TokenKind::PLUSCOLON => cv = cv.range(start_val, start_val + width_val - 1),
            TokenKind::NEGCOLON => cv = cv.range(start_val - width_val + 1, start_val),
            _ => return Err(ProgramError::of("unknown fixed width selection type","blah")),
        }
        Ok(cv)
    }

    fn signal_apply_bitselector(&self, base: &ConstantValue, select: &Option<BitSelector>) -> ConstantExpressionResult {
        if let Some(bitselector) = select {
            match bitselector {
                BitSelector::ConstBitSelector {start, stop} =>
                    return self.signal_apply_const_bit_selector(base, start, stop),
                BitSelector::FixedWidthBitSelector {start, sign, width} =>
                    return self.signal_apply_fixed_width(base, start, sign, width),
            }
        }
        Ok(base.clone())
    }

    fn signal_apply_selector(&self, base: &ConstantValue, select: &BitSelection) -> ConstantExpressionResult {
        // First apply the arrays
        let base_arrays = self.signal_apply_arrays(base, &select.arrays)?;
        // Then apply the bit selector
        let base_selected = self.signal_apply_bitselector(&base_arrays, &select.selector)?;
        Ok(base_selected)
    }

    // We want to look up a signal (this is for a rhs only)
    // We start with the signal itself, and then apply the various
    // bit and name selections.  The first lookup is a name, to give
    // us the entry in the symbol table.
    //    We then have either a name or a bit selection
    //    Each of these can be the last or not-last in the list
    //    If it is not the last, then the bit selection must be unique - i.e., point
    //      to a single entry - that means all array indices must be constant values
    //      no bit selectors allowed.
    fn signal(&self, s: &[NamedBitSelector]) -> ConstantExpressionResult {
        if s.len() == 0 {
            return Err(ProgramError::of("Invalid signal expression", "Signal expression is not valid"));
        }
        // Get the base value from the symbol table
        let mut signal_name = s[0].name.text(self.input).clone();
        // Check for foo.bar
        let mut selectors = &s[0].selector;
        if s[0].selector.arrays.is_empty() && s.len() > 1 {
            signal_name = signal_name + &"." + &s[1].name.text(self.input);
            selectors = &s[1].selector;
        }
        let mut base = self.signal_base(&signal_name)?;
        if base.is_empty() {
            return Err(ProgramError::of("Symbol is not defined", &(String::from("Symbol used before definition:")
            + &signal_name)));
        }
        base = self.signal_apply_selector(&base, selectors)?;
        Ok(base)
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
        //println!("{:?}", expr);
        match expr {
            SignalExpression(s) => self.signal(s),
            NumberExpression(n) => self.number(n),
            ExpressionGroup(g) => self.constant_expression(g),
            InfixExpression {lhs, operator, operator_token: _, rhs} => self.infix(lhs, operator, rhs),
            PrefixExpression {operator, operator_token: _, operand} => self.prefix(operator,operand),
            ArrayExpression {elements} => self.array(elements),
            DuplicateExpression {multiplier, base} => self.dup(multiplier, base),
            FunctionExpression {name, arguments} => self.function(name, arguments),
            ConcatenateExpression {expressions} => self.concat(expressions),
            TernaryExpression { selector, first, second } => self.ternary(selector, first, second),
        }
    }

    fn module(&mut self, m: &ModuleBlock) -> SemanticAnalysisResult {
        self.symbols.prefix = m.name.text(self.input);
        self.parameter_declarations(&m.params)?;
        self.port_declarations(&m.ports)?;
        self.body(&m.body)?;
        self.symbols.prefix = String::new();
        Ok(())
    }

    fn parameter_declaration(&mut self, param: &ParameterDeclaration) -> SemanticAnalysisResult {
        if let Some(e) = &param.initial {
            let initial = self.constant_expression(e)?;
            let name = self.symbols.prefixed_name(&param.name.text(self.input));
            self.symbols.insert(name, SymbolKind::Constant(initial).clone());
        }
        Ok(())
    }

    fn parameter_declarations(&mut self, params: &[ParameterDeclaration]) -> SemanticAnalysisResult {
        for x in params {
            self.parameter_declaration(x)?;
        }
        Ok(())
    }

    fn port_declarations(&mut self, _ports: &[PortDeclaration]) -> SemanticAnalysisResult {
        Ok(())
    }

    fn body(&mut self, block: &[Statement]) -> SemanticAnalysisResult {
        for x in block {
            self.statement(x)?;
        }
        Ok(())
    }

    fn statement(&mut self, s: &Statement) -> SemanticAnalysisResult {
        if let Statement::ConstantDeclaration(c) = s {
            return self.constant_declaration(c);
        }
        Ok(())
    }

    fn constant_declaration(&mut self, c: &ConstantDeclaration) -> SemanticAnalysisResult {
        let value = self.constant_expression(&c.value)?;
        let name = self.symbols.prefixed_name(&c.name.text(self.input));
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
        self.symbols.prefix = g.name.text(self.input);
        for x in &g.statements {
            self.global_statement(x)?;
        }
        self.symbols.prefix = String::new();
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
