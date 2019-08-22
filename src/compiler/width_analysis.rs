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

// Perform a width analysis
// This determines the bitwidth of each entity in the symbol table

use crate::compiler::semantic_analysis::*;
use crate::compiler::error::ProgramError;
use crate::compiler::ast::*;
use crate::compiler::token::Token;

pub type WidthAnalysisResult = Result<(), ProgramError>;

fn get_sized_shape(symbols: &SymbolTable, shape: &Shape) -> Result<Shape, ProgramError> {
    let mut ret = shape.clone();
    let mut base_size = 1usize;
    if let Some(kind) = &shape.kind {
        base_size = get_symbol_size(kind, symbols)?;
    }
    ret.elementsize = base_size;
    ret.bitwidth = compute_sizes(&shape.dimensions) * base_size;
    Ok(ret)
}

fn compute_sizes(array: &[usize]) -> usize {
    array.iter().fold(1usize, |accum, x| accum * x)
}

fn get_struct_total_size(s: &StructDetails, symbols: &SymbolTable) -> Result<usize, ProgramError> {
    let mut total_bits = 0usize;
    for x in &s.fields {
        let sized_shape = get_sized_shape(symbols, &x.shape)?;
        total_bits = total_bits + sized_shape.bitwidth;
    }
    Ok(total_bits)
}

fn get_symbol_size(kind: &String, symbols: &SymbolTable) -> Result<usize, ProgramError> {
    if !symbols.contains_key(kind) {
        return Err(ProgramError::of("get_symbol_size", &(String::from("Symbol not found: " ) + &kind)));
    }
    match symbols.get(&kind).unwrap() {
        SymbolKind::Input(s) |
        SymbolKind::Output(s) |
        SymbolKind::InOut(s) |
        SymbolKind::DFF(s) |
        SymbolKind::Signal(s) => Ok(get_sized_shape(symbols, s).unwrap().bitwidth),
        SymbolKind::Struct(s) => get_struct_total_size(s, symbols),
        _ => unimplemented!("Symbol not covered"),
    }
}

fn size_struct(details: &mut StructDetails, symbols: &SymbolTable) -> Result<(), ProgramError> {
    let mut total_bits = 0usize;
    for x in &mut details.fields {
        let sized_shape = get_sized_shape(symbols, &x.shape)?;
        x.shape.bitwidth = sized_shape.bitwidth;
        x.shape.bitoffset = total_bits;
        x.shape.elementsize = sized_shape.elementsize;
        total_bits = total_bits + x.shape.bitwidth;
    }
    details.bitwidth = total_bits;
    Ok(())
}

pub fn compute_bitwidths(symbols: &SymbolTable) -> SymbolTable {
    let mut ret = SymbolTable::new();
    for symbol in &symbols.symbols {
        println!("Symbol: {}", symbol.0);
        let mut kind = symbol.1.clone();
        match &mut kind {
            SymbolKind::Input(s) |
            SymbolKind::Output(s) |
            SymbolKind::InOut(s) |
            SymbolKind::DFF(s) |
            SymbolKind::Signal(s) => s.clone_from(&get_sized_shape(symbols, &s).unwrap()),
            SymbolKind::Struct(s) => size_struct(s, symbols).unwrap(),
            _ => {},
        }
        ret.insert(symbol.0.clone(), kind);
    }
    ret
}
