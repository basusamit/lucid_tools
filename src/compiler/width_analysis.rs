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

pub struct WidthAnalysisContext<'a> {
    input: &'a str,
    symbols: &'a mut SymbolTable,
}

pub type WidthAnalysisResult = Result<(), ProgramError>;

impl<'a> WidthAnalysisContext<'a> {
    pub fn from (input: &'a str, symbols: &'a mut SymbolTable) -> WidthAnalysisContext<'a> {
        WidthAnalysisContext {
            input,
            symbols,
        }
    }

    fn get_symbol_size(&self, t: &String) -> Result<usize, ProgramError> {
        let ident = self.symbols.prefixed_name(t);
        if !self.symbols.contains_key(&ident) {
            return Err(ProgramError::of("size", &(String::from("Symbol not found: ") + &ident)));
        }
        match self.symbols.get(&ident).unwrap() {
            SymbolKind::Input(s) |
            SymbolKind::Output(s) |
            SymbolKind::InOut(s) |
            SymbolKind::DFF(s) |
            SymbolKind::Signal(s) => Ok(s.bitwidth),
            SymbolKind::Struct(s) => Ok(s.bitwidth),
            _ => Err(ProgramError::of("symbol", "Unsupported symbol type")),
        }
    }

    fn define_symbol_size(&mut self, t: &Token, bits: usize) -> WidthAnalysisResult {
        let ident = self.symbols.prefixed_name(&t.text(self.input));
        if !self.symbols.contains_key(&ident) {
            return Err(ProgramError::of("size", &(String::from("Symbol not found: ") + &ident)));
        }
        let mut symbol = self.symbols.get_mut(&ident).unwrap();
        match symbol {
            SymbolKind::Input(s) |
            SymbolKind::Output(s) |
            SymbolKind::InOut(s) |
            SymbolKind::DFF(s) |
            SymbolKind::Signal(s) => s.bitwidth = bits,
            SymbolKind::Struct(s) => s.bitwidth = bits,
            _ => return Err(ProgramError::of("symbol", "Unknown symbol type")),
        }
        Ok(())
    }

    fn compute_sizes(&self, array: &[usize]) -> usize {
        array.iter().fold(1usize, |accum, x| accum * x)
    }

    fn shape(&self, shape: &Shape) -> Result<usize, ProgramError> {
        let mut base_size = 1usize;
        if let Some(kind) = &shape.kind {
            base_size = self.get_symbol_size(kind)?;
        }
        Ok(self.compute_sizes(&shape.dimensions) * base_size)
    }

    fn structure_declaration(&mut self, s: &StructureDeclaration) -> WidthAnalysisResult {
        let ident = self.symbols.prefixed_name(&s.name.text(self.input));
        if let Some(SymbolKind::Struct(details)) = self.symbols.get_mut(&ident) {
            let mut total_bits = 0usize;
            for x in &details.fields {
                total_bits += self.shape(&x.shape);
            }
            details.bitwidth = total_bits;
            OK(())
        } else {
            Err(ProgramError::of("struct_decl", "Name mismatch in symbol table"))
        }
    }

    fn global_statement(&mut self, s: &GlobalStatement) -> WidthAnalysisResult {
        match s {
            GlobalStatement::StructureDeclaration(s) => self.structure_declaration(s),
            GlobalStatement::ConstantDeclaration(c) => Ok(()),
        }
    }

    fn global_statements(&mut self, block: &[GlobalStatement]) -> WidthAnalysisResult {
        for x in block {
            self.global_statement(x)?;
        }
        Ok(())
    }

    fn global(&mut self, g: &GlobalBlock) -> WidthAnalysisResult {
        self.symbols.prefix = g.name.text(self.input);
        self.global_statements(&g.statements)?;
        self.symbols.prefix = String::new();
        Ok(())
    }

    fn source_block(&mut self, b: &SourceBlock) -> WidthAnalysisResult {
        match b {
            SourceBlock::GlobalBlock(g) => self.global(g),
            //SourceBlock::ModuleBlock(m) => self.module(m),
            _ => Ok(()),
        }
    }

    pub fn source(&mut self, blk: &[SourceBlock]) -> WidthAnalysisResult {
        for x in blk {
            self.source_block(x)?;
        }
        Ok(())
    }
}