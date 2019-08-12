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

// Perform a size analysis
// The following constructs require a size analysis:
// Signal declarations (TypeDeclarations) - done
// DFF declarations - done
// FSM declarations
// Module instantiations
// Port declaration
// Struct members - done

use crate::compiler::error::ProgramError;
use crate::compiler::semantic_analysis::{SymbolTable, SymbolKind};
use crate::compiler::ast::*;
use crate::compiler::token::Token;
use crate::compiler::constant_expressions::ConstantExpressionContext;
use num_traits::cast::ToPrimitive;

pub struct SizeAnalysisContext<'a> {
    input: &'a str,
    symbols: &'a mut SymbolTable,
}

pub type SizeAnalysisResult = Result<(), ProgramError>;

impl<'a> SizeAnalysisContext<'a> {
    pub fn from(input: &'a str, symbols: &'a mut SymbolTable) -> SizeAnalysisContext<'a> {
        SizeAnalysisContext {
            input,
            symbols,
        }
    }

    fn variable_declaration(&mut self, v: &[TypeDeclaration]) -> SizeAnalysisResult {
        for x in v {
            self.size_symbol(&x.name, &x.sizes)?;
        }
        Ok(())
    }

    fn signal_declaration(&mut self, s: &SignalDeclaration) -> SizeAnalysisResult {
        for x in &s.vars {
            self.size_symbol(&x.name, &x.sizes)?;
        }
        Ok(())
    }

    fn fsm_declaration(&mut self, f: &FSMDeclaration) -> SizeAnalysisResult {
        self.size_symbol(&f.name, &f.sizes)
    }

    fn dff_declaration(&mut self, d: &DFFDeclaration) -> SizeAnalysisResult {
        for x in &d.dffs {
            self.size_symbol(&x.name, &x.sizes)?;
        }
        Ok(())
    }

    fn module_instantiation(&mut self, m: &ModuleInstantiation) -> SizeAnalysisResult {
        self.size_symbol(&m.myname, &m.sizes)
    }

    fn always_statements(&mut self, _b: &[AlwaysStatement]) -> SizeAnalysisResult {
        Ok(())
    }

    fn always_block(&mut self, b: &[AlwaysStatement]) -> SizeAnalysisResult {
        self.always_statements(b)
    }

    fn assign_block_declaration(&mut self, d: &AssignBlockKind) -> SizeAnalysisResult {
        match d {
            AssignBlockKind::ModuleInstantiation(m) => self.module_instantiation(m),
            AssignBlockKind::DFFDeclaration(d) => self.dff_declaration(d),
            AssignBlockKind::FSMDeclaration(f) => self.fsm_declaration(f),
            AssignBlockKind::AssignBlock(a) => self.assign_block(a),
        }
    }

    fn assign_block(&mut self, b: &AssignBlock) -> SizeAnalysisResult {
        for x in &b.declarations {
            self.assign_block_declaration(x)?;
        }
        Ok(())
    }

    fn structure_field(&mut self, struct_name: &String, field: &StructMember) -> SizeAnalysisResult {
        let context = ConstantExpressionContext::from(self.input, self.symbols);
        let size_vec = self.map_sizes_to_vec(&field.sizes)?;
        let info = self.symbols.get_mut(struct_name).unwrap();
        let field_name = field.name.text(self.input);
        println!("Sizing field {} of struct {} as {:?}", field_name, struct_name, size_vec);
        if let SymbolKind::Struct(details) = info {
            for field_detail in &mut details.fields {
                if field_detail.field == field.name.text(self.input) {
                    field_detail.shape.dimensions = size_vec.clone();
                }
            }
        };
        Ok(())
    }

    fn structure_declaration(&mut self, s: &StructureDeclaration) -> SizeAnalysisResult {
        let structure_name = s.name.text(self.input);
        let struct_name = self.symbols.prefixed_name(&structure_name);
        for x in &s.members {
            self.structure_field(&struct_name, x)?;
        }
        // Nothing to
        Ok(())
    }

    fn statement(&mut self, s: &Statement) -> SizeAnalysisResult {
        match s {
            Statement::VariableDeclaration(v) => self.variable_declaration(v),
            Statement::SignalDeclaration(s) => self.signal_declaration(s),
            Statement::FSMDeclaration(f) => self.fsm_declaration(f),
            Statement::DFFDeclaration(d) => self.dff_declaration(d),
            Statement::ModuleInstantiation(m) => self.module_instantiation(m),
            Statement::AlwaysBlock(b) => self.always_block(b),
            Statement::AssignBlock(b) => self.assign_block(b),
            Statement::StructureDeclaration(s) => self.structure_declaration(s),
            _ => Ok(()),
        }
    }

    fn body(&mut self, block: &[Statement]) -> SizeAnalysisResult {
        for x in block {
            self.statement(x)?;
        }
        Ok(())
    }

    fn map_sizes_to_vec(&mut self, sizes: &[Box<Expression>]) -> Result<Vec<usize>, ProgramError> {
        let context = ConstantExpressionContext::from(self.input, self.symbols);
        let mut size_vec = vec![];
        for x in sizes {
            let cexpr = context.constant_expression(x)?;
            let cint = cexpr.as_int().to_i32().unwrap() as usize;
            size_vec.push(cint);
        }
        if size_vec.is_empty() {
            Ok(vec![1])
        } else {
            Ok(size_vec)
        }
    }

    fn size_symbol(&mut self, name: &Token, sizes: &[Box<Expression>]) -> SizeAnalysisResult {
        let size_vec = self.map_sizes_to_vec(sizes);
        let prefixed = self.symbols.prefixed_name(&name.text(self.input));
        println!("Sizing symbol {}", prefixed);
        let info = self.symbols.get_mut(&prefixed).unwrap();
        match info {
            SymbolKind::Input(v) |
            SymbolKind::Output(v) |
            SymbolKind::InOut(v) |
            SymbolKind::DFF(v) |
            SymbolKind::Signal(v) => v.dimensions = size_vec?,
            _ => {}
        }
        Ok(())
    }

    fn parameter_declarations(&mut self, p: &[ParameterDeclaration]) -> SizeAnalysisResult {
        if p.len() != 0 {
            panic!("Parameter declarations not handled yet");
        }
        Ok(())
    }

    fn port_declaration(&mut self, p: &PortDeclaration) -> SizeAnalysisResult {
        self.size_symbol(&p.name, &p.sizes)
    }


    fn port_declarations(&mut self, ports: &[PortDeclaration]) -> SizeAnalysisResult {
        for x in ports {
            self.port_declaration(x)?;
        }
        Ok(())
    }

    fn module(&mut self, m: &ModuleBlock) -> SizeAnalysisResult {
        self.symbols.prefix = m.name.text(self.input);
        self.port_declarations(&m.ports)?;
        self.parameter_declarations(&m.params)?;
        self.body(&m.body);
        self.symbols.prefix = String::new();
        Ok(())
    }

    fn global(&mut self, g: &GlobalBlock) -> SizeAnalysisResult {
        Ok(())
    }

    fn source_block(&mut self, b: &SourceBlock) -> SizeAnalysisResult {
        match b {
            SourceBlock::GlobalBlock(g) => self.global(g),
            SourceBlock::ModuleBlock(m) => self.module(m),
        }
    }

    pub fn source(&mut self, blk: &[SourceBlock]) -> SizeAnalysisResult {
        for x in blk {
            self.source_block(x)?;
        }
        Ok(())
    }
}