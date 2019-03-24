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

use std::collections::HashMap;
use super::ast::*;
use super::token::*;
use super::error::ProgramError;
use crate::compiler::cvalue::ConstantValue;

#[derive(Clone, Debug, PartialEq)]
pub struct TypeDetails {
    kind: Option<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FSMDetails {
    states: Vec<String>,
}


#[derive(Clone, Debug, PartialEq)]
pub enum SymbolKind {
    Constant(ConstantValue),
    Parameter,
    Input,
    Output,
    InOut,
    FSM(FSMDetails),
    DFF(TypeDetails),
    ModuleInstance,
    Variable,
    Struct,
    Signal(TypeDetails),
    Module,
    Global,
}

pub type SymbolTable = HashMap<String, SymbolKind>;

pub struct SemanticAnalysisContext<'a> {
    input: &'a str,
    symbols: &'a mut SymbolTable,
    prefix: String,
}

pub type SemanticAnalysisResult = Result<(), ProgramError>;

impl<'a> SemanticAnalysisContext<'a> {
    pub fn from(input: &'a str, symbols: &'a mut SymbolTable) -> SemanticAnalysisContext<'a> {
        SemanticAnalysisContext {
            input,
            symbols,
            prefix: String::new(),
        }
    }

    fn define_symbol(&mut self, t: &Token, s: &SymbolKind) -> SemanticAnalysisResult {
        let ident = self.prefixed_name(&t.text(self.input));
        if self.symbols.contains_key(&ident) {
            return Err(ProgramError::of("sema", "Symbol already defined"));
        }
        self.symbols.insert(ident, s.clone());
        Ok(())
    }

    pub fn update_symbol(&mut self, ident: String, s: &SymbolKind) -> SemanticAnalysisResult {
        self.symbols.insert(ident, s.clone());
        Ok(())
    }

    fn structure_declaration(&mut self, s: &StructureDeclaration) -> SemanticAnalysisResult {
        self.define_symbol(&s.name, &SymbolKind::Struct)?;
        // TODO - handle fields of the struct?
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
        self.define_symbol(&c.name, &SymbolKind::Constant(ConstantValue::empty()))?;
        Ok(())
    }

    fn global_statement(&mut self, s: &GlobalStatement) -> SemanticAnalysisResult {
        match s {
            GlobalStatement::StructureDeclaration(s) => self.structure_declaration(s),
            GlobalStatement::ConstantDeclaration(c) => self.constant_declaration(c),
        }
    }

    fn global_statements(&mut self, block: &[GlobalStatement]) -> SemanticAnalysisResult {
        for x in block {
            self.global_statement(x)?;
        }
        Ok(())
    }

    fn variable_declaration(&mut self, v: &[TypeDeclaration]) -> SemanticAnalysisResult {
        for x in v {
            self.define_symbol(&x.name, &SymbolKind::Variable)?;
        }
        Ok(())
    }

    fn map_struct_type(&mut self, s: &Option<StructureType>) -> Result<Option<String>, ProgramError> {
        // TODO - validate this type
        match s {
            Some(s) => Ok(Some(
                s.names
                    .iter()
                    .map(|x| x.text(self.input))
                    .collect::<Vec<String>>()
                    .join("."))),
            None => Ok(None),
        }
    }

    fn signal_declaration(&mut self, s: &SignalDeclaration) -> SemanticAnalysisResult {
        let struct_kind = self.map_struct_type(&s.kind)?;
        for x in &s.vars {
            self.define_symbol(&x.name, &SymbolKind::Signal(TypeDetails {
                kind: struct_kind.clone()
            }))?;
        }
        Ok(())
    }

    fn fsm_declaration(&mut self, f: &FSMDeclaration) -> SemanticAnalysisResult {
        let mut states = vec![];
        for x in &f.states {
            states.push(x.text(self.input));
        }
        self.define_symbol(&f.name, &SymbolKind::FSM(FSMDetails { states }))
    }

    fn dff_declaration(&mut self, d: &DFFDeclaration) -> SemanticAnalysisResult {
        let struct_kind = self.map_struct_type(&d.kind)?;
        for x in &d.dffs {
            self.define_symbol(&x.name, &SymbolKind::DFF(TypeDetails {
                kind: struct_kind.clone()
            }))?
        }
        Ok(())
    }


    fn always_statements(&mut self, _b: &[AlwaysStatement]) -> SemanticAnalysisResult {
        // Always statements do not require analysis.  They cannot contain definitions`
        Ok(())
    }

    fn always_block(&mut self, b: &[AlwaysStatement]) -> SemanticAnalysisResult {
        self.always_statements(b)
    }

    fn module_instantiation(&mut self, m: &ModuleInstantiation) -> SemanticAnalysisResult {
        self.define_symbol(&m.myname, &SymbolKind::ModuleInstance)
    }

    fn assign_block_declaration(&mut self, d: &AssignBlockKind) -> SemanticAnalysisResult {
        match d {
            AssignBlockKind::ModuleInstantiation(m) => self.module_instantiation(m),
            AssignBlockKind::DFFDeclaration(d) => self.dff_declaration(d),
            AssignBlockKind::FSMDeclaration(f) => self.fsm_declaration(f),
            AssignBlockKind::AssignBlock(a) => self.assign_block(a),
        }
    }


    fn assign_block(&mut self, b: &AssignBlock) -> SemanticAnalysisResult {
        for x in &b.declarations {
            self.assign_block_declaration(x)?;
        }
        Ok(())
    }

    fn statement(&mut self, s: &Statement) -> SemanticAnalysisResult {
        match s {
            Statement::ConstantDeclaration(c) => self.constant_declaration(c),
            Statement::VariableDeclaration(v) => self.variable_declaration(v),
            Statement::SignalDeclaration(s) => self.signal_declaration(s),
            Statement::FSMDeclaration(f) => self.fsm_declaration(f),
            Statement::DFFDeclaration(d) => self.dff_declaration(d),
            Statement::ModuleInstantiation(m) => self.module_instantiation(m),
            Statement::AlwaysBlock(b) => self.always_block(b),
            Statement::AssignBlock(b) => self.assign_block(b),
            Statement::StructureDeclaration(s) => self.structure_declaration(s),
        }
    }

    fn body(&mut self, block: &[Statement]) -> SemanticAnalysisResult {
        for x in block {
            self.statement(x)?;
        }
        Ok(())
    }

    fn port_declaration(&mut self, p: &PortDeclaration) -> SemanticAnalysisResult {
        match p.direction.kind {
            TokenKind::INPUT => self.define_symbol(&p.name, &SymbolKind::Input),
            TokenKind::OUTPUT => self.define_symbol(&p.name, &SymbolKind::Output),
            TokenKind::INOUT => self.define_symbol(&p.name, &SymbolKind::InOut),
            _ => Err(ProgramError::of("portdeclaration", "Unexpected type for port"))
        }
    }

    fn port_declarations(&mut self, ports: &[PortDeclaration]) -> SemanticAnalysisResult {
        for x in ports {
            self.port_declaration(x)?;
        }
        Ok(())
    }

    fn parameter_declaration(&mut self, p: &ParameterDeclaration) -> SemanticAnalysisResult {
        self.define_symbol(&p.name, &SymbolKind::Parameter)
    }

    fn parameter_declarations(&mut self, params: &[ParameterDeclaration]) -> SemanticAnalysisResult {
        for x in params {
            self.parameter_declaration(x)?;
        }
        Ok(())
    }

    fn module(&mut self, m: &ModuleBlock) -> SemanticAnalysisResult {
        self.define_symbol(&m.name, &SymbolKind::Module)?;
        self.parameter_declarations(&m.params)?;
        self.port_declarations(&m.ports)?;
        self.body(&m.body)
    }

    fn global(&mut self, g: &GlobalBlock) -> SemanticAnalysisResult {
        self.define_symbol(&g.name, &SymbolKind::Global)?;
        self.prefix = g.name.text(self.input);
        self.global_statements(&g.statements)?;
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
        Ok(()) // Not sure if this is rust-approved
    }
}
