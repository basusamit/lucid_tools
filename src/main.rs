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
pub mod compiler;
use std::fs::File;
use std::io::prelude::*;
use crate::compiler::pretty_write_file::PrettyWriteContext;
#[macro_use]
extern crate clap;
use clap::App;
use crate::compiler::semantic_analysis::{SemanticAnalysisContext, SymbolTable};
use crate::compiler::constant_expressions::ConstantExpressionContext;
use crate::compiler::size_analysis::SizeAnalysisContext;

/// First, parse the command line to get the arguments
fn main() {
    let yaml = load_yaml!("cli.yml");
    let matches = App::from_yaml(yaml).get_matches();

    let mut input_file = File::open(matches.value_of("INPUT").unwrap()).expect("file not found");

    let mut contents = String::new();
    input_file.read_to_string(&mut contents).expect("Unable to read the file");

    let lexer = compiler::lexer::Lexer::from(&contents);
    let mut parser = compiler::parser::Parser::from(lexer);
    let source = parser.source().unwrap();
    let output_file = File::create(matches.value_of("OUTPUT").unwrap()).expect("Cannot create output file");

    let mut ctxt = PrettyWriteContext::from(&contents, &output_file);
    ctxt.source(&source);

    let mut symbol = SymbolTable::new();
    {
        let mut semantic_analysis = SemanticAnalysisContext::from(&contents, &mut symbol);
        semantic_analysis.source(&source).unwrap();
    }
    {
        let mut const_analysis = ConstantExpressionContext::from(&contents, &mut symbol);
        const_analysis.source(&source).unwrap();
    }
    for (sym,val) in &symbol.symbols {
        println!("{} {:?}", sym, val);
    }
    {
        let mut size_analysis = SizeAnalysisContext::from(&contents, &mut symbol);
        size_analysis.source(&source).unwrap();
    }
    for (sym,val) in &symbol.symbols {
        println!("{} {:?}", sym, val);
    }
//    println!("{:?}", symbol);
}
