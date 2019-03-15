pub mod compiler;
use std::fs::File;
use std::io::prelude::*;
use crate::compiler::pretty_write_file::PrettyWriteContext;
#[macro_use]
extern crate clap;
use clap::App;

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

    let mut ctxt = PrettyWriteContext::from(&contents,&output_file);
    ctxt.source(&source);
}
