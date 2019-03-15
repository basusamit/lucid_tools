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
use super::ast::*;
use super::ast::Expression::*;
use super::token::*;
use std::fs::File;
use std::error::Error;
use std::io::Write;
use std::cmp::min;
use std::cmp::max;

pub struct PrettyWriteContext<'a> {
    indent_level: usize,
    line_indent_count: i32,
    line_min_indent_count: i32,
    file: &'a File,
    input: &'a str,
    line: String,
}

pub type PrettyWriteError = Result<(), Error>;

impl<'a> PrettyWriteContext<'a> {
    pub fn from(input: &'a str, file: &'a File) -> PrettyWriteContext<'a> {
        PrettyWriteContext {
            indent_level: 0,
            line_indent_count: 0,
            line_min_indent_count: 0,
            file,
            input,
            line: String::new(),
        }
    }

    fn newline(&mut self) {
        let indent = max(0,self.indent_level as i32 + self.line_min_indent_count);
        for _i in 0..(indent*4) {
            write!(self.file," ").unwrap();
        }
        self.indent_level = max(0,self.indent_level as i32 + self.line_indent_count) as usize;
        self.line_indent_count = 0;
        self.line_min_indent_count = 0;
    }

    fn putc(& mut self, val: char) {
        // Add the character to the line
        self.line.push(val.clone());
        if val.eq(&'{') || val.eq(&'(') {
            self.line_indent_count += 1;
        }
        if val.eq(&'}') || val.eq(&')') {
            self.line_indent_count -= 1;
        }
        self.line_min_indent_count = min(self.line_min_indent_count, self.line_indent_count);
        if val.eq(&'\n') {
            self.newline();
            write!(self.file,"{}",self.line).unwrap();
            self.line.clear();
        }
    }

    fn put(& mut self, text: &str) {
        for x in text.chars() {
            self.putc(x);
        }
    }

    fn token(& mut self, token: &Token) {
        self.put(&token.text(&self.input));
    }

    fn number(& mut self, number: &Number) {
        match number {
            Number::IntegerNumber{value} => self.token(value),
            Number::DecimalNumber {value } => self.token(value),
            _ => panic!(),
        }
    }

    fn const_bit_selector(& mut self, start: &Expression, stop: &Expression) {
        self.put("[");
        self.expression(start);
        self.put(":");
        self.expression(stop);
        self.put("]");
    }

    fn fixed_width_bit_selector(& mut self, start: &Expression, sign: &Token, width: &Expression) {
        self.put("[");
        self.expression(start);
        self.token(sign);
        self.expression(width);
        self.put("]");
    }

    fn bit_selector(& mut self, selector: &BitSelector) {
        match selector {
            BitSelector::ConstBitSelector {start, stop} =>
                self.const_bit_selector(&start, &stop),
            BitSelector::FixedWidthBitSelector { start, sign, width} =>
                self.fixed_width_bit_selector(&start, &sign, &width),
        }
    }

    fn bit_selection(& mut self, selection: &BitSelection) {
        if let Some(e) = &selection.selector {
            self.bit_selector(e);
        }
        for expr in &selection.arrays {
            self.put("[");
            self.expression(expr);
            self.put("]");
        }
    }

    fn named_bit_selector(& mut self, selector: &NamedBitSelector) {
        self.token(&selector.name);
        self.bit_selection(&selector.selector);
    }

    fn write_delim_sep<T,F>(& mut self, list: &[T], sep: &str, func: F) where
        F: Fn(& mut Self, &T) -> ()
    {
        for x in 0..list.len() {
            func(self, &list[x]);
            if x != list.len()-1 {
                self.put(sep);
            }
        }
    }

    fn signal(& mut self, signal: &[NamedBitSelector]) {
        self.write_delim_sep(signal, ".", PrettyWriteContext::named_bit_selector);
    }

    fn expression_list(& mut self, arguments: &[Box<Expression>]) {
        self.write_delim_sep(arguments,",",
                             |x, y| x.expression(y));
//                             PrettyWriteContext::expression);
    }

    fn function(& mut self, name: &Token, arguments: &[Box<Expression>]) {
        self.token(name);
        self.put("(");
        self.expression_list(arguments);
        self.put(")");
    }

    fn concatenate(& mut self, expressions: &[Box<Expression>]) {
        self.put("c{ ");
        self.expression_list(expressions);
        self.put(" }");
    }

    fn duplicate(& mut self, multiplier: &Expression, base: &Expression) {
        self.expression(multiplier);
        self.put(" x{ ");
        self.expression(base);
        self.put(" }");
    }

    fn array(& mut self, elements: &[Box<Expression>]) {
        self.put("{");
        self.expression_list(elements);
        self.put("}");
    }

    fn prefix(& mut self, operator: &Token, operand: &Expression) {
        self.token(operator);
        self.expression(operand);
    }

    fn infix(& mut self, lhs: &Expression, operator: &Token, rhs: &Expression) {
        self.expression(lhs);
        self.token(operator);
        self.expression(rhs);
    }

    fn ternary(& mut self, selector: &Expression, first: &Expression, second: &Expression) {
        self.expression(selector);
        self.put(" ? ");
        self.expression(first);
        self.put(" : ");
        self.expression(second);
    }

    fn expression_group(& mut self, exp: &Expression) {
        self.put("(");
        self.expression(exp);
        self.put(")");
    }

    fn expression(& mut self, expr: &Expression) {
        match expr {
            SignalExpression(s) => self.signal(s),
            NumberExpression(n) => self.number(n),
            ExpressionGroup(e) => self.expression_group(e),
            FunctionExpression{name, arguments} => self.function(name,arguments),
            ConcatenateExpression {expressions} => self.concatenate(expressions),
            DuplicateExpression {multiplier, base} => self.duplicate(multiplier,base),
            ArrayExpression {elements} => self.array(elements),
            PrefixExpression {operator, operand} => self.prefix(operator, operand),
            InfixExpression {lhs, operator, rhs} => self.infix(lhs, operator, rhs),
            TernaryExpression {selector, first, second} => self.ternary(selector, first, second),
        }
    }

    fn parameter_declaration(& mut self, param: &ParameterDeclaration) {
        self.token(&param.name);
        if let Some(i) = &param.initial {
            self.put(" = ");
            self.expression(i);
        }
        if let Some(c) = &param.constraint {
            self.put(" : ");
            self.expression(c);
        }
    }

    fn parameter_declarations(& mut self, params: &[ParameterDeclaration]) {
        if params.is_empty() {
            return;
        }
        self.put("#(");
        self.put("\n");
        self.write_delim_sep(params,",\n",PrettyWriteContext::parameter_declaration);
        self.put("\n)\n");
    }

    fn structure_type(& mut self, kind: &Option<StructureType>) {
        if let Some(s) = kind {
            self.put("<");
            self.write_delim_sep(&s.names,".", PrettyWriteContext::token);
            self.put(">");
        }
    }

    fn array_sizes(& mut self, args: &[Box<Expression>]) {
        for x in args {
            self.put("[");
            self.expression(x);
            self.put("]");
        }
    }

    fn port_declaration(& mut self, port: &PortDeclaration) {
        if port.signed {self.put("signed ");}
        self.token(&port.direction);
        self.structure_type(&port.kind);
        self.put(" ");
        self.token(&port.name);
        self.array_sizes(&port.sizes);
    }

    fn port_declarations(& mut self, ports: &[PortDeclaration]) {
        self.put("(\n");
        self.write_delim_sep(ports, ",\n", PrettyWriteContext::port_declaration);
        self.put("\n)");
    }

    fn constant_declaration(& mut self, c: &ConstantDeclaration) {
        self.put("const ");
        self.token(&c.name);
        self.put(" = ");
        self.expression(&c.value);
        self.put(";\n");
    }

    fn type_declaration(& mut self, t: &TypeDeclaration) {
        self.token(&t.name);
        self.array_sizes(&t.sizes);
    }

    fn type_declarations(& mut self, v : &[TypeDeclaration]) {
        self.write_delim_sep(v, ",", PrettyWriteContext::type_declaration);
    }

    fn variable_declaration(& mut self, v : &[TypeDeclaration]) {
        self.put("var ");
        self.type_declarations(v);
        self.put(";\n");
    }

    fn signal_declaration(& mut self, s: &SignalDeclaration) {
        if s.signed {self.put("signed ");}
        self.put("sig ");
        self.structure_type(&s.kind);
        self.type_declarations(&s.vars);
        self.put(";\n");
    }

    fn name_list(& mut self, n: &[Token]) {
        self.write_delim_sep(n, ",", PrettyWriteContext::token);
    }

    fn connection(& mut self, c: &Connection) {
        match c {
            Connection::SignalConnection{name, value} => {
                self.put(".");
                self.token(name);
                self.put("(");
                self.expression(value);
                self.put(")");
            },
            Connection::ParameterConnection {name, value} => {
                self.put("#");
                self.token(name);
                self.put("(");
                self.expression(value);
                self.put(")");
            }
        }
    }

    fn connection_list(& mut self, list: &[Connection]) {
        self.write_delim_sep(list, ", ", PrettyWriteContext::connection);
    }

    fn instance_connections(& mut self, i: &InstanceConnections) {
        if i.is_empty() {
            return;
        }
        self.put("(");
        self.connection_list(i);
        self.put(")");
    }

    fn fsm_declaration(& mut self, f: &FSMDeclaration) {
        self.put("fsm ");
        self.token(&f.name);
        self.array_sizes(&f.sizes);
        self.instance_connections(&f.connections);
        self.put(" = {");
        self.name_list(&f.states);
        self.put("};\n");
    }

    fn dff_single_declaration(& mut self, d: &DFFSingleDeclaration) {
        self.token(&d.name);
        self.array_sizes(&d.sizes);
        self.instance_connections(&d.connections);
    }

    fn dff_single_declarations(& mut self, d: &[DFFSingleDeclaration]) {
        self.write_delim_sep(d,",",PrettyWriteContext::dff_single_declaration);
    }

    fn dff_declaration(& mut self, d: &DFFDeclaration) {
        if d.signed {self.put("signed ");}
        self.put("dff ");
        self.structure_type(&d.kind);
        self.dff_single_declarations(&d.dffs);
        self.put(";\n");
    }

    fn module_instantiation(& mut self, m: &ModuleInstantiation) {
        self.token(&m.modname);
        self.put(" ");
        self.token(&m.myname);
        self.array_sizes(&m.sizes);
        self.instance_connections(&m.connections);
        self.put(";\n");
    }

    fn case_element(& mut self, c: &CaseElement) {
        match &c.kind {
            ExpressionOrDefault::Expression(e) => {
                self.expression(e);
                self.put(": \n");
                self.indent_level += 1;
                self.always_statements(&c.statements);
                self.indent_level -= 1;
            }
            ExpressionOrDefault::Default => {
                self.put("default: \n");
                self.indent_level += 1;
                self.always_statements(&c.statements);
                self.indent_level -= 1;
            },
        }
    }

    fn case_elements(& mut self, v: &[CaseElement]) {
        self.write_delim_sep(v, "",PrettyWriteContext::case_element);
    }

    fn case_statement(& mut self, c: &CaseStatement) {
        self.put("case ");
        self.put("(");
        self.expression(&c.test);
        self.put(") {\n");
        self.case_elements(&c.cases);
        self.put("}\n");
    }

    fn if_statement(& mut self, i: &IfStatement) {
        self.put("if (");
        self.expression(&i.test);
        self.put(") {\n");
        self.always_statements(&i.block);
        self.put("}");
        if !i.else_block.is_empty() {
            self.put(" else {\n");
            self.always_statements(&i.else_block);
            self.put("}\n");
        } else {
            self.put("\n");
        }
    }

    fn assign_statement(& mut self, a: &AssignStatement) {
        self.signal(&a.signal);
        self.put(" = ");
        self.expression(&a.rhs);
        self.put(";\n");
    }

    fn signal_increment_decrement(& mut self, s: &SignalIncrementDecrement) {
        self.signal(&s.signal);
        self.token(&s.operator);
    }

    fn variable_assign(& mut self, v: &VariableAssign) {
        match v {
            VariableAssign::SignalIncrementDecrement(s) =>
                self.signal_increment_decrement(s),
            VariableAssign::AssignStatement(a) =>
                self.assign_statement(a),
        }
    }

    fn for_statement(& mut self, f: &ForStatement) {
        self.put("for (");
        self.signal(&f.init.signal);
        self.put(" = ");
        self.expression(&f.init.rhs);
        self.put(";");
        self.expression(&f.test);
        self.put("; ");
        self.variable_assign(&f.increment);
        self.put(") {\n");
        self.always_statements(&f.block);
        self.put("}\n");
    }

    fn always_statement(& mut self, s: &AlwaysStatement) {
        match s {
            AlwaysStatement::AssignStatement(a) => self.assign_statement(a),
            AlwaysStatement::CaseStatement(c) => self.case_statement(c),
            AlwaysStatement::IfStatement(i) => self.if_statement(i),
            AlwaysStatement::ForStatement(f) => self.for_statement(f),
        }
    }

    fn always_statements(& mut self, b: &[AlwaysStatement]) {
        self.write_delim_sep(b, "", PrettyWriteContext::always_statement);
    }

    fn always_block(& mut self, b: &[AlwaysStatement]) {
        self.put("always {\n");
        self.always_statements(b);
        self.put("}\n");
    }

    fn assign_block_declaration(& mut self, d: &AssignBlockKind) {
        match d {
            AssignBlockKind::ModuleInstantiation(m) => self.module_instantiation(m),
            AssignBlockKind::DFFDeclaration(d) => self.dff_declaration(d),
            AssignBlockKind::FSMDeclaration(f) => self.fsm_declaration(f),
            AssignBlockKind::AssignBlock(a) => self.assign_block(a),
        }
    }

    fn assign_block(& mut self, b: &AssignBlock) {
        self.connection_list(&b.connections);
        self.put(" {\n");
        self.write_delim_sep(&b.declarations, "", PrettyWriteContext::assign_block_declaration);
        self.put("}\n");
    }

    fn structure_member(& mut self, s: &StructMember) {
        self.token(&s.name);
        self.put(" ");
        self.structure_type(&s.kind);
        self.array_sizes(&s.sizes);
    }

    fn structure_declaration(& mut self, s: &StructureDeclaration) {
        self.put("struct ");
        self.token(&s.name);
        self.put("{\n");
        self.write_delim_sep(&s.members, ";\n", PrettyWriteContext::structure_member);
        self.put("}\n");
    }

    fn statement(& mut self, s: &Statement) {
        match s {
            Statement::ConstantDeclaration(c) => self.constant_declaration(c),
            Statement::VariableDeclaration(v) => self.variable_declaration(v),
            Statement::SignalDeclaration(s) => self.signal_declaration(s),
            Statement::FSMDeclaration(f) => self.fsm_declaration(f),
            Statement::DFFDeclaration(d) => self.dff_declaration(d),
            Statement::ModuleInstantiation(m)=> self.module_instantiation(m),
            Statement::AlwaysBlock(b) => self.always_block(b),
            Statement::AssignBlock(b) => self.assign_block(b),
            Statement::StructureDeclaration(s) => self.structure_declaration(s),
        }
    }

    fn body(& mut self, block: &[Statement]) {
        self.write_delim_sep(block, "\n", PrettyWriteContext::statement);
    }

    fn module(& mut self, module: &ModuleBlock) {
        self.put("module ");
        self.token(&module.name);
        self.put("\n");
        self.indent_level += 1;
//        self.prev_indent_level += 1;
        self.parameter_declarations(&module.params);
        self.port_declarations(&module.ports);
        self.put("\n{");
        self.put("\n");
        self.body(&module.body);
        self.put("}");
        self.put("\n");
        self.indent_level -= 1;
    }

    fn global_statement(& mut self, s: &GlobalStatement) {
        match s {
            GlobalStatement::StructureDeclaration(s) => self.structure_declaration(s),
            GlobalStatement::ConstantDeclaration(c) => self.constant_declaration(c),
        }
    }

    fn global_statements(& mut self, block: &[GlobalStatement]) {
        self.write_delim_sep(block, "\n", PrettyWriteContext::global_statement);
    }

    fn global(& mut self, global: &GlobalBlock) {
        self.put("global ");
        self.token(&global.name);
        self.put("{");
        self.put("\n");
        self.global_statements(&global.statements);
        self.put("}");
        self.put("\n");
    }

    fn source_block(& mut self, block: &SourceBlock) {
        match block {
            SourceBlock::GlobalBlock(g) => self.global(g),
            SourceBlock::ModuleBlock(m) => self.module(m),
        }
    }

    pub fn source(& mut self, blk: &[SourceBlock]) {
        self.write_delim_sep(blk, "\n", PrettyWriteContext::source_block);
    }
}