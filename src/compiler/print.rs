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
use std::fmt::Write;

pub struct PrintContext {
    input: String,
}

impl PrintContext {
    pub fn from(text: &str) -> PrintContext {
        PrintContext {
            input: String::from(text),
        }
    }

    fn token(& self, token: &Token) -> String {
        token.text(&self.input)
    }

    fn number(& self, number: &Number) -> String {
        match number {
            Number::IntegerNumber{value} => self.token(value),
            Number::DecimalNumber {value } => self.token(value),
            _ => panic!(),
        }
    }

    fn const_bit_selector(&self, start: &Expression, stop: &Expression) -> String {
        let mut ret = String::new();
        let _ = write!(ret,"[{}:{}]",self.expression(start), self.expression(stop));
        ret
    }

    fn fixed_width_bit_selector(&self, start: &Expression, sign: &Token, width: &Expression) -> String {
        let mut ret = String::new();
        let _ = write!(ret,"[{}{}{}]", self.expression(start), self.token(sign), self.expression(width));
        ret
    }

    fn bit_selector(&self, selector: &BitSelector) -> String {
        match selector {
            BitSelector::ConstBitSelector {start, stop} => self.const_bit_selector(&start, &stop),
            BitSelector::FixedWidthBitSelector { start, sign, width} => self.fixed_width_bit_selector(&start, &sign, &width),
        }
    }

    fn bit_selection(&self, selection: &BitSelection) -> String {
        let selector_text = if let Some(e) = &selection.selector {
            self.bit_selector(e)
        } else {
            String::new()
        };
        (&selection.arrays).iter()
            .map(|x| self.wrap("[",&self.expression(&x),"]"))
            .collect::<Vec<String>>()
            .join("") + &selector_text
    }

    fn named_bit_selector(&self, selector: &NamedBitSelector) -> String {
        self.token(&selector.name) + &self.bit_selection(&selector.selector)
    }

    fn signal(&self, signal: &[NamedBitSelector]) -> String {
        signal.iter()
            .map(|x| self.named_bit_selector(x))
            .collect::<Vec<String>>()
            .join(".")
    }

    fn expression_list(&self, arguments: &[Box<Expression>]) -> String {
        arguments.iter()
            .map(|x| self.expression(x))
            .collect::<Vec<String>>()
            .join(",")
    }

    fn wrap(&self, left: &str, mid: &str, right: &str) -> String {
        String::from(left) + mid + &String::from(right)
    }

    fn paren(&self, arg: &str) -> String {
        self.wrap("(", arg, ")")
    }

    fn function(&self, name: &Token, arguments: &[Box<Expression>]) -> String {
        self.token(name) + &self.paren(&self.expression_list(arguments))
    }

    fn concatenate(&self, expressions: &[Box<Expression>]) -> String {
        self.wrap("c{ ", &self.expression_list(expressions), " }")
    }

    fn duplicate(&self, multiplier: &Expression, base: &Expression) -> String {
        self.expression(multiplier) + &self.wrap("x{ ", &self.expression(base), " }")
    }

    fn array(&self, elements: &[Box<Expression>]) -> String {
        self.wrap("{", &self.expression_list(elements), "}")
    }

    fn prefix(&self, operator: &Token, operand: &Expression) -> String {
        self.token(operator) + &self.expression(operand)
    }

    fn infix(&self, lhs: &Expression, operator: &Token, rhs: &Expression) -> String {
        self.expression(lhs) + &self.token(operator) + &self.expression(rhs)
    }

    fn ternary(&self, selector: &Expression, first: &Expression, second: &Expression) -> String {
        let mut ret = String::new();
        let _ = write!(ret, "{} ? {} : {}", self.expression(selector),
                       self.expression(first), self.expression(second));
        ret
    }

    fn expression_group(&self, exp: &Expression) -> String {
        self.paren(&self.expression(exp))
    }

    fn expression(& self, expr: &Expression) -> String {
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

    fn parameter_declaration(&self, param: &ParameterDeclaration) -> String {
        let base = self.token(&param.name);
        let initial = if let Some(i) = &param.initial {
            String::from(" = ") + &self.expression(i)
        } else {
            String::new()
        };
        let constraint = if let Some(c) = &param.constraint {
            String::from(" : ") + &self.expression(c)
        } else {
            String::new()
        };
        base + &initial + &constraint
    }

    fn parameter_declarations(&self, params: &[ParameterDeclaration]) -> String {
        if params.is_empty() {
            return String::new();
        }
        let ret : Vec<String> = params.iter().map(|x| self.parameter_declaration(x)).collect();
        self.wrap("#(", &ret.join(","), ")")
    }

    fn structure_type(&self, kind: &Option<StructureType>) -> String {
        if let Some(s) = kind {
            self.wrap("<",
                      &(&s.names).iter()
                          .map(|x| self.token(&x))
                          .collect::<Vec<String>>()
                          .join("."),
                      ">")
        } else {
            String::new()
        }
    }

    fn array_sizes(&self, args: &[Box<Expression>]) -> String {
        args.iter()
            .map(|x| self.wrap("[", &self.expression(x), "]"))
            .collect::<Vec<String>>()
            .join("")
    }

    fn port_declaration(&self, port: &PortDeclaration) -> String {
        let signed = if port.signed {String::from("SIGNED")} else {String::new()};
        let direction = self.token(&port.direction);
        let kind = self.structure_type(&port.kind);
        let name = self.token(&port.name);
        let sizes = self.array_sizes(&port.sizes);
        signed + &String::from(" ") + &direction + &String::from(" ") +
            &kind + &String::from(" ") + &name + &sizes
    }

    fn port_declarations(&self, ports: &[PortDeclaration]) -> String {
        self.paren(&
            ports.iter()
                .map(|x| self.port_declaration(x))
                .collect::<Vec<String>>()
                .join(","))
    }

    fn constant_declaration(&self, c: &ConstantDeclaration) -> String {
        String::from("const ") + &self.token(&c.name) + &String::from(" = ") +
            &self.expression(&c.value) + ";"
    }

    fn type_declaration(&self, t: &TypeDeclaration) -> String {
        self.token(&t.name) + &self.array_sizes(&t.sizes)
    }

    fn type_declarations(&self, v : &[TypeDeclaration]) -> String {
        v.iter()
            .map(|x| self.type_declaration(x))
            .collect::<Vec<String>>()
            .join(",")
    }

    fn variable_declaration(&self, v : &[TypeDeclaration]) -> String {
        String::from (" var ") +
            &self.type_declarations(v) +
            ";"
    }

    fn signal_declaration(&self, s: &SignalDeclaration) -> String {
        let signed = if s.signed {"SIGNED"} else {""};
        String::from(signed) +
            " sig " +
            &self.structure_type(&s.kind) +
            &self.type_declarations(&s.vars) +
            ";"
    }

    fn name_list(&self, n: &[Token]) -> String {
        n.iter()
            .map(|x| self.token(&x))
            .collect::<Vec<String>>()
            .join(",")
    }

    fn connection(&self, c: &Connection) -> String {
        match c {
            Connection::SignalConnection{name, value} =>
                String::from(".") + &self.token(name) + &self.paren(&self.expression(value)),
            Connection::ParameterConnection {name, value} =>
                String::from("#") + &self.token(name) + &self.paren(&self.expression(value)),
        }
    }

    fn connection_list(&self, list: &[Connection]) -> String {
        list.iter()
            .map(|x| self.connection(x))
            .collect::<Vec<String>>()
            .join(",")
    }

    fn instance_connections(&self, i: &InstanceConnections) -> String {
        if i.is_empty() {
            String::new()
        } else {
            self.paren(&self.connection_list(i))
        }
    }

    fn fsm_declaration(&self, f: &FSMDeclaration) -> String {
        String::from("fsm ") +
            &self.token(&f.name) +
            &self.array_sizes(&f.sizes) +
            &self.instance_connections(&f.connections) +
            " = {" +
            &self.name_list(&f.states) +
            "};"
    }

    fn dff_single_declaration(&self, d: &DFFSingleDeclaration) -> String {
        self.token(&d.name)
            + &self.array_sizes(&d.sizes)
            + &self.instance_connections(&d.connections)
    }

    fn dff_single_declarations(&self, d: &[DFFSingleDeclaration]) -> String {
        d.iter()
            .map(|x| self.dff_single_declaration(x))
            .collect::<Vec<String>>()
            .join(",")
    }

    fn dff_declaration(&self, d: &DFFDeclaration) -> String {
        let signed = if d.signed {String::from("SIGNED")} else {String::new()};
        let kind = self.structure_type(&d.kind);
        let dffs = self.dff_single_declarations(&d.dffs);
        signed + " dff " + &kind + &dffs + ";"
    }

    fn module_instantiation(&self, m: &ModuleInstantiation) -> String {
        self.token(&m.modname) + " " +
            &self.token(&m.myname) + &self.array_sizes(&m.sizes)
            + &self.instance_connections(&m.connections) + ";"
    }

    fn case_element(&self, c: &CaseElement) -> String {
        match &c.kind {
            ExpressionOrDefault::Expression(e) => self.expression(e) + ": " + &self.always_statements(&c.statements),
            ExpressionOrDefault::Default => String::from("default: ") + &self.always_statements(&c.statements),
        }
    }

    fn case_elements(&self, v: &[CaseElement]) -> String {
        v.iter()
            .map(|x| self.case_element(x))
            .collect::<Vec<String>>()
            .join("\n  ")
    }

    fn case_statement(&self, c: &CaseStatement) -> String {
        String::from("case ") + &self.paren(&self.expression(&c.test)) +
            &self.wrap("{\n", &self.case_elements(&c.cases), "\n}\n")
    }

    fn if_statement(&self, i: &IfStatement) -> String {
        String::from("if ") + &self.paren(&self.expression(&i.test)) +
            &self.wrap("{", &self.always_statements(&i.block), "}")
        + &(if !i.else_block.is_empty() {
            String::from(" else ") +
                &self.wrap("{", &self.always_statements(&i.else_block), "}")
        } else {
            String::new()
        })
    }

    fn assign_statement(&self, a: &AssignStatement) -> String {
        self.signal(&a.signal) + " = " + &self.expression(&a.rhs) + ";"
    }

    fn signal_increment_decrement(&self, s: &SignalIncrementDecrement) -> String {
        self.signal(&s.signal) + &self.token(&s.operator)
    }

    fn variable_assign(&self, v: &VariableAssign) -> String {
        match v {
            VariableAssign::SignalIncrementDecrement(s) =>
                self.signal_increment_decrement(s),
            VariableAssign::AssignStatement(a) =>
                self.assign_statement(a),
        }
    }

    fn for_statement(&self, f: &ForStatement) -> String {
        String::from("for (") +
            &self.assign_statement(&f.init) +
            &self.expression(&f.test) +
            "; " + &self.variable_assign(&f.increment) + ") {\n"
            + &self.always_statements(&f.block) + "}\n"
    }

    fn always_statement(&self, s: &AlwaysStatement) -> String {
        match s {
            AlwaysStatement::AssignStatement(a) => self.assign_statement(a),
            AlwaysStatement::CaseStatement(c) => self.case_statement(c),
            AlwaysStatement::IfStatement(i) => self.if_statement(i),
            AlwaysStatement::ForStatement(f) => self.for_statement(f),
        }
    }

    fn always_statements(&self, b: &[AlwaysStatement]) -> String {
        b.iter()
            .map(|x| self.always_statement(x))
            .collect::<Vec<String>>()
            .join("\n")
    }

    fn always_block(&self, b: &[AlwaysStatement]) -> String {
        String::from("always {") +
            &self.always_statements(b) + "}\n"
    }

    fn assign_block_declaration(&self, d: &AssignBlockKind) -> String {
        match d {
            AssignBlockKind::ModuleInstantiation(m) => self.module_instantiation(m),
            AssignBlockKind::DFFDeclaration(d) => self.dff_declaration(d),
            AssignBlockKind::FSMDeclaration(f) => self.fsm_declaration(f),
            AssignBlockKind::AssignBlock(a) => self.assign_block(a),
        }
    }

    fn assign_block(&self, b: &AssignBlock) -> String {
        self.connection_list(&b.connections) +
            &self.wrap("{",
            &b.declarations.iter()
                .map(|x| self.assign_block_declaration(x))
                .collect::<Vec<String>>()
                .join(""),
            "}")

    }

    fn structure_member(&self, s: &StructMember) -> String {
        self.token(&s.name) + " " +
            &self.structure_type(&s.kind) +
            &self.array_sizes(&s.sizes)
    }

    fn structure_declaration(&self, s: &StructureDeclaration) -> String {
        String::from("struct ") +
            &self.token(&s.name) + &self.wrap("{",
        &s.members.iter()
            .map(|x| self.structure_member(x))
            .collect::<Vec<String>>()
            .join(";"),
            "}")
    }

    fn statement(&self, s: &Statement) -> String {
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

    fn body(&self, block: &[Statement]) -> String {
        block.iter()
            .map(|x| self.statement(x))
            .collect::<Vec<String>>()
            .join("\n")
    }

    fn module(&self, module: &ModuleBlock) -> String {
        let mut ret = String::new();
        let _ = write!(ret, "module {} {} {} {{{}}}", self.token(&module.name),
                       self.parameter_declarations(&module.params),
                       self.port_declarations(&module.ports),
                       self.body(&module.body));
        ret
    }

    fn global_statement(&self, s: &GlobalStatement) -> String {
        match s {
            GlobalStatement::StructureDeclaration(s) => self.structure_declaration(s),
            GlobalStatement::ConstantDeclaration(c) => self.constant_declaration(c),
        }
    }

    fn global_statements(&self, block: &[GlobalStatement]) -> String {
        block.iter()
            .map(|x| self.global_statement(x))
            .collect::<Vec<String>>()
            .join("\n")
    }

    fn global(&self, global: &GlobalBlock) -> String {
        String::from("global ") +
            &self.token(&global.name) +
            &self.wrap("{",
                       &self.global_statements(&global.statements)
                       , "}")
    }

    fn source_block(&self, block: &SourceBlock) -> String {
        match block {
            SourceBlock::GlobalBlock(g) => self.global(g),
            SourceBlock::ModuleBlock(m) => self.module(m),
        }
    }

    pub fn source(&self, blk: &[SourceBlock]) -> String {
        blk.iter()
            .map(|x| self.source_block(x))
            .collect::<Vec<String>>()
            .join("\n")
    }
}