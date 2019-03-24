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
use super::lexer::*;
use super::ast::*;
use super::token::*;
use super::error::ProgramError;
use std::fmt::Write;

//pub type ParserResult = Result<ASTPtr, ProgramError>;
pub type ExpressionResult = Result<Box<Expression>, ProgramError>;


pub struct Parser {
    lexer: Lexer,
}

// This is internal to the parser
enum CaseLabelOrAssignStatement {
    CaseLabel(ExpressionOrDefault),
    AssignStatement(AssignStatement),
}

impl Parser {
    pub fn from(lexer: Lexer) -> Parser {
        Parser {
           lexer
        }
    }

    // A bit selector is a series of array indices [expr][expr]
    // that optionally terminates in a constant range selector [expr:expr]
    // or a fix width range selector [expr +: expr] or [expr -: expr]
    // To parse this, we must look ahead after the expression is parsed
    //
    fn bit_selection(&mut self) -> Result<BitSelection, ProgramError> {
        let mut arrays = vec![];
        let mut selector = None;
        let mut expr_saved = None ;
        // Start in array mode
        while self.lexer.next_is(&TokenKind::LBRACKET) {
            // First consume the opening bracket
            let _ = self.lexer.consume()?;
            // Next consume the expression
            let expr = self.expression()?;
            // Is the next token a RBRACKET?  If so then we are
            // parsing array indices.  Consume it and add it to the
            // array
            if self.lexer.next_is(&TokenKind::RBRACKET) {
                let _ = self.lexer.consume()?;
                arrays.push(expr);
            } else {
                // No it is not.  So then save the expression
                expr_saved = Some(expr);
                // Exit the loop
                break;
            }
        }
        // If the next token is not a +:, -: or :, then return
        // if it is, then consume, build the BitSelector
        if expr_saved.is_some() && (self.lexer.next_is(&TokenKind::PLUSCOLON) || self.lexer.next_is(&TokenKind::NEGCOLON)) {
            let sign = self.lexer.consume()?;
            let width = self.expression()?;
            let _ = self.lexer.match_token(&TokenKind::RBRACKET);
            selector = Some(BitSelector::FixedWidthBitSelector {
                start : expr_saved.unwrap(),
                sign,
                width
            });
        } else if expr_saved.is_some() && self.lexer.next_is(&TokenKind::COLON) {
            let _ = self.lexer.match_token(&TokenKind::COLON);
            let stop = self.expression()?;
            let _ = self.lexer.match_token(&TokenKind::RBRACKET);
            selector = Some(BitSelector::ConstBitSelector {
                start: expr_saved.unwrap(),
                stop
            });
        }
        Ok(BitSelection {
            arrays,
            selector
        })
    }

    fn named_bit_selector(&mut self) -> Result<NamedBitSelector, ProgramError> {
        let name = self.lexer.match_token(&TokenKind::IDENTIFIER)?;
        let selector = self.bit_selection()?;
        Ok(NamedBitSelector{
            name,
            selector
        })
    }

    fn signal(&mut self) -> Result<Signal, ProgramError> {
        let mut ret = vec![self.named_bit_selector()?];
        while self.lexer.next_is(&TokenKind::DOT) {
            let _ = self.lexer.consume()?;
            ret.push(self.named_bit_selector()?);
        }
        Ok(ret)
    }

    fn signal_expression(&mut self) -> ExpressionResult {
        Ok(Box::from(Expression::SignalExpression(self.signal()?)))
    }

    fn expression_list(&mut self) -> Result<Vec<Box<Expression>>, ProgramError> {
        let mut elements = vec![self.exp(0)?];
        while self.lexer.next_is(&TokenKind::COMMA) {
            let _ = self.lexer.consume()?;
            elements.push(self.exp(0)?);
        }
        Ok(elements)
    }

    fn concatenate_expression(&mut self) -> ExpressionResult {
        let _ = self.lexer.match_token(&TokenKind::CONCATENATE)?;
        let expressions = self.expression_list()?;
        let _ = self.lexer.match_token(&TokenKind::RBRACE)?;
        Ok(Box::from(Expression::ConcatenateExpression {
            expressions
        }))
    }

    fn array_expression(&mut self) -> ExpressionResult {
        let _ = self.lexer.match_token(&TokenKind::LBRACE)?;
        let elements = self.expression_list()?;
        let _ = self.lexer.match_token(&TokenKind::RBRACE)?;
        Ok(Box::from(Expression::ArrayExpression {
            elements
        }))
    }

    fn group_expression(&mut self) -> ExpressionResult {
        let _ = self.lexer.consume()?;
        let exp = self.exp(0)?;
        let _ = self.lexer.match_token(&TokenKind::RPAREN)?;
        Ok(Box::from(Expression::ExpressionGroup(exp)))
    }

    fn function_expression(&mut self) -> ExpressionResult {
        let name = self.lexer.match_token(&TokenKind::FUNCTION)?;
        let _ = self.lexer.match_token(&TokenKind::LPAREN)?;
        let arguments = self.expression_list()?;
        let _ = self.lexer.match_token(&TokenKind::RPAREN)?;
        Ok(Box::from(Expression::FunctionExpression {
            name,
            arguments
        }))
    }

    fn exp(&mut self, precedence: usize) -> ExpressionResult {
        let mut t = self.primary_expression()?;
        while self.lexer.next_token().is_operator() && self.lexer.next_token().precedence() >= precedence {
            let opr_save = self.lexer.consume()?;
            match opr_save.kind {
                TokenKind::DUPLICATE => {
                    let t1 = self.exp(0)?;
                    let _ = self.lexer.match_token(&TokenKind::RBRACE);
                    let root = Box::from(Expression::DuplicateExpression {
                        multiplier: t,
                        base: t1
                    });
                    t = root;
                },
                TokenKind::TERNARY => {
                    let first = self.exp(0)?;
                    let _ = self.lexer.match_token(&TokenKind::COLON);
                    let second = self.exp(0)?;
                    let root = Box::from(Expression::TernaryExpression {
                        selector: t,
                        first,
                        second
                    });
                    t = root;
                },
                _ => {
                    let q = 1 + opr_save.precedence();
                    let t1 = self.exp(q)?;
                    let root = Box::from(Expression::InfixExpression {
                        lhs: t,
                        operator: opr_save,
                        rhs: t1
                    });
                    t = root;
                }
            }
        }
        Ok(t)
    }

    fn expression(&mut self) -> ExpressionResult {
        self.exp(0)
    }

    fn primary_expression(&mut self) -> ExpressionResult {
        let token = self.lexer.next_token();
        if token.is_unary_op() {
            let operator = self.lexer.consume()?;
            let operand = self.exp(operator.precedence())?;
            return Ok(Box::from(Expression::PrefixExpression {
                operator,
                operand,
            }));
        }
        match token.kind {
            TokenKind::NUMBERINT =>
                Ok(Box::from(Expression::NumberExpression(
                    Number::IntegerNumber(self.lexer.consume()?)))),
            TokenKind::NUMBERHEX =>
                Ok(Box::from(Expression::NumberExpression(
                    Number::HexNumber(self.lexer.consume()?)))),
            TokenKind::NUMBERBIN =>
                Ok(Box::from(Expression::NumberExpression(
                    Number::BinaryNumber(self.lexer.consume()?)))),
            TokenKind::NUMBERDEC =>
                Ok(Box::from(Expression::NumberExpression(
                    Number::DecimalNumber(self.lexer.consume()?)))),
            TokenKind::IDENTIFIER => self.signal_expression(),
            TokenKind::LPAREN => self.group_expression(),
            TokenKind::LBRACE => self.array_expression(),
            TokenKind::CONCATENATE => self.concatenate_expression(),
            TokenKind::FUNCTION => self.function_expression(),
            _ => {
                let mut msg = String::new();
                let _ = write!(msg, "Unexpected token in expression: {:?}", token);
                Err(ProgramError::of("Invalid expression", &msg))
            }
        }
    }

    fn parameter_declaration(&mut self) -> Result<ParameterDeclaration, ProgramError> {
        let name = self.lexer.match_token(&TokenKind::IDENTIFIER)?;
        let initial = if self.lexer.next_is(&TokenKind::ASSIGN) {
            let _ = self.lexer.consume()?;
            Some(self.expression()?)
        } else {
            None
        };
        let constraint = if self.lexer.next_is(&TokenKind::COLON) {
            let _ = self.lexer.consume()?;
            Some(self.expression()?)
        } else {
            None
        };
        Ok(ParameterDeclaration{
            name,
            initial,
            constraint,
        })
    }

    fn param_list(&mut self) -> Result<Vec<ParameterDeclaration>, ProgramError> {
        let mut ret = vec![];
        if self.lexer.next_is(&TokenKind::PARAMSTART) {
            let _ = self.lexer.consume()?;
            ret.push(self.parameter_declaration()?);
            while self.lexer.next_is(&TokenKind::COMMA) {
                let _ = self.lexer.consume()?;
                ret.push(self.parameter_declaration()?);
            }
            let _ = self.lexer.match_token(&TokenKind::RPAREN)?;
        }
        Ok(ret)
    }

    fn struct_type(&mut self) -> Result<Option<StructureType>, ProgramError> {
        if !self.lexer.next_is(&TokenKind::LT) {
            return Ok(None);
        }
        let _ = self.lexer.consume()?;
        let mut names = vec![self.lexer.match_token(&TokenKind::IDENTIFIER)?];
        while self.lexer.next_is(&TokenKind::DOT) {
            let _ = self.lexer.consume();
            names.push(self.lexer.match_token(&TokenKind::IDENTIFIER)?);
        }
        let _ = self.lexer.match_token(&TokenKind::GT);
        Ok(Some(StructureType {
            names,
        }))
    }

    fn array_sizes(&mut self) -> Result<Vec<Box<Expression>>, ProgramError> {
        let mut ret = vec![];
        while self.lexer.next_is(&TokenKind::LBRACKET) {
            let _ = self.lexer.consume()?;
            ret.push(self.expression()?);
            let _ = self.lexer.match_token(&TokenKind::RBRACKET)?;
        }
        Ok(ret)
    }

    fn port_declaration(&mut self) -> Result<PortDeclaration, ProgramError> {
        let signed = self.lexer.next_is(&TokenKind::SIGNED);
        if signed {
            let _ = self.lexer.consume()?;
        }
        let direction = self.lexer.match_token_from_list(&[&TokenKind::INPUT,&TokenKind::OUTPUT,&TokenKind::INOUT])?;
        let kind = self.struct_type()?;
        let name = self.lexer.match_token(&TokenKind::IDENTIFIER)?;
        let sizes = self.array_sizes()?;
        Ok(PortDeclaration{
            signed,
            direction,
            kind,
            name,
            sizes,
        })
    }

    fn port_list(&mut self) -> Result<Vec<PortDeclaration>, ProgramError> {
        let mut ret = vec![];
        let _ = self.lexer.match_token(&TokenKind::LPAREN);
        while !self.lexer.next_is(&TokenKind::RPAREN) {
            ret.push(self.port_declaration()?);
            if !self.lexer.next_is(&TokenKind::COMMA) {
                break;
            }
            let _ = self.lexer.match_token(&TokenKind::COMMA)?;
        }
        let _ = self.lexer.match_token(&TokenKind::RPAREN);
        Ok(ret)
    }

    fn constant_statement(&mut self) -> Result<ConstantDeclaration, ProgramError> {
        let _ = self.lexer.match_token(&TokenKind::CONST)?;
        let name = self.lexer.match_token(&TokenKind::IDENTIFIER)?;
        let _ = self.lexer.match_token(&TokenKind::ASSIGN)?;
        let value = self.expression()?;
        let _ = self.lexer.match_token(&TokenKind::SEMICOLON)?;
        Ok(ConstantDeclaration {name, value})
    }

    fn type_declaration(&mut self) -> Result<TypeDeclaration, ProgramError> {
        let name = self.lexer.match_token(&TokenKind::IDENTIFIER)?;
        let sizes = self.array_sizes()?;
        Ok(TypeDeclaration{name, sizes})
    }

    fn type_declaration_list(&mut self) -> Result<Vec<TypeDeclaration>, ProgramError> {
        let mut ret = vec![self.type_declaration()?];
        while self.lexer.next_is(&TokenKind::COMMA) {
            self.lexer.match_token(&TokenKind::COMMA)?;
            ret.push(self.type_declaration()?);
        }
        Ok(ret)
    }

    fn variable_declaration(&mut self) -> Result<Vec<TypeDeclaration>, ProgramError> {
        let _ = self.lexer.match_token(&TokenKind::VAR)?;
        let ret = self.type_declaration_list()?;
        let _ = self.lexer.match_token(&TokenKind::SEMICOLON)?;
        Ok(ret)
    }

    fn signal_declaration(&mut self, signed: bool) -> Result<SignalDeclaration, ProgramError> {
        let _ = self.lexer.match_token(&TokenKind::SIG)?;
        let kind = self.struct_type()?;
        let vars = self.type_declaration_list()?;
        let _ = self.lexer.match_token(&TokenKind::SEMICOLON)?;
        Ok(SignalDeclaration{signed, kind, vars})
    }

    fn dff_single_declaration(&mut self) -> Result<DFFSingleDeclaration, ProgramError> {
        let name = self.lexer.match_token(&TokenKind::IDENTIFIER)?;
        let sizes = self.array_sizes()?;
        let connections = self.instance_connections()?;
        Ok(DFFSingleDeclaration{name, sizes, connections})
    }

    fn dff_single_declarations(&mut self) -> Result<Vec<DFFSingleDeclaration>, ProgramError> {
        let mut ret = vec![self.dff_single_declaration()?];
        while self.lexer.next_is(&TokenKind::COMMA) {
            let _ = self.lexer.match_token(&TokenKind::COMMA);
            ret.push(self.dff_single_declaration()?);
        }
        let _ = self.lexer.match_token(&TokenKind::SEMICOLON)?;
        Ok(ret)
    }

    fn dff_declaration(&mut self, signed: bool) -> Result<DFFDeclaration, ProgramError> {
        let _ = self.lexer.match_token(&TokenKind::DFF);
        let kind = self.struct_type()?;
        let dffs = self.dff_single_declarations()?;
        Ok(DFFDeclaration{signed, kind, dffs})
    }

    fn name_list(&mut self) -> Result<Vec<Token>, ProgramError> {
        let mut ret = vec![self.lexer.match_token(&TokenKind::IDENTIFIER)?];
        while self.lexer.next_is(&TokenKind::COMMA) {
            let _ = self.lexer.match_token(&TokenKind::COMMA)?;
            ret.push(self.lexer.match_token(&TokenKind::IDENTIFIER)?);
        }
        Ok(ret)
    }

    fn signal_connection(&mut self) -> Result<Connection, ProgramError> {
        let _ = self.lexer.match_token(&TokenKind::DOT)?;
        let name = self.lexer.match_token(&TokenKind::IDENTIFIER)?;
        let _ = self.lexer.match_token(&TokenKind::LPAREN)?;
        let value = self.expression()?;
        let _ = self.lexer.match_token(&TokenKind::RPAREN)?;
        Ok(Connection::SignalConnection{name, value})
    }

    fn parameter_connection(&mut self) -> Result<Connection, ProgramError> {
        let _ = self.lexer.match_token(&TokenKind::HASH)?;
        let name = self.lexer.match_token(&TokenKind::IDENTIFIER)?;
        let _ = self.lexer.match_token(&TokenKind::LPAREN)?;
        let value = self.expression()?;
        let _ = self.lexer.match_token(&TokenKind::RPAREN)?;
        Ok(Connection::ParameterConnection{name, value})
    }

    fn connection(&mut self) -> Result<Connection, ProgramError> {
        match self.lexer.next_token().kind {
            TokenKind::DOT => Ok(self.signal_connection()?),
            TokenKind::HASH => Ok(self.parameter_connection()?),
            _ => Err(ProgramError::of("connection", "unrecognized connection syntax, expected . or #"))
        }
    }

    fn connection_list(&mut self) -> Result<Vec<Connection>, ProgramError> {
        let mut ret = vec![];
        while self.lexer.next_is_in(&[&TokenKind::DOT, &TokenKind::HASH]) {
            ret.push(self.connection()?);
            if !self.lexer.next_is(&TokenKind::COMMA) {
                return Ok(ret);
            }
            let _ = self.lexer.match_token(&TokenKind::COMMA)?;
        }
        Ok(ret)
    }

    fn instance_connections(&mut self) -> Result<InstanceConnections, ProgramError> {
        let mut ret = vec![];
        if self.lexer.next_is(&TokenKind::LPAREN) {
            let _ = self.lexer.match_token(&TokenKind::LPAREN);
            ret = self.connection_list()?;
            let _ = self.lexer.match_token(&TokenKind::RPAREN);
        }
        Ok(ret)
    }

    fn fsm_declaration(&mut self) -> Result<FSMDeclaration, ProgramError> {
        let _ = self.lexer.match_token(&TokenKind::FSM)?;
        let name = self.lexer.match_token(&TokenKind::IDENTIFIER)?;
        let sizes = self.array_sizes()?;
        let connections = self.instance_connections()?;
        let _ = self.lexer.match_token(&TokenKind::ASSIGN)?;
        let _ = self.lexer.match_token(&TokenKind::LBRACE)?;
        let states = self.name_list()?;
        let _ = self.lexer.match_token(&TokenKind::RBRACE)?;
        let _ = self.lexer.match_token(&TokenKind::SEMICOLON)?;
        Ok(FSMDeclaration{name,sizes,connections,states})
    }

    fn signed_signal_or_dff(&mut self) -> Result<Statement, ProgramError> {
        let _ = self.lexer.match_token(&TokenKind::SIGNED)?;
        match self.lexer.next_token().kind {
            TokenKind::SIG => Ok(Statement::SignalDeclaration(self.signal_declaration(true)?)),
            TokenKind::DFF => Ok(Statement::DFFDeclaration(self.dff_declaration(true)?)),
            _ => Err(ProgramError::of("statement", "After SIGNED, expect sig or dff"))
        }
    }

    fn module_instantiation(&mut self) -> Result<ModuleInstantiation, ProgramError> {
        let modname = self.lexer.match_token(&TokenKind::IDENTIFIER)?;
        let myname = self.lexer.match_token(&TokenKind::IDENTIFIER)?;
        let sizes = self.array_sizes()?;
        let connections = self.instance_connections()?;
        let _ = self.lexer.match_token(&TokenKind::SEMICOLON)?;
        Ok(ModuleInstantiation{modname, myname, sizes, connections})
    }

    fn assign_statement(&mut self) -> Result<AssignStatement, ProgramError> {
        let signal = self.signal()?;
        let _ = self.lexer.match_token(&TokenKind::ASSIGN);
        let rhs = self.expression()?;
        let _ = self.lexer.match_token(&TokenKind::SEMICOLON)?;
        Ok(AssignStatement{signal, rhs})
    }

    fn if_statement(&mut self) -> Result<IfStatement, ProgramError> {
        let _ = self.lexer.match_token(&TokenKind::IF)?;
        let _ = self.lexer.match_token(&TokenKind::LPAREN)?;
        let test = self.expression()?;
        let _ = self.lexer.match_token(&TokenKind::RPAREN)?;
        let block = self.block()?;
        let else_block = if self.lexer.next_is(&TokenKind::ELSE) {
            let _ = self.lexer.match_token(&TokenKind::ELSE);
            self.block()?
        } else {
            vec![]
        };
        Ok(IfStatement{test, block, else_block})
    }

    fn expression_or_default(&mut self) -> Result<ExpressionOrDefault, ProgramError> {
        if self.lexer.next_is(&TokenKind::DEFAULT) {
            let _ = self.lexer.match_token(&TokenKind::DEFAULT);
            Ok(ExpressionOrDefault::Default)
        } else {
            Ok(ExpressionOrDefault::Expression(self.expression()?))
        }
    }

    fn case_label_or_assignment(&mut self) -> Result<CaseLabelOrAssignStatement, ProgramError> {
        // We start with an IDENTIFIER that has already been parsed.
        // We will revert the parse to the start of that identifier.
        let ptr = self.lexer.next_token().start();
        // Parse the signal definition
        let signal = self.signal()?;
        match self.lexer.next_token().kind {
            TokenKind::ASSIGN => {
                self.lexer.restore(ptr);
                Ok(CaseLabelOrAssignStatement::AssignStatement(self.assign_statement()?))
            }
            TokenKind::COLON => {
                Ok(CaseLabelOrAssignStatement::CaseLabel(ExpressionOrDefault::Expression(Box::from(Expression::SignalExpression(signal)))))
            }
            _ => {
                self.lexer.restore(ptr);
                let expr = self.expression()?;
                Ok(CaseLabelOrAssignStatement::CaseLabel(ExpressionOrDefault::Expression(expr)))
            }
        }
    }

    // When parsing cases, there is an ambiguity that involves deciding if
    // IDENTIFIER is the start of an Assign or a CaseElement.
    // Either way, it can be parsed as a Signal.  The next token then indicates
    // which it is
    // case_elem: (expr | 'default') ':' always_stat+;
    // To handle this, we start with a single statement
    fn case_elements(&mut self) -> Result<Vec<CaseElement>, ProgramError> {
        let mut skip_kind = false;
        let mut kind = ExpressionOrDefault::Default;
        let mut ret = vec![];
        while !self.lexer.next_is(&TokenKind::RBRACE) {
            if !skip_kind {
                kind = self.expression_or_default()?
            }
            let _ = self.lexer.match_token(&TokenKind::COLON);
            let mut statements = vec![];
            statements.push(self.always_statement()?);
            loop {
                match self.lexer.next_token().kind {
                    TokenKind::RBRACE => {
                        ret.push(CaseElement{kind: kind.clone(), statements});
                        skip_kind = false;
                        break
                    },
                    TokenKind::DEFAULT => {
                        ret.push(CaseElement{kind: kind.clone(), statements});
                        skip_kind = false;
                        break;
                    },
                    TokenKind::NUMBER_STRING | TokenKind::NUMBERBIN | TokenKind::NUMBERHEX
                    | TokenKind::NUMBERDEC | TokenKind::NUMBERINT => {
                        ret.push(CaseElement{kind: kind.clone(), statements});
                        skip_kind = false;
                        break;
                    }
                    TokenKind::IDENTIFIER => {
                        let temp = self.case_label_or_assignment()?;
                        match temp {
                            CaseLabelOrAssignStatement::CaseLabel(k) => {
                                ret.push(CaseElement{ kind, statements });
                                kind = k.clone();
                                skip_kind = true;
                                break;
                            },
                            CaseLabelOrAssignStatement::AssignStatement(a) => {
                                statements.push(AlwaysStatement::AssignStatement(a));
                            },
                        }
                    }
                    _ => statements.push(self.always_statement()?)
                }
            }
        }
        Ok(ret)
    }

    fn case_statement(&mut self) -> Result<CaseStatement, ProgramError> {
        let _ = self.lexer.match_token(&TokenKind::CASE)?;
        let _ = self.lexer.match_token(&TokenKind::LPAREN)?;
        let test = self.expression()?;
        let _ = self.lexer.match_token(&TokenKind::RPAREN)?;
        let _ = self.lexer.match_token(&TokenKind::LBRACE)?;
        let cases = self.case_elements()?;
        let _ = self.lexer.match_token(&TokenKind::RBRACE)?;
        Ok(CaseStatement{test, cases})
    }

    fn variable_assign(&mut self) -> Result<VariableAssign, ProgramError> {
        let signal = self.signal()?;
        if self.lexer.next_is(&TokenKind::ASSIGN) {
            let _ = self.lexer.match_token(&TokenKind::ASSIGN)?;
            let rhs = self.expression()?;
            Ok(VariableAssign::AssignStatement(AssignStatement{signal, rhs}))
        } else {
            let operator = self.lexer.match_token_from_list(&[&TokenKind::PLUSPLUS, &TokenKind::MINUSMINUS])?;
            Ok(VariableAssign::SignalIncrementDecrement(SignalIncrementDecrement{ signal, operator }))
        }
    }

    fn for_statement(&mut self) -> Result<ForStatement, ProgramError> {
        let _ = self.lexer.match_token(&TokenKind::FOR)?;
        let _ = self.lexer.match_token(&TokenKind::LPAREN)?;
        let init = self.assign_statement()?;
        let test = self.expression()?;
        let _ = self.lexer.match_token(&TokenKind::SEMICOLON)?;
        let increment = self.variable_assign()?;
        let _ = self.lexer.match_token(&TokenKind::RPAREN)?;
        let block = self.block()?;
        Ok(ForStatement{init, test, increment, block})
    }

    fn always_statement(&mut self) -> Result<AlwaysStatement, ProgramError> {
        match self.lexer.next_token().kind {
            TokenKind::IDENTIFIER => Ok(AlwaysStatement::AssignStatement(self.assign_statement()?)),
            TokenKind::IF => Ok(AlwaysStatement::IfStatement(self.if_statement()?)),
            TokenKind::CASE => Ok(AlwaysStatement::CaseStatement(self.case_statement()?)),
            TokenKind::FOR => Ok(AlwaysStatement::ForStatement(self.for_statement()?)),
            _ => {
                let mut msg = String::new();
                let _ = write!(msg,"Expected identifier for assignment, if, case, or for, got {:?}", self.lexer.next_token());
                Err(ProgramError::of("statement", &msg))
            }
        }
    }

    fn block(&mut self) -> Result<Block, ProgramError> {
        let mut ret = vec![];
        if self.lexer.next_is(&TokenKind::LBRACE) {
            let _ = self.lexer.match_token(&TokenKind::LBRACE);
            while !self.lexer.next_is(&TokenKind::RBRACE) {
                ret.push(self.always_statement()?);
            }
            let _ = self.lexer.match_token(&TokenKind::RBRACE);
        } else {
            ret.push(self.always_statement()?);
        }
        Ok(ret)
    }

    fn always_block(&mut self) -> Result<Block, ProgramError> {
        let _ = self.lexer.match_token(&TokenKind::ALWAYS);
        let block = self.block()?;
        Ok(block)
    }

    fn structure_member(&mut self) -> Result<StructMember, ProgramError> {
        let name = self.lexer.match_token(&TokenKind::IDENTIFIER)?;
        let kind = self.struct_type()?;
        let sizes = self.array_sizes()?;
        Ok(StructMember{name, kind, sizes})
    }

    fn structure_members(&mut self) -> Result<Vec<StructMember>, ProgramError> {
        let mut ret = vec![self.structure_member()?];
        while self.lexer.next_is(&TokenKind::COMMA) {
            let _ = self.lexer.match_token(&TokenKind::COMMA);
            ret.push(self.structure_member()?);
        }
        Ok(ret)
    }

    fn structure_declaration(&mut self) -> Result<StructureDeclaration, ProgramError> {
        let _ = self.lexer.match_token(&TokenKind::STRUCT)?;
        let name = self.lexer.match_token(&TokenKind::IDENTIFIER)?;
        let _ = self.lexer.match_token(&TokenKind::LBRACE)?;
        let members = self.structure_members()?;
        let _ = self.lexer.match_token(&TokenKind::RBRACE)?;
        Ok(StructureDeclaration{name,members})
    }

    fn assign_block(&mut self) -> Result<AssignBlock, ProgramError> {
        let connections = self.connection_list()?;
        let _ = self.lexer.match_token(&TokenKind::LBRACE);
        let mut declarations = vec![];
        while !self.lexer.next_is(&TokenKind::RBRACE) {
            match self.lexer.next_token().kind {
                TokenKind::SIGNED => {
                    let _ = self.lexer.match_token(&TokenKind::SIGNED)?;
                    declarations.push(AssignBlockKind::DFFDeclaration(self.dff_declaration(true)?));
                },
                TokenKind::DFF => declarations.push(AssignBlockKind::DFFDeclaration(self.dff_declaration(false)?)),
                TokenKind::FSM => declarations.push(AssignBlockKind::FSMDeclaration(self.fsm_declaration()?)),
                TokenKind::IDENTIFIER => declarations.push(AssignBlockKind::ModuleInstantiation(self.module_instantiation()?)),
                TokenKind::DOT | TokenKind::HASH => declarations.push(AssignBlockKind::AssignBlock(self.assign_block()?)),
                TokenKind::RBRACE => break,
                _ => return Err(ProgramError::of("assign_block", "Unrecognisted statement type in assign block"))
            }
        }
        let _ = self.lexer.match_token(&TokenKind::RBRACE);
        Ok(AssignBlock{connections, declarations})
    }

    fn statement(&mut self) -> Result<Statement, ProgramError> {
        match self.lexer.next_token().kind {
            TokenKind::CONST => Ok(Statement::ConstantDeclaration(self.constant_statement()?)),
            TokenKind::VAR => Ok(Statement::VariableDeclaration(self.variable_declaration()?)),
            TokenKind::SIGNED => Ok(self.signed_signal_or_dff()?),
            TokenKind::SIG => Ok(Statement::SignalDeclaration(self.signal_declaration(false)?)),
            TokenKind::FSM => Ok(Statement::FSMDeclaration(self.fsm_declaration()?)),
            TokenKind::DFF => Ok(Statement::DFFDeclaration(self.dff_declaration(false)?)),
            TokenKind::IDENTIFIER => Ok(Statement::ModuleInstantiation(self.module_instantiation()?)),
            TokenKind::ALWAYS => Ok(Statement::AlwaysBlock(self.always_block()?)),
            TokenKind::DOT | TokenKind::HASH => Ok(Statement::AssignBlock(self.assign_block()?)),
            TokenKind::STRUCT => Ok(Statement::StructureDeclaration(self.structure_declaration()?)),
            _ => Err(ProgramError::of("statement","Unrecognized statement type"))
        }
    }

    fn module_body(&mut self) -> Result<Vec<Statement>, ProgramError> {
        let mut ret = vec![];
        let _ = self.lexer.match_token(&TokenKind::LBRACE);
        while !self.lexer.next_is(&TokenKind::RBRACE) {
            ret.push(self.statement()?)
        }
        let _ = self.lexer.match_token(&TokenKind::RBRACE);
        Ok(ret)
    }

    fn module(&mut self) -> Result<ModuleBlock, ProgramError> {
        let _ = self.lexer.match_token(&TokenKind::MODULE)?;
        let name = self.lexer.match_token(&TokenKind::IDENTIFIER)?;
        let params = self.param_list()?;
        let ports = self.port_list()?;
        let body = self.module_body()?;
        Ok(ModuleBlock {name, params, ports, body})
    }

    fn global_statement(&mut self) -> Result<GlobalStatement, ProgramError> {
        match self.lexer.next_token().kind {
            TokenKind::CONST => Ok(GlobalStatement::ConstantDeclaration(self.constant_statement()?)),
            TokenKind::STRUCT => Ok(GlobalStatement::StructureDeclaration(self.structure_declaration()?)),
            _ => Err(ProgramError::of("global_statement", "Unrecognized global statement type"))
        }
    }

    fn global(&mut self) -> Result<GlobalBlock, ProgramError> {
        let mut statements = vec![];
        let _ = self.lexer.match_token(&TokenKind::GLOBAL)?;
        let name = self.lexer.match_token(&TokenKind::IDENTIFIER)?;
        let _ = self.lexer.match_token(&TokenKind::LBRACE)?;
        while !self.lexer.next_is(&TokenKind::RBRACE) {
            statements.push(self.global_statement()?)
        }
        let _ = self.lexer.match_token(&TokenKind::RBRACE);
        Ok(GlobalBlock {name, statements})
    }

    pub fn source(&mut self) -> Result<Source, ProgramError> {
        let mut source = vec![];
        let _ = self.lexer.match_token(&TokenKind::SOF);
        loop {
            match self.lexer.next_token().kind {
                TokenKind::GLOBAL => source.push(SourceBlock::GlobalBlock(self.global()?)),
                TokenKind::MODULE => source.push(SourceBlock::ModuleBlock(self.module()?)),
                TokenKind::EOF => break,
                _ => return Err(ProgramError::of("sourceblock", "Unknown source block type")),
            }
        }
        Ok(source)
    }
}


