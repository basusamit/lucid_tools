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
use super::token::Token;

#[derive(Clone, Debug, PartialEq)]
// bit_selector
//  : '[' expr ':' expr ']'            #BitSelectorConst
//  | '[' expr ('+'|'-') ':' expr ']'  #BitSelectorFixWidth
//  ;
pub enum BitSelector {
    // '[' expr ':' expr ']'            #BitSelectorConst
    ConstBitSelector {
        start: Box<Expression>,
        stop: Box<Expression>,
    },
    // '[' expr ('+'|'-') ':' expr ']'  #BitSelectorFixWidth
    FixedWidthBitSelector {
        start: Box<Expression>,
        sign: Token,
        width: Box<Expression>,
    },
}

#[derive(Clone, Debug, PartialEq)]
// array_index: '[' expr ']';
// bit_selection: array_index* (array_index | bit_selector);
pub struct BitSelection {
    pub arrays: Vec<Box<Expression>>,
    pub selector: Option<BitSelector>,
}


#[derive(Clone, Debug, PartialEq)]
pub struct ConstantStatement {
    pub name: Token,
    pub value: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
// type_dec: name array_size*;
pub struct TypeDeclaration {
    pub name: Token,
    pub sizes: ArraySizes,
}

#[derive(Clone, Debug, PartialEq)]
// number: HEX | BIN | DEC | INT | STRING;
// HEX: ([1-9][0-9]*)? 'h' ([0-9a-fA-FzZX]|('x' {_input.LA(1) != '{'}?))+;
// BIN: ([1-9][0-9]*)? 'b' ([0-1zZX]|('x' {_input.LA(1) != '{'}?))+;
// DEC: ([1-9][0-9]*)? 'd' [0-9]+;
// REAL: '-'? [0-9]* '.' [0-9]+ | '-'? [0-9]+ '.' [0-9]*;
// INT: [0-9]+;
// STRING: '"' ( '\\' ~[\r\n] | ~[\\"\r\n] )* '"';
pub enum Number {
    HexNumber(Token),
    BinaryNumber(Token),
    DecimalNumber(Token),
    IntegerNumber(Token),
    String(Token),
}

#[derive(Clone, Debug, PartialEq)]
pub struct NamedBitSelector {
    pub name: Token,
    pub selector: BitSelection,
}

pub type Signal = Vec<NamedBitSelector>;

#[derive(Clone, Debug, PartialEq)]
//expr
//  ;
pub enum Expression {
    //  : signal                                      #ExprSignal
    // signal: name bit_selection? ('.' name bit_selection?)*;
    SignalExpression(Signal),
    //  | number                                      #ExprNum
    NumberExpression(Number),
    // function: FUNCTION_ID '(' expr (',' expr)* ')';
    FunctionExpression {
        name: Token,
        arguments: Vec<Box<Expression>>,
    },
    //  | '(' expr ')'                                #ExprGroup
    ExpressionGroup(Box<Expression>),
    //  | 'c{' expr (',' expr)* '}'                   #ExprConcat
    ConcatenateExpression {
        expressions: Vec<Box<Expression>>,
    },
    //  | expr 'x{' expr '}'                          #ExprDup
    DuplicateExpression {
        multiplier: Box<Expression>,
        base: Box<Expression>,
    },
    //  | '{' expr (',' expr)* '}'                    #ExprArray
    ArrayExpression {
        elements: Vec<Box<Expression>>,
    },
    //  | '-' expr                                    #ExprNegate
    //  | ('|'|'&'|'~&'|'~|'|'^'|'~^') expr           #ExprCompress
    //  | ('~'|'!') expr                              #ExprInvert
    PrefixExpression {
        operator: Token,
        operand: Box<Expression>,
    },
    //  | expr ('*'|'/') expr                         #ExprMultDiv
    //  | expr ('+'|'-') expr                         #ExprAddSub
    //  | expr ('>>'|'<<'|'<<<'|'>>>') expr           #ExprShift
    //  | expr ('|'|'&'|'^'|'~^') expr                #ExprAndOr
    //  | expr ('<'|'>'|'=='|'!='|'>='|'<=') expr     #ExprCompare
    //  | expr ('||'|'&&') expr                       #ExprLogical
    InfixExpression {
        lhs: Box<Expression>,
        operator: Token,
        rhs: Box<Expression>,
    },
    //  | expr '?' expr ':' expr                      #ExprTernary
    TernaryExpression {
        selector: Box<Expression>,
        first: Box<Expression>,
        second: Box<Expression>,
    },
}

#[derive(Clone, Debug, PartialEq)]
// param_dec: param_name (':' param_constraint)?;
// param_name: name ('=' expr)?;
// param_constraint: expr;
pub struct ParameterDeclaration {
    pub name: Token,
    pub initial: Option<Box<Expression>>,
    pub constraint: Option<Box<Expression>>,
}

#[derive(Clone, Debug, PartialEq)]
// struct_type: '<' name ('.' name)? '>';
pub struct StructureType {
    pub names: Vec<Token>,
}

#[derive(Clone, Debug, PartialEq)]
// port_dec: input_dec | output_dec | inout_dec;
// input_dec: SIGNED? 'input' struct_type? name array_size*;
// output_dec: SIGNED? 'output' struct_type? name array_size*;
// inout_dec: SIGNED? 'inout' struct_type? name array_size*;
pub struct PortDeclaration {
    pub signed: bool,
    pub direction: Token,
    pub kind: Option<StructureType>,
    pub name: Token,
    pub sizes: Vec<Box<Expression>>,
}

// con_list  : connection (',' connection)*;
//type ConnectionList = Vec<Box<Connection>>;

#[derive(Clone, Debug, PartialEq)]
// connection: param_con | sig_con;
pub enum Connection {
    // sig_con: '.' name '(' expr ')';
    SignalConnection {
        name: Token,
        value: Box<Expression>,
    },
    // param_con: '#' name '(' (expr | REAL) ')';
    ParameterConnection {
        name: Token,
        value: Box<Expression>, // TODO - add handling of REAL
    }
}

// inst_cons : '(' con_list ')';
pub type InstanceConnections = Vec<Connection>;

#[derive(Clone, Debug, PartialEq)]
// struct_member: name struct_type? array_size*;
pub struct StructMember {
    pub name: Token,
    pub kind: Option<StructureType>,
    pub sizes: Vec<Box<Expression>>
}

// array_size: '[' expr ']';
type ArraySizes = Vec<Box<Expression>>;

#[derive(Clone, Debug, PartialEq)]
// dff_single: name array_size* inst_cons?;
pub struct DFFSingleDeclaration {
    pub name: Token,
    pub sizes: ArraySizes,
    pub connections: InstanceConnections,
}

#[derive(Clone, Debug, PartialEq)]
// fsm_dec: 'fsm' name array_size* inst_cons? '=' '{' fsm_states '}';
// fsm_states: name (',' name)*;
pub struct FSMDeclaration {
    pub name: Token,
    pub sizes: ArraySizes,
    pub connections: InstanceConnections,
    pub states: Vec<Token>,
}

#[derive(Clone, Debug, PartialEq)]
// dff_dec: SIGNED? 'dff' struct_type? dff_single (',' dff_single)*;
pub struct DFFDeclaration {
    pub signed: bool,
    pub kind: Option<StructureType>,
    pub dffs: Vec<DFFSingleDeclaration>,
}

#[derive(Clone, Debug, PartialEq)]
// module_inst: name name array_size* inst_cons?;
pub struct ModuleInstantiation {
    pub modname: Token,
    pub myname: Token,
    pub sizes: ArraySizes,
    pub connections: InstanceConnections,
}


#[derive(Clone, Debug, PartialEq)]
// case_elem: (expr | 'default') ':' always_stat+;
pub enum ExpressionOrDefault {
    Expression(Box<Expression>),
    Default,
}

#[derive(Clone, Debug, PartialEq)]
// case_elem: (expr | 'default') ':' always_stat+;
pub struct CaseElement {
    pub kind: ExpressionOrDefault,
    pub statements: Vec<AlwaysStatement>,
}

#[derive(Clone, Debug, PartialEq)]
// assign_stat: signal '=' expr;
pub struct AssignStatement {
    pub signal: Signal,
    pub rhs: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
//  : signal ('++'|'--')
pub struct SignalIncrementDecrement {
    pub signal: Signal,
    pub operator: Token,
}

#[derive(Clone, Debug, PartialEq)]
// var_assign
//  | assign_stat
//  ;
pub enum VariableAssign {
    SignalIncrementDecrement(SignalIncrementDecrement),
    AssignStatement(AssignStatement),
}

// block
//  : '{' always_stat* '}'
//  | always_stat
//  ;
pub type Block = Vec<AlwaysStatement>;

#[derive(Clone, Debug, PartialEq)]
// case_stat: 'case' '(' expr ')' '{' case_elem+ '}';
pub struct CaseStatement {
    pub test: Box<Expression>,
    pub cases: Vec<CaseElement>,
}

#[derive(Clone, Debug, PartialEq)]
// if_stat: 'if' '(' expr ')' block else_stat?;
// else_stat: 'else' block;
pub struct IfStatement {
    pub test: Box<Expression>,
    pub block: Block,
    pub else_block: Block,
}

#[derive(Clone, Debug, PartialEq)]
// for_stat: 'for' '(' assign_stat ';' expr ';' var_assign ')' block;
pub struct ForStatement {
    pub init: AssignStatement,
    pub test: Box<Expression>,
    pub increment: VariableAssign,
    pub block: Block,
}

#[derive(Clone, Debug, PartialEq)]
//always_stat
//  : assign_stat ';' #AlwaysStat
//  | case_stat       #AlwaysCase
//  | if_stat         #AlwaysIf
//  | for_stat        #AlwaysFor
//  ;
pub enum AlwaysStatement {
    AssignStatement(AssignStatement),
    CaseStatement(CaseStatement),
    IfStatement(IfStatement),
    ForStatement(ForStatement),
}

#[derive(Clone, Debug, PartialEq)]
// struct_dec: 'struct' name '{' struct_member (',' struct_member)* '}';
pub struct StructureDeclaration {
    pub name: Token,
    pub members: Vec<StructMember>,
}

#[derive(Clone, Debug, PartialEq)]
// const_dec: 'const' name '=' expr;
pub struct ConstantDeclaration {
    pub name: Token,
    pub value: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
// sig_dec: SIGNED? 'sig' struct_type? type_dec (',' type_dec)*;
pub struct SignalDeclaration {
    pub signed: bool,
    pub kind: Option<StructureType>,
    pub vars: Vec<TypeDeclaration>,
}

#[derive(Clone, Debug, PartialEq)]
// assign_block: con_list '{' ((dff_dec | fsm_dec | module_inst) ';' | assign_block)* '}';
pub enum AssignBlockKind {
    DFFDeclaration(DFFDeclaration),
    FSMDeclaration(FSMDeclaration),
    ModuleInstantiation(ModuleInstantiation),
    AssignBlock(AssignBlock),
}

#[derive(Clone, Debug, PartialEq)]
// assign_block: con_list '{' ((dff_dec | fsm_dec | module_inst) ';' | assign_block)* '}';
pub struct AssignBlock {
    pub connections: Vec<Connection>,
    pub declarations: Vec<AssignBlockKind>,
}


#[derive(Clone, Debug, PartialEq)]
// stat
//  : const_dec ';'   #StatConst
//  | var_dec ';'     #StatVar
//  | sig_dec ';'     #StatSig
//  | fsm_dec ';'     #StatFSM
//  | dff_dec ';'     #StatDFF
//  | module_inst ';' #StatModuleInst
//  | assign_block    #StatAssign
//  | always_block    #StatAlways
//  | struct_dec      #StatStruct
//  ;
pub enum Statement {
    ConstantDeclaration(ConstantDeclaration),
    // var_dec: 'var' type_dec (',' type_dec)*;
    VariableDeclaration(Vec<TypeDeclaration>),
    SignalDeclaration(SignalDeclaration),
    FSMDeclaration(FSMDeclaration),
    DFFDeclaration(DFFDeclaration),
    ModuleInstantiation(ModuleInstantiation),
    AssignBlock(AssignBlock),
    // always_block: 'always' block;
    AlwaysBlock(Block),
    StructureDeclaration(StructureDeclaration),
}

#[derive(Clone, Debug, PartialEq)]
//global_stat
//  : struct_dec
//  | const_dec ';'
//  ;
pub enum GlobalStatement {
    StructureDeclaration(StructureDeclaration),
    ConstantDeclaration(ConstantDeclaration),
}

#[derive(Clone, Debug, PartialEq)]
// module: 'module' name param_list? port_list module_body;
// param_list: '#(' param_dec (',' param_dec)* ')';
// port_list: '(' port_dec (',' port_dec)* ')';
// module_body: '{' stat* '}';
pub struct ModuleBlock {
    pub name: Token,
    pub params: Vec<ParameterDeclaration>,
    pub ports: Vec<PortDeclaration>,
    pub body: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
// global: 'global' name '{' global_stat* '}';
pub struct GlobalBlock {
    pub name: Token,
    pub statements: Vec<GlobalStatement>,
}


#[derive(Clone, Debug, PartialEq)]
pub enum SourceBlock {
    GlobalBlock(GlobalBlock),
    ModuleBlock(ModuleBlock),
}

// starting rule
// source: (global | module)* EOF;
pub type Source = Vec<SourceBlock>;



