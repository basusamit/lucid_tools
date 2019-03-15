use std::slice::Iter;

#[derive(Clone, Debug, PartialEq)]
pub struct TextRange {
    pos: usize,
    end: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    IDENTIFIER,
    FUNCTION,
    NUMBER,
    PLUS,
    MINUS,
    MULTIPLY,
    DIVIDE,
    LPAREN,
    RPAREN,
    LBRACKET,
    RBRACKET,
    LOGICALNOT,
    BITNOT,
    BITAND,
    BITOR,
    BITNAND,
    BITNOR,
    BITXOR,
    BITXNOR,
    LBRACE,
    RBRACE,
    LEFTSHIFT,
    LEFTSIGNEDSHIFT,
    RIGHTSHIFT,
    RIGHTSIGNEDSHIFT,
    GT,
    GE,
    LT,
    LE,
    EQ,
    NEQ,
    LOGICALAND,
    LOGICALOR,
    TERNARY,
    COLON,
    ASSIGN,
    EOF,
    COMMA,
    SEMICOLON,
    SOF,
    MODULE,
    INPUT,
    OUTPUT,
    INOUT,
    SIG,
    DFF,
    FSM,
    CONST,
    CASE,
    DEFAULT,
    FOR,
    VAR,
    DUPLICATE,
    CONCATENATE,
    DOT,
    PLUSCOLON,
    NEGCOLON,
    PARAMSTART,
    SIGNED,
    HASH,
    ALWAYS,
    IF,
    ELSE,
    PLUSPLUS,
    MINUSMINUS,
    STRUCT,
    GLOBAL,
}

#[derive(Clone, Debug, PartialEq)]
pub enum OperatorType {
    None,
    Prefix,
    Infix,
    InfixOrPrefix
}

#[derive(Clone, Debug, PartialEq)]
pub struct TokenInfo {
    pub kind: TokenKind,
    pub text: &'static str,
    pub precedence: usize,
    pub opkind: OperatorType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub range: TextRange,
}

impl TextRange {
    pub fn from(start: usize, end: usize) -> TextRange {
        TextRange{pos: start, end}
    }

    pub fn empty() -> TextRange {
        TextRange::from(0, 0)
    }

    pub fn text(&self, arg: &str) -> String {
        let mut result = String::new();
        for ndx in self.pos..self.end {
            result.push(arg.as_bytes()[ndx] as char);
        }
        result
    }

    pub fn start(&self) -> usize {self.pos}
}

impl Token {
    pub fn from_kind_and_positions(kind: TokenKind, start: usize, end: usize) -> Token {
        let range = TextRange::from(start, end);
        Token {
            kind,
            range
        }
    }

    pub fn from_kind_and_range(kind: TokenKind, range: TextRange) -> Token {
        Token {
            kind,
            range
        }
    }

    pub fn eof() -> Token {
        Token {
            kind: TokenKind::EOF,
            range: TextRange::from(0, 0),
        }
    }

    pub fn text(&self, arg: &str) -> String {
        self.range.text(arg)
    }

    pub fn start(&self) -> usize {self.range.start()}

    pub fn is_operator(&self) -> bool {
        for info in TokenInfo::operators() {
            if info.kind.eq(&self.kind) {
                return info.opkind.ne(&OperatorType::None);
            }
        }
        false
    }

    pub fn is_unary_op(&self) -> bool {
        for info in TokenInfo::operators() {
            if info.kind.eq(&self.kind) {
                return info.opkind.eq(&OperatorType::Prefix) ||
                    info.opkind.eq(&OperatorType::InfixOrPrefix);
            }
        }
        false
    }

    pub fn precedence(&self) -> usize {
        for info in TokenInfo::operators() {
            if info.kind.eq(&self.kind) {
                return info.precedence;
            }
        }
        100
    }
}

impl TokenInfo {
    //     | expr 'x{' expr '}'                          #ExprDup      - 11
    //     | '-' expr                                    #ExprNegate   - 10
    //     | ('|'|'&'|'~&'|'~|'|'^'|'~^') expr           #ExprCompress - 9
    //     | ('~'|'!') expr                              #ExprInvert   - 8
    //     | expr ('*'|'/') expr                         #ExprMultDiv  - 7
    //     | expr ('+'|'-') expr                         #ExprAddSub   - 6
    //     | expr ('>>'|'<<'|'<<<'|'>>>') expr           #ExprShift    - 5
    //     | expr ('|'|'&'|'^'|'~^') expr                #ExprAndOr    - 4
    //     | expr ('<'|'>'|'=='|'!='|'>='|'<=') expr     #ExprCompare  - 3
    //     | expr ('||'|'&&') expr                       #ExprLogical  - 2
    //     | expr '?' expr ':' expr                      #ExprTernary  - 1

    pub fn operators() -> Iter<'static, TokenInfo> {
        static TOKENS: [TokenInfo; 44] = [
            TokenInfo { kind: TokenKind::PLUS, text: "+", precedence: 6, opkind: OperatorType::InfixOrPrefix },
            TokenInfo { kind: TokenKind::MINUS, text: "-", precedence: 6, opkind: OperatorType::InfixOrPrefix },
            TokenInfo { kind: TokenKind::MULTIPLY, text: "*", precedence: 7, opkind: OperatorType::Infix },
            TokenInfo { kind: TokenKind::DIVIDE, text: "/", precedence: 7, opkind: OperatorType::Infix },
            TokenInfo { kind: TokenKind::LPAREN, text: "(", precedence: 0, opkind: OperatorType::None },
            TokenInfo { kind: TokenKind::RPAREN, text: ")", precedence: 0, opkind: OperatorType::None },
            TokenInfo { kind: TokenKind::LBRACKET, text: "[", precedence: 0, opkind: OperatorType::None },
            TokenInfo { kind: TokenKind::RBRACKET, text: "]", precedence: 0, opkind: OperatorType::None },
            TokenInfo { kind: TokenKind::LOGICALNOT, text: "!", precedence: 8, opkind: OperatorType::Prefix },
            TokenInfo { kind: TokenKind::BITNOT, text: "~", precedence: 8, opkind: OperatorType::Prefix },
            TokenInfo { kind: TokenKind::BITAND, text: "&", precedence: 4, opkind: OperatorType::InfixOrPrefix },
            TokenInfo { kind: TokenKind::BITOR, text: "|", precedence: 4, opkind: OperatorType::InfixOrPrefix },
            TokenInfo { kind: TokenKind::BITNAND, text: "~&", precedence: 9, opkind: OperatorType::InfixOrPrefix },
            TokenInfo { kind: TokenKind::BITNOR, text: "~|", precedence: 9, opkind: OperatorType::InfixOrPrefix },
            TokenInfo { kind: TokenKind::BITXOR, text: "^", precedence: 4, opkind: OperatorType::InfixOrPrefix },
            TokenInfo { kind: TokenKind::BITXNOR, text: "~^", precedence: 4, opkind: OperatorType::InfixOrPrefix },
            TokenInfo { kind: TokenKind::LBRACE, text: "{", precedence: 0, opkind: OperatorType::None },
            TokenInfo { kind: TokenKind::RBRACE, text: "}", precedence: 0, opkind: OperatorType::None },
            TokenInfo { kind: TokenKind::LEFTSHIFT, text: "<<", precedence: 5, opkind: OperatorType::Infix },
            TokenInfo { kind: TokenKind::RIGHTSHIFT, text: ">>", precedence: 5, opkind: OperatorType::Infix },
            TokenInfo { kind: TokenKind::LEFTSIGNEDSHIFT, text: "<<<", precedence: 5, opkind: OperatorType::Infix },
            TokenInfo { kind: TokenKind::RIGHTSIGNEDSHIFT, text: ">>>", precedence: 5, opkind: OperatorType::Infix },
            TokenInfo { kind: TokenKind::GT, text: ">", precedence: 3, opkind: OperatorType::Infix },
            TokenInfo { kind: TokenKind::GE, text: ">=", precedence: 3, opkind: OperatorType::Infix },
            TokenInfo { kind: TokenKind::LT, text: "<", precedence: 3, opkind: OperatorType::Infix },
            TokenInfo { kind: TokenKind::LE, text: "<=", precedence: 3, opkind: OperatorType::Infix },
            TokenInfo { kind: TokenKind::EQ, text: "==", precedence: 3, opkind: OperatorType::Infix },
            TokenInfo { kind: TokenKind::NEQ, text: "!=", precedence: 3, opkind: OperatorType::Infix },
            TokenInfo { kind: TokenKind::LOGICALAND, text: "&&", precedence: 2, opkind: OperatorType::Infix },
            TokenInfo { kind: TokenKind::LOGICALOR, text: "||", precedence: 2, opkind: OperatorType::Infix },
            TokenInfo { kind: TokenKind::TERNARY, text: "?", precedence: 1, opkind: OperatorType::Infix },
            TokenInfo { kind: TokenKind::COLON, text: ":", precedence: 0, opkind: OperatorType::None},
            TokenInfo { kind: TokenKind::ASSIGN, text: "=", precedence: 0, opkind: OperatorType::Infix },
            TokenInfo { kind: TokenKind::COMMA, text: ",", precedence: 0, opkind: OperatorType::None },
            TokenInfo { kind: TokenKind::SEMICOLON, text: ";", precedence: 0, opkind: OperatorType::None },
            TokenInfo { kind: TokenKind::DUPLICATE, text: "x{", precedence: 11, opkind: OperatorType::Infix},
            TokenInfo { kind: TokenKind::CONCATENATE, text: "c{", precedence: 0, opkind: OperatorType::None},
            TokenInfo { kind: TokenKind::DOT, text: ".", precedence: 0, opkind: OperatorType::None},
            TokenInfo { kind: TokenKind::PLUSCOLON, text: "+:", precedence: 0, opkind: OperatorType::None},
            TokenInfo { kind: TokenKind::NEGCOLON, text: "-:", precedence: 0, opkind: OperatorType::None},
            TokenInfo { kind: TokenKind::PARAMSTART, text: "#(", precedence: 0, opkind: OperatorType::None},
            TokenInfo { kind: TokenKind::HASH, text: "#", precedence: 0, opkind: OperatorType::None},
            TokenInfo { kind: TokenKind::PLUSPLUS, text: "++", precedence: 0, opkind: OperatorType::None},
            TokenInfo { kind: TokenKind::MINUSMINUS, text: "--", precedence: 0, opkind: OperatorType::None},
        ];
        TOKENS.iter()
    }

    pub fn keywords() -> Iter<'static, TokenInfo> {
        static KEYWORDS: [TokenInfo; 18] = [
            TokenInfo {kind: TokenKind::MODULE, text: "module", precedence:0, opkind:OperatorType::None},
            TokenInfo {kind: TokenKind::INPUT, text:"input", precedence:0, opkind:OperatorType::None},
            TokenInfo {kind: TokenKind::OUTPUT, text:"output", precedence:0, opkind:OperatorType::None},
            TokenInfo {kind: TokenKind::INOUT, text:"inout", precedence:0, opkind:OperatorType::None},
            TokenInfo {kind: TokenKind::SIG, text: "sig", precedence:0, opkind:OperatorType::None},
            TokenInfo {kind: TokenKind::DFF, text: "dff", precedence:0, opkind:OperatorType::None},
            TokenInfo {kind: TokenKind::FSM, text: "fsm", precedence:0, opkind:OperatorType::None},
            TokenInfo {kind: TokenKind::CONST, text: "const", precedence:0, opkind:OperatorType::None},
            TokenInfo {kind: TokenKind::CASE, text: "case", precedence:0, opkind:OperatorType::None},
            TokenInfo {kind: TokenKind::DEFAULT, text: "default", precedence: 0, opkind:OperatorType::None},
            TokenInfo {kind: TokenKind::FOR, text: "for", precedence: 0, opkind:OperatorType::None},
            TokenInfo {kind: TokenKind::VAR, text: "var", precedence: 0, opkind:OperatorType::None},
            TokenInfo {kind: TokenKind::SIGNED, text: "signed", precedence: 0, opkind:OperatorType::None},
            TokenInfo {kind: TokenKind::ALWAYS, text: "always", precedence: 0, opkind:OperatorType::None},
            TokenInfo {kind: TokenKind::IF, text: "if", precedence: 0, opkind:OperatorType::None},
            TokenInfo {kind: TokenKind::ELSE, text: "else", precedence: 0, opkind:OperatorType::None},
            TokenInfo {kind: TokenKind::STRUCT, text: "struct", precedence: 0, opkind:OperatorType::None},
            TokenInfo {kind: TokenKind::GLOBAL, text: "global", precedence: 0, opkind:OperatorType::None},
        ];
        KEYWORDS.iter()
    }
}

