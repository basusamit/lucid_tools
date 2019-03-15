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
use super::token::*;
use super::error::ProgramError;
use std::fmt::Write;

pub type LexerResult = Result<Token, ProgramError>;

pub struct Lexer {
    text: String,
    size: usize,
    ptr: usize,
    chr: Option<char>,
    current: Token,
}


impl Lexer {

    fn is_hex_marker(c: char) -> bool {c.eq(&'h')}
    fn is_dec_marker(c: char) -> bool {c.eq(&'d')}
    fn is_bin_marker(c: char) -> bool {c.eq(&'b')}

    pub fn from(text: &str) -> Lexer {
        Lexer {
            text: String::from(text),
            size: text.len(),
            ptr: 0,
            chr: Some(text.as_bytes()[0] as char),
            current: Token {kind: TokenKind::SOF, range: TextRange::empty() },
        }
    }

    pub fn source(&self) -> &String {
        &self.text
    }

    pub fn save(&self) -> usize {self.ptr}

    fn fetch_chr(&mut self) {
        if self.ptr < self.size {
            self.chr = Some(self.text.as_bytes()[self.ptr] as char);
        } else {
            self.chr = None;
        }
    }

    pub fn restore(&mut self, val: usize) {
        self.ptr = val;
        self.fetch_chr();
        self.current = self.fetch().unwrap();
    }

    fn push_ptr(&mut self) {
        self.ptr += 1;
        self.fetch_chr();
    }

    fn push_ptr_by_count(&mut self, amt: usize) {
        for _ in 0..amt {
            self.push_ptr();
        }

    }

    fn peek_ahead(&self, amt: usize) -> String {
        let mut result = String::new();
        let next_ptr = self.ptr;
        for pos in (next_ptr)..(next_ptr + amt) {
            if pos < self.size {
                result.push(self.text.as_bytes()[pos] as char);
            } else {
                break;
            }
        }
        result
    }

    fn skip_whitespace(&mut self) {
        while let Some(chr) = self.chr {
            if chr.is_whitespace() {
                self.push_ptr();
            } else {
                break;
            }
        }
    }

    fn build_identifier(&mut self) -> TextRange {
        let result_position = self.ptr;
        self.push_ptr(); // Accept the first character
        while let Some(chr) = self.chr {
            if chr.is_alphanumeric() || chr.eq(&'_'){
                if self.peek_ahead(2).eq(&String::from("x{")) {
                    break;
                }
                self.push_ptr();
            } else {
                break;
            }
        }
        TextRange::from(result_position, self.ptr)
    }

    fn build_integer(&mut self) -> TextRange {
        let result_position = self.ptr;
        while let Some(chr) = self.chr {
            if chr.is_digit(10) {
                self.push_ptr();
            } else {
                break;
            }
        }
        TextRange::from(result_position, self.ptr)
    }

    pub fn skip(&mut self, amt: usize) -> &mut Lexer {
        for _ in 0..amt {
            match self.next_token() {
                _ => (),
            };
        }
        self
    }

    pub fn match_block_comment_start(&mut self) -> bool {
        self.peek_ahead(2).eq(&String::from("/*"))
    }

    pub fn match_line_comment_start(&mut self) -> bool {
        self.peek_ahead(2).eq(&String::from("//"))
    }

    pub fn skip_block_comment(&mut self) {
        self.push_ptr();
        self.push_ptr();
        while self.peek_ahead(2).ne(&String::from("*/")) {
            self.push_ptr();
        }
        self.push_ptr();
        self.push_ptr();
    }

    pub fn skip_line_comment(&mut self) {
        self.push_ptr();
        self.push_ptr();
        while self.peek_ahead(1).ne(&String::from("\n")) {
            self.push_ptr();
        }
        self.push_ptr();
    }

    pub fn match_keyword(&mut self) -> LexerResult {
        for keyword in TokenInfo::keywords() {
            if keyword.text.eq(&self.peek_ahead(keyword.text.len())) {
                let start_pos = self.ptr;
                self.push_ptr_by_count(keyword.text.len());
                return Ok(Token::from_kind_and_positions(keyword.kind.clone(), start_pos, start_pos + keyword.text.len()));
            }
        }
        Err(ProgramError::of("NoKeyword", "No keyword found"))
    }

    pub fn match_double_operator(&mut self) -> LexerResult {
        let start_pos = self.ptr;
        let to_match = self.peek_ahead(2);
        for token in TokenInfo::operators() {
            if token.text.len() == 2 && token.text.eq(&to_match) {
                self.push_ptr();
                self.push_ptr();
                return Ok(Token::from_kind_and_positions(token.kind.clone(), start_pos, start_pos + 2))
            }
        }
        Err(ProgramError::of("InvalidOperator", "No matching operators found"))
    }

    pub fn match_single_operator(&mut self, chr: char) -> LexerResult {
        let start_pos = self.ptr;
        self.push_ptr();
        let range = TextRange::from(start_pos,  self.ptr);
        for token in TokenInfo::operators() {
            if token.text.len() == 1 && chr == (token.text.as_bytes()[0] as char) {
                return Ok(Token::from_kind_and_range(token.kind.clone(), range));
            }
        }
        let mut msg = String::from("Expected Token, found unknown character:");
        msg.push(chr);
        Err(ProgramError::of(
            "InvalidCharacter",
            &msg,
        ))
    }

    pub fn is_dont_care(&self, l: usize) -> bool {
        if let Some(chr) = self.chr {
            if chr.eq(&'z') || chr.eq(&'Z') || chr.eq(&'X') {
                return true;
            }
            if chr.eq(&'x') {
                if let Some(pk) = self.peek(l+1) {
                    if pk.eq(&'{') {
                        return false;
                    }
                }
                return true;
            }
        }
        false
    }
    pub fn peek(&self, l: usize) -> Option<char> {
        if (self.ptr+l) < self.size {
            Some(self.text.as_bytes()[self.ptr + l] as char)
        } else {
            None
        }
    }

    // To decide between a number and a name, we must scan ahead.
    // Essentially, if we have a digit, the symbol must be a number
    // however, if we have a base marker ('h', 'b', or 'd') then we
    // have to keep looking for a string of valid digits behind it
    // that either terminate in a symbol (a non-alphanumeric
    pub fn match_number_start(&self) -> bool {
        if let Some(chr) = self.chr {
            if chr.is_digit(10) {
                return true;
            }
            if Lexer::is_hex_marker(chr) {
                if let Some(nxt) = self.peek(1) {
                    if nxt.is_digit(16) || self.is_dont_care(1) {
                        return true;
                    }
                }
            }
            if Lexer::is_bin_marker(chr) {
                if let Some(nxt) = self.peek(1) {
                    if nxt.is_digit(2) || self.is_dont_care(1) {
                        return true;
                    }
                }
            }
            if Lexer::is_dec_marker(chr) {
                if let Some(nxt) = self.peek(1) {
                    if nxt.is_digit(10) {
                        return true;
                    }
                }
            }
        }
        false
    }

    pub fn match_integer(&mut self) -> LexerResult {
        Ok(Token::from_kind_and_range(TokenKind::NUMBER, self.build_integer()))
    }

    fn skip_type_specifier(&mut self, spec: char) {
        if let Some(chr) = self.chr {
            if chr.eq(&spec) {
                self.push_ptr();
            }
        }
    }

    fn skip_width_specifier(&mut self) {
        while let Some(chr) = self.chr {
            if chr.is_digit(10) {
                self.push_ptr();
            } else {
                break;
            }
        }
    }

    fn skip_number_body(&mut self, base: u32) {
        while let Some(chr) = self.chr {
            if chr.is_digit(base) || self.is_dont_care(0) {
                self.push_ptr();
            } else {
                break;
            }
        }
    }

    fn match_hex(&mut self) -> LexerResult {
        let result_position = self.ptr;
        self.skip_width_specifier();
        self.skip_type_specifier('h');
        self.skip_number_body(16);
        let range = TextRange::from(result_position, self.ptr);
        Ok(Token::from_kind_and_range(TokenKind::NUMBER, range))
    }

    fn match_binary(&mut self) -> LexerResult {
        let result_position = self.ptr;
        self.skip_width_specifier();
        self.skip_type_specifier('b');
        self.skip_number_body(2);
        let range = TextRange::from(result_position, self.ptr);
        Ok(Token::from_kind_and_range(TokenKind::NUMBER, range))
    }

    fn match_decimal(&mut self) -> LexerResult {
        let result_position = self.ptr;
        self.skip_width_specifier();
        self.skip_type_specifier('d');
        self.skip_number_body(10);
        let range = TextRange::from(result_position, self.ptr);
        Ok(Token::from_kind_and_range(TokenKind::NUMBER, range))
    }

    pub fn match_number(&mut self) -> LexerResult {
        if let Some(chr) = self.chr {
            if chr.eq(&'0') {
                return self.match_integer();
            }
            // We see a digit - this could be
            // a size prefix. To determine if it is, we have
            // to skip forward to the character after the
            // end of the digits
            let mut skip = 0;
            if chr.is_digit(10) {
                while let Some(t) = self.peek(skip) {
                    if t.is_digit(10) {
                        skip += 1;
                    } else {
                        break;
                    }
                }
            }
            // Check for a hex marker
            if let Some(t) = self.peek(skip) {
                if Lexer::is_hex_marker(t) {
                    return self.match_hex();
                }
                if Lexer::is_bin_marker(t) {
                    return self.match_binary();
                }
                if Lexer::is_dec_marker(t) {
                    return self.match_decimal();
                }
            }
            return self.match_integer();
        }
        Err(ProgramError::of("invalid_literal",
                             "Unable to parse numeric literal expression"))
    }

    fn match_identifier(&mut self) -> LexerResult {
        if let Ok(k) = self.match_keyword() {
            Ok(k)
        } else {
            Ok(Token::from_kind_and_range(TokenKind::IDENTIFIER, self.build_identifier()))
        }
    }

    fn match_number_or_identifier(&mut self) -> LexerResult {
        // Save the current pointer
        let current_ptr = self.ptr;
        // Try a number parse
        let _ = self.match_number();
        // Calculate the number length
        let number_length = self.ptr - current_ptr;
        // Reset the pointer
        self.ptr = current_ptr;
        // Try an identifier parse
        let ident_match = self.match_identifier();
        // Calculate the identifier length
        let ident_length = self.ptr - current_ptr;
        // if they are the same length or if the number is longer, use the
        // number match rule
        if number_length >= ident_length {
            self.ptr = current_ptr;
            self.fetch_chr();
            self.match_number()
        } else {
            ident_match
        }
    }

    fn match_string(&mut self) -> LexerResult {
        let result_position = self.ptr;
        self.push_ptr();
        while let Some(chr) = self.chr {
            if chr.eq(&'"') {
                self.push_ptr();
                break;
            }
            self.push_ptr();
        }
        let range = TextRange::from(result_position, self.ptr);
        Ok(Token::from_kind_and_range(TokenKind::NUMBER, range))
    }

    pub fn fetch(&mut self) -> LexerResult {
        while let Some(chr) = self.chr {
            if chr.is_whitespace() {
                self.skip_whitespace();
                continue;
            }
            if self.match_block_comment_start() {
                self.skip_block_comment();
                continue;
            }
            if self.match_line_comment_start() {
                self.skip_line_comment();
                continue;
            }
            if chr.eq(&'"') {
                return self.match_string();
            }
            if self.match_number_start() && chr.is_alphabetic() {
                return self.match_number_or_identifier();
            }
            if self.match_number_start() {
                return self.match_number();
            }
            if let Ok(t) = self.match_double_operator() {
                return Ok(t);
            }
            if chr.eq(&'$') {
                return Ok(Token::from_kind_and_range(TokenKind::FUNCTION, self.build_identifier()));
            }
            if chr.is_alphabetic() {
                let keyword_match = self.match_keyword();
                if keyword_match.is_ok() {
                    return keyword_match;
                }
                return Ok(Token::from_kind_and_range(TokenKind::IDENTIFIER, self.build_identifier()));
            }
            if chr.is_digit(10) {
                return Ok(Token::from_kind_and_range(TokenKind::NUMBER, self.build_integer()));
            }
            return self.match_single_operator(chr);
        }
        Ok(Token::eof())
    }

    pub fn next_token(&mut self) -> &Token {
        &self.current
    }

    pub fn next_is(&self, kind: &TokenKind) -> bool {
        self.current.kind.eq(kind)
    }

    pub fn next_is_in(&self, kind: &[&TokenKind]) -> bool {
        for k in kind {
            if self.next_is(k) {
                return true;
            }
        }
        false
    }

    pub fn consume(&mut self) -> LexerResult {
        let to_return = self.current.clone();
        self.current = self.fetch()?;
        Ok(to_return)
    }

    pub fn match_token(&mut self, kind: &TokenKind) -> LexerResult {
        let next = self.next_token();
        if next.kind.eq(kind) {
            self.consume()
        } else {
            let mut emsg = String::new();
            let _ = write!(emsg, "expected {:?}, got {:?}", kind, next);
            Err(ProgramError::of("mismatch",&emsg))
        }
    }

    pub fn match_token_from_list(&mut self, kind: &[&TokenKind]) -> LexerResult {
        for k in kind {
            if self.next_is(k) {
                return self.match_token(k)
            }
        }
        Err(ProgramError::of("mismatch", "Expected A, B, C, got D"))
    }
}
