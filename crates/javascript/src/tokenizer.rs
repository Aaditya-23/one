use crate::{
    kind::Kind,
    lookup_table::{get_mapping, AsciiMapping::*},
};

const UNICODE_SPACES: [char; 19] = [
    '\u{0020}', '\u{00A0}', '\u{1680}', '\u{2000}', '\u{2001}', '\u{2002}', '\u{2003}', '\u{2004}',
    '\u{2005}', '\u{2006}', '\u{2007}', '\u{2008}', '\u{2009}', '\u{200A}', '\u{200B}', '\u{202F}',
    '\u{205F}', '\u{3000}', '\u{FEFF}',
];

#[derive(Clone, Debug)]
#[repr(C, align(16))]
pub struct Token {
    pub kind: Kind,
    pub is_on_new_line: bool,
    pub start: u32,
    pub end: u32,
}

#[derive(Debug)]
pub enum LexContext {
    Normal,
    Template,
}

#[derive(Debug, Clone)]
pub struct Tokenizer<'a> {
    pub code: &'a str,
    pub index: usize,
    pub line_number: u32,
}

impl<'a> Tokenizer<'a> {
    #[inline]
    unsafe fn get_byte(&self) -> u8 {
        let byte = self.code.as_bytes().get_unchecked(self.index);
        *byte
    }

    #[inline]
    unsafe fn get_utf8_char(&self) -> char {
        let value = self
            .code
            .get_unchecked(self.index..self.code.len())
            .chars()
            .next();

        let Some(char) = value else {
            core::hint::unreachable_unchecked();
        };

        char
    }

    #[inline]
    fn advance(&mut self, n: usize) {
        self.index += n;
    }

    #[inline]
    fn byte_at(&mut self, offset: usize) -> Option<u8> {
        let index = self.index + offset;

        if index < self.code.len() {
            Some(unsafe { *self.code.as_bytes().get_unchecked(index) })
        } else {
            None
        }
    }

    #[inline]
    fn next_byte(&mut self) -> Option<u8> {
        self.advance(1);

        if self.index < self.code.len() {
            Some(unsafe { self.get_byte() })
        } else {
            None
        }
    }

    pub fn peek(&mut self, ctx: LexContext) -> Token {
        let prev_index = self.index;
        let prev_line_number = self.line_number;

        let token = self.lex(ctx);

        self.index = prev_index;
        self.line_number = prev_line_number;

        token
    }

    #[inline]
    fn eat_byte(&mut self, kind: Kind) -> Kind {
        self.advance(1);
        kind
    }

    #[inline]
    fn read_hex_number(&mut self) {
        loop {
            match self.next_byte() {
                Some(b'_') => {}
                Some(b) if char::from(b).is_ascii_hexdigit() => {}
                _ => return,
            }
        }
    }

    #[inline]
    fn read_binary_number(&mut self) {
        loop {
            match self.next_byte() {
                // handle _ later
                Some(b'_') => {}
                Some(b'0' | b'1') => {}
                _ => return,
            }
        }
    }

    #[inline]
    fn read_octal_number(&mut self) {
        loop {
            match self.next_byte() {
                Some(b'_') => {}
                Some(b'0'..=b'7') => {}
                _ => return,
            }
        }
    }

    fn consume_whitespaces_and_newline(&mut self) {
        while self.index < self.code.len() {
            let byte = unsafe { self.get_byte() };
            match get_mapping(byte) {
                Whitespace => match byte {
                    b'\n' => {
                        self.advance(1);
                        self.line_number += 1;
                    }

                    b'\r' => {
                        self.advance(1);
                        if b'\n' == unsafe { self.get_byte() } {
                            self.advance(1);
                        }

                        self.line_number += 1;
                    }

                    _ => self.advance(1),
                },

                Unicode => {
                    let ch = unsafe { self.get_utf8_char() };

                    if UNICODE_SPACES.contains(&ch) {
                        self.advance(ch.len_utf8());
                    } else {
                        break;
                    }
                }

                _ => {
                    if !byte.is_ascii() {
                        let ch = unsafe { self.get_utf8_char() };
                        if ch == '\u{2028}' || ch == '\u{2029}' {
                            self.advance(ch.len_utf8());
                            self.line_number += 1;
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
            }
        }
    }

    pub fn lex_regexp(&mut self) -> (u32, &'a str) {
        let mut in_class = false;
        let (pattern_end, flag);

        loop {
            match self.byte_at(0) {
                Some(b'[') => {
                    in_class = true;
                    self.advance(1);
                }

                Some(b']') => {
                    in_class = false;
                    self.advance(1);
                }

                Some(b'\\') => {
                    let next = self.next_byte().expect("unterminated regexp");

                    if next.is_ascii() {
                        self.advance(1);
                    } else {
                        let ch = unsafe { self.get_utf8_char() };
                        self.advance(ch.len_utf8());
                    }
                }

                Some(b'/') => {
                    if !in_class {
                        self.advance(1);
                        break;
                    } else {
                        self.advance(1);
                    }
                }

                Some(b'\n' | b'\r') => panic!("unterminated regexp"),

                Some(ch) => {
                    if ch.is_ascii() {
                        self.advance(1);
                    } else {
                        let ch = unsafe { self.get_utf8_char() };

                        if let '\u{2028}' | '\u{2029}' = ch {
                            panic!("unterminated regexp");
                        }

                        self.advance(ch.len_utf8());
                    }
                }

                None => panic!("unterminated regexp"),
            }
        }

        pattern_end = self.index as u32 - 1;

        if let Some(b'g' | b'i' | b'm' | b's' | b'u' | b'y' | b'd' | b'v') = self.byte_at(0) {
            flag = unsafe { self.code.get_unchecked(self.index..self.index + 1) };
            self.advance(1);
        } else {
            flag = "";
        }

        (pattern_end, flag)
    }

    pub fn lex_template_literal(&mut self) -> Kind {
        loop {
            match self.byte_at(0) {
                Some(b'`') => {
                    self.advance(1);
                    return Kind::BackQuote;
                }
                Some(b'\\') => {
                    let next = self.next_byte().expect("unterminated template literal");

                    if next.is_ascii() {
                        self.advance(1);
                    } else {
                        let ch = unsafe { self.get_utf8_char() };
                        self.advance(ch.len_utf8());
                    }
                }
                Some(b'$') => {
                    if self.byte_at(1) == Some(b'{') {
                        self.advance(2);
                        return Kind::DollarCurly;
                    } else {
                        self.advance(1);
                    }
                }
                Some(ch) => {
                    if ch.is_ascii() {
                        match ch {
                            b'\n' => {
                                self.line_number += 1;
                            }
                            b'\r' => {
                                if let Some(b'\n') = self.byte_at(1) {
                                    self.advance(1);
                                }

                                self.line_number += 1;
                            }
                            _ => {}
                        }

                        self.advance(1);
                    } else {
                        let ch = unsafe { self.get_utf8_char() };

                        match ch {
                            '\u{2028}' | '\u{2029}' => {
                                self.line_number += 1;
                            }
                            _ => {}
                        }

                        self.advance(ch.len_utf8());
                    }
                }
                None => panic!("unterminated template literal"),
            }
        }
    }

    fn read_slash(&mut self) -> Kind {
        match self.next_byte() {
            Some(b'/') => {
                self.advance(1);

                while let Some(ch) = self.byte_at(0) {
                    if let b'\n' | b'\r' = ch {
                        break;
                    } else if ch.is_ascii() {
                        self.advance(1);
                    } else {
                        let ch = unsafe { self.get_utf8_char() };
                        if let '\u{2028}' | '\u{2029}' = ch {
                            break;
                        } else {
                            self.advance(ch.len_utf8());
                        }
                    }
                }

                Kind::LineComment
            }

            Some(b'*') => {
                self.advance(1);

                while let Some(ch) = self.byte_at(0) {
                    if let b'*' = ch {
                        if Some(b'/') == self.next_byte() {
                            self.advance(1);
                            return Kind::BlockComment;
                        } else {
                            self.advance(1);
                        }
                    } else if ch.is_ascii() {
                        self.advance(1);
                    } else {
                        let ch = unsafe { self.get_utf8_char() };
                        self.advance(ch.len_utf8());
                    }
                }

                panic!("unterminated block comment")
            }

            Some(b'=') => {
                self.advance(1);
                Kind::SlashEqual
            }

            _ => Kind::Slash,
        }
    }

    fn resolve_asterisk(&mut self) -> Kind {
        match self.next_byte() {
            Some(b'*') => {
                if Some(b'=') == self.next_byte() {
                    Kind::Star2Equal
                } else {
                    Kind::Star2
                }
            }
            Some(b'=') => {
                self.advance(1);
                Kind::StarEqual
            }
            _ => Kind::Star,
        }
    }

    fn resolve_plus(&mut self) -> Kind {
        match self.next_byte() {
            Some(b'+') => {
                self.advance(1);
                Kind::Plus2
            }
            Some(b'=') => {
                self.advance(1);
                Kind::PlusEqual
            }
            _ => Kind::Plus,
        }
    }

    fn resolve_minus(&mut self) -> Kind {
        match self.next_byte() {
            Some(b'-') => {
                self.advance(1);
                Kind::Minus2
            }
            Some(b'=') => {
                self.advance(1);
                Kind::MinusEqual
            }
            _ => Kind::Minus,
        }
    }

    fn read_exponent_part(&mut self) {
        if let Some(b'+' | b'-') = self.byte_at(1) {
            self.advance(1);
        }

        loop {
            match self.next_byte() {
                Some(b'0'..=b'9') => {}
                Some(b'_') => {}
                _ => return,
            }
        }
    }

    fn read_floating_number(&mut self) -> Kind {
        loop {
            match self.next_byte() {
                Some(b'_') => {}
                Some(b'0'..=b'9') => {}
                Some(b'e' | b'E') => {
                    self.advance(1);
                    self.read_exponent_part();
                    break;
                }
                _ => break,
            }
        }

        Kind::Number
    }

    fn read_zero(&mut self) -> Kind {
        match self.byte_at(1) {
            Some(b'.') => {
                self.advance(1);
                return self.read_floating_number();
            }
            Some(b'0') => panic!("multiple zeros not allowed"),
            Some(b'n') => self.advance(2),
            Some(b'x' | b'X') => match self.byte_at(2) {
                Some(byte) if char::from(byte).is_ascii_hexdigit() => {
                    self.advance(2);
                    self.read_hex_number();

                    if let Some(b'n') = self.byte_at(0) {
                        self.advance(1);
                    }
                }
                _ => panic!("expected hex number"),
            },
            Some(b'b' | b'B') => match self.byte_at(2) {
                Some(b'0' | b'1') => {
                    self.advance(2);
                    self.read_binary_number();

                    if let Some(b'n') = self.byte_at(0) {
                        self.advance(1);
                    }
                }
                _ => panic!("expected binary number"),
            },
            Some(b'o' | b'O') => match self.byte_at(2) {
                Some(b'0'..=b'7') => {
                    self.advance(2);
                    self.read_octal_number();

                    if let Some(b'n') = self.byte_at(0) {
                        self.advance(1);
                    }
                }
                _ => panic!("expected octal number"),
            },
            _ => return self.read_number(true),
        }

        Kind::Number
    }

    fn read_number(&mut self, has_leading_zero: bool) -> Kind {
        loop {
            match self.next_byte() {
                Some(b'.') => {
                    return self.read_floating_number();
                }
                Some(b'_') => {
                    if has_leading_zero {
                        panic!("no leading zero");
                    }
                }
                Some(b'0'..=b'9') => {}
                Some(b'e' | b'E') => {
                    self.advance(1);
                    self.read_exponent_part();
                    break;
                }
                Some(b'n') => {
                    if has_leading_zero {
                        panic!("no leading zero");
                    }

                    self.advance(1);
                    break;
                }
                _ => break,
            }
        }

        Kind::Number
    }

    fn resolve_dot(&mut self) -> Kind {
        if self.byte_at(1) == Some(b'.') && self.byte_at(2) == Some(b'.') {
            self.advance(3);
            Kind::Dot3
        } else if let Some(b'0'..=b'9') = self.byte_at(1) {
            self.read_floating_number()
        } else {
            self.advance(1);
            Kind::Dot
        }
    }

    fn resolve_percent(&mut self) -> Kind {
        match self.next_byte() {
            Some(b'=') => {
                self.advance(1);
                Kind::ModEqual
            }
            _ => Kind::Mod,
        }
    }

    fn resolve_ampersand(&mut self) -> Kind {
        match self.next_byte() {
            Some(b'&') => {
                if self.next_byte() == Some(b'=') {
                    self.advance(1);
                    Kind::Ampersand2Equal
                } else {
                    Kind::Ampersand2
                }
            }

            Some(b'=') => {
                self.advance(1);
                Kind::AmpersandEqual
            }
            _ => Kind::Ampersand,
        }
    }

    fn resolve_pipe(&mut self) -> Kind {
        match self.next_byte() {
            Some(b'|') => {
                if self.next_byte() == Some(b'=') {
                    self.advance(1);
                    Kind::Pipe2Equal
                } else {
                    Kind::Pipe2
                }
            }
            Some(b'=') => {
                self.advance(1);
                Kind::PipeEqual
            }
            _ => Kind::Pipe,
        }
    }

    fn resolve_question(&mut self) -> Kind {
        match self.next_byte() {
            Some(b'?') => {
                if let Some(b'=') = self.next_byte() {
                    self.advance(1);
                    Kind::Question2Equal
                } else {
                    Kind::Question2
                }
            }
            Some(b'.') => {
                self.advance(1);
                Kind::QuestionDot
            }
            _ => Kind::Question,
        }
    }

    fn resolve_bang(&mut self) -> Kind {
        match self.next_byte() {
            Some(b'=') => {
                if let Some(b'=') = self.next_byte() {
                    self.advance(1);
                    Kind::NotEqual2
                } else {
                    Kind::NotEqual
                }
            }
            _ => Kind::Bang,
        }
    }

    fn resolve_less_than(&mut self) -> Kind {
        match self.next_byte() {
            Some(b'=') => {
                self.advance(1);
                Kind::LessThanOrEqual
            }
            _ => Kind::LessThan,
        }
    }

    fn resolve_greater_than(&mut self) -> Kind {
        match self.next_byte() {
            Some(b'=') => {
                self.advance(1);
                Kind::GreaterThanOrEqual
            }
            _ => Kind::GreaterThan,
        }
    }

    fn resolve_equal(&mut self) -> Kind {
        match self.next_byte() {
            Some(b'=') => {
                if let Some(b'=') = self.next_byte() {
                    self.advance(1);
                    Kind::Equal3
                } else {
                    Kind::Equal2
                }
            }
            Some(b'>') => {
                self.advance(1);
                Kind::Arrow
            }
            _ => Kind::Equal,
        }
    }

    fn resolve_caret(&mut self) -> Kind {
        match self.next_byte() {
            Some(b'=') => {
                self.advance(1);
                Kind::CaretEqual
            }
            _ => Kind::Caret,
        }
    }

    fn read_identifier_to_buffer(&mut self, buf: &mut [u8]) -> usize {
        let mut buff_len = 0;

        while let Some(b) = self.byte_at(0) {
            match get_mapping(b) {
                Identifier | Dollar | Digit | Zero => {
                    if let Some(buf) = buf.get_mut(buff_len..buff_len + 4) {
                        buff_len += (b as char).encode_utf8(buf).len();
                    }
                    self.advance(1);
                }
                Unicode => {
                    // check for valid unicodes later
                    let ch = unsafe { self.get_utf8_char() };
                    if let Some(buf) = buf.get_mut(buff_len..buff_len + 4) {
                        buff_len += ch.encode_utf8(buf).len();
                    }
                    self.advance(ch.len_utf8());
                }
                // complete this later
                // Backslash => {}
                _ => break,
            }
        }

        buff_len
    }

    fn resolve_identifier(&mut self) -> Kind {
        let mut buf = [0u8; 16];
        let buff_len = self.read_identifier_to_buffer(&mut buf);

        match &buf[..buff_len] {
            b"await" => Kind::Await,
            b"async" => Kind::Async,
            b"as" => Kind::As,
            b"assert" => Kind::Assert,

            b"break" => Kind::Break,

            b"case" => Kind::Case,
            b"catch" => Kind::Catch,
            b"class" => Kind::Class,
            b"continue" => Kind::Continue,
            b"const" => Kind::Const,
            b"constructor" => Kind::Constructor,

            b"debugger" => Kind::Debugger,
            b"delete" => Kind::Delete,
            b"do" => Kind::Do,
            b"default" => Kind::Default,

            b"export" => Kind::Export,
            b"else" => Kind::Else,
            b"extends" => Kind::Extends,

            b"function" => Kind::Function,
            b"for" => Kind::For,
            b"finally" => Kind::Finally,
            b"false" => Kind::Boolean,
            b"from" => Kind::From,

            b"get" => Kind::Get,

            b"if" => Kind::If,
            b"in" => Kind::In,
            b"instanceof" => Kind::Instanceof,
            b"import" => Kind::Import,

            b"let" => Kind::Let,

            b"new" => Kind::New,
            b"null" => Kind::Null,

            b"return" => Kind::Return,

            b"super" => Kind::Super,
            b"switch" => Kind::Switch,
            b"static" => Kind::Static,
            b"set" => Kind::Set,

            b"this" => Kind::This,
            b"typeof" => Kind::Typeof,
            b"throw" => Kind::Throw,
            b"try" => Kind::Try,
            b"true" => Kind::Boolean,

            b"var" => Kind::Var,
            b"void" => Kind::Void,

            b"While" => Kind::While,
            b"with" => Kind::With,

            _ => Kind::Identifier,
        }
    }

    fn resolve_string_literal(&mut self) -> Kind {
        let quote = unsafe { self.get_byte() };
        self.advance(1);

        loop {
            match self.byte_at(0) {
                Some(b'\\') => {
                    self.advance(2);
                }
                Some(ch) if ch == quote => {
                    self.advance(1);
                    return Kind::String;
                }
                Some(ch) => {
                    if ch.is_ascii() {
                        match ch {
                            b'\n' => {
                                self.line_number += 1;
                            }
                            b'\r' => {
                                if let Some(b'\n') = self.byte_at(1) {
                                    self.advance(1);
                                }

                                self.line_number += 1;
                            }
                            _ => {}
                        }

                        self.advance(1);
                    } else {
                        let ch = unsafe { self.get_utf8_char() };

                        match ch {
                            '\u{2028}' | '\u{2029}' => {
                                self.line_number += 1;
                            }
                            _ => {}
                        }

                        self.index += ch.len_utf8();
                    }
                }
                None => break,
            }
        }

        panic!("unterminated string")
    }

    fn get_token(&mut self) -> Kind {
        let byte = unsafe { self.get_byte() };

        match get_mapping(byte) {
            Slash => self.read_slash(),
            Asterisk => self.resolve_asterisk(),
            Plus => self.resolve_plus(),
            Minus => self.resolve_minus(),
            Zero => self.read_zero(),
            Digit => self.read_number(false),
            Dot => self.resolve_dot(),
            Percent => self.resolve_percent(),
            Ampersand => self.resolve_ampersand(),
            Pipe => self.resolve_pipe(),
            Question => self.resolve_question(),
            Bang => self.resolve_bang(),
            LessThan => self.resolve_less_than(),
            GreaterThan => self.resolve_greater_than(),
            Equal => self.resolve_equal(),
            Caret => self.resolve_caret(),
            Identifier | Dollar => self.resolve_identifier(),
            Quote => self.resolve_string_literal(),

            Backquote => self.eat_byte(Kind::BackQuote),
            Comma => self.eat_byte(Kind::Comma),
            ParenO => self.eat_byte(Kind::ParenO),
            ParenC => self.eat_byte(Kind::ParenC),
            BracesO => self.eat_byte(Kind::BracesO),
            BracesC => self.eat_byte(Kind::BracesC),
            BracketO => self.eat_byte(Kind::BracketO),
            BracketC => self.eat_byte(Kind::BracketC),
            Colon => self.eat_byte(Kind::Colon),
            Semicolon => self.eat_byte(Kind::Semicolon),
            Tilde => self.eat_byte(Kind::Tilde),

            Hash => todo!(),
            Backslash => todo!(),
            At => todo!(),
            Unicode => todo!(),

            _ => panic!("unexpected token"),
        }
    }

    pub fn lex(&mut self, ctx: LexContext) -> Token {
        let start;
        let prev_line_number = self.line_number;
        let (kind, end);

        if self.index >= self.code.len() {
            kind = Kind::EOF;
            start = self.index;
            end = start + 1;
        } else {
            match ctx {
                LexContext::Normal => {
                    self.consume_whitespaces_and_newline();
                    start = self.index;
                    kind = self.get_token();
                }

                LexContext::Template => {
                    start = self.index;
                    kind = self.lex_template_literal();
                }
            }

            end = self.index;
        }

        Token {
            kind,
            is_on_new_line: prev_line_number != self.line_number,
            start: start as u32,
            end: end as u32,
        }
    }

    pub fn new(code: &'a str) -> Tokenizer<'a> {
        Tokenizer {
            code,
            index: 0,
            line_number: 1,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{fs::read_to_string, time::Instant};

    use super::*;

    #[test]
    fn test_tokenizer() {
        let code = read_to_string("../../data/input.js").unwrap();
        let mut tokenizer = Tokenizer::new(code.as_str());

        loop {
            let token = tokenizer.lex(LexContext::Normal);
            println!("{:?}", token);

            if let Kind::EOF = token.kind {
                break;
            }
        }
    }

    #[test]
    fn tokenizer_performance() {
        let code = read_to_string("../../data/input.js").unwrap();
        let mut tokenizer = Tokenizer::new(code.as_str());

        let start = Instant::now();
        loop {
            let token = tokenizer.lex(LexContext::Normal);
            if let Kind::EOF = token.kind {
                break;
            }
        }

        let end = start.elapsed();
        println!("{}", end.as_millis());
    }
}
