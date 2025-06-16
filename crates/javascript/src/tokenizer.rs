use std::ops::Range;

use crate::{
    kind::Kind,
    lookup_table::{get_mapping, AsciiMapping::*},
    DiagnosticError, ParserDiagnostics,
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

impl Token {
    pub fn range(&self) -> Range<u32> {
        self.start..self.end
    }
}

#[derive(Debug)]
pub enum LexContext {
    Normal,
    Template,
    JSXChild,
}

#[derive(Debug, Clone)]
pub struct Tokenizer<'a> {
    pub code: &'a str,
    pub index: usize,
    pub line_number: u32,
    pub diagnostics: Vec<ParserDiagnostics<'a>>,
}

impl<'a> Tokenizer<'a> {
    fn add_diagnostic(&mut self, message: &'a str, position: Range<usize>) {
        self.diagnostics.push(ParserDiagnostics {
            error: DiagnosticError::Other(message),
            position,
        })
    }

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
        self.code.as_bytes().get(self.index + offset).copied()
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
    fn read_numeric_separator(&mut self, allow_ascii_hex_digits: bool) {
        let is_forbidden = |b| {
            if let Some(byte) = b {
                if (b'0'..=b'9').contains(&byte)
                    || (allow_ascii_hex_digits && char::from(byte).is_ascii_hexdigit())
                {
                    false
                } else {
                    true
                }
            } else {
                true
            }
        };

        let prev = self.code.as_bytes().get(self.index - 1).copied();

        if is_forbidden(prev) || is_forbidden(self.byte_at(1)) {
            self.add_diagnostic("'_' cannot be used here", self.index..self.index + 1);
        }

        self.advance(1);
    }

    #[inline]
    fn read_hex_number(&mut self) {
        loop {
            match self.next_byte() {
                Some(b'_') => self.read_numeric_separator(true),
                Some(b) if char::from(b).is_ascii_hexdigit() => {}
                _ => return,
            }
        }
    }

    #[inline]
    fn read_binary_number(&mut self) {
        loop {
            match self.next_byte() {
                Some(b'_') => self.read_numeric_separator(false),
                Some(b'0' | b'1') => {}
                _ => return,
            }
        }
    }

    #[inline]
    fn read_octal_number(&mut self) {
        loop {
            match self.next_byte() {
                Some(b'_') => self.read_numeric_separator(false),
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

    pub fn lex_regexp(&mut self) -> Option<(u32, &'a str)> {
        let start = self.index;
        let (mut in_class, mut is_escaped) = (false, false);
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
                    is_escaped = !is_escaped;
                    self.advance(1);
                    continue;
                }

                Some(b'/') => {
                    self.advance(1);
                    if !in_class && !is_escaped {
                        break;
                    }
                }

                Some(b'\n' | b'\r') => {
                    self.add_diagnostic("unterminated regular expression", start..self.index);
                    return None;
                }

                Some(ch) => {
                    if ch.is_ascii() {
                        self.advance(1);
                    } else {
                        let ch = unsafe { self.get_utf8_char() };

                        if let '\u{2028}' | '\u{2029}' = ch {
                            self.add_diagnostic(
                                "unterminated regular expression",
                                start..self.index,
                            );
                            return None;
                        }

                        self.advance(ch.len_utf8());
                    }
                }

                None => {
                    self.add_diagnostic("unterminated regular expression", start..self.index);
                    return None;
                }
            }

            is_escaped = false;
        }

        pattern_end = self.index as u32 - 1;

        if let Some(b'g' | b'i' | b'm' | b's' | b'u' | b'y' | b'd' | b'v') = self.byte_at(0) {
            flag = unsafe { self.code.get_unchecked(self.index..self.index + 1) };
            self.advance(1);
        } else {
            flag = "";
        }

        Some((pattern_end, flag))
    }

    fn lex_template_literal(&mut self) -> Kind {
        let start = self.index;
        let mut is_escaped = false;

        loop {
            match self.byte_at(0) {
                Some(b'`') => {
                    self.advance(1);

                    if !is_escaped {
                        return Kind::BackQuote;
                    }
                }
                Some(b'\\') => {
                    self.advance(1);
                    is_escaped = !is_escaped;
                    continue;
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
                None => {
                    self.add_diagnostic("unterminated template literal", start..self.index);
                    return Kind::Error;
                }
            }

            is_escaped = false;
        }
    }

    fn lex_jsx_child(&mut self) -> Kind {
        let start = self.index;

        let ch = unsafe { self.get_byte() };

        match ch {
            b'{' => self.eat_byte(Kind::BracesO),
            b'<' => self.eat_byte(Kind::LessThan),
            _ => {
                while let Some(ch) = self.byte_at(0) {
                    match ch {
                        b'{' | b'<' => break,
                        b'}' => {
                            self.add_diagnostic("did you mean {'}'} ?", start..self.index);
                            return Kind::Error;
                        }
                        b'>' => {
                            self.add_diagnostic("did you mean {'>'} ?", start..self.index);
                            return Kind::Error;
                        }
                        ch => {
                            if ch.is_ascii() {
                                if ch == b'\n' {
                                    self.line_number += 1;
                                } else if ch == b'\r' {
                                    if let Some(b'\n') = self.byte_at(1) {
                                        self.advance(1);
                                    }

                                    self.line_number += 1;
                                }

                                self.advance(1);
                            } else {
                                let ch = unsafe { self.get_utf8_char() };

                                if ch == '\u{2028}' || ch == '\u{2029}' {
                                    self.line_number += 1;
                                }

                                self.index += ch.len_utf8();
                            }
                        }
                    }
                }

                Kind::JSXText
            }
        }
    }

    pub fn relex_jsx_child(&mut self, token: Token) -> Token {
        if token.kind == Kind::EOF {
            return token;
        }

        let start = token.start;

        self.index = start as usize;
        let kind = self.lex_jsx_child();

        Token {
            start,
            kind,
            is_on_new_line: token.is_on_new_line,
            end: self.index as u32,
        }
    }

    fn read_slash(&mut self) -> Kind {
        let start = self.index;

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

                self.add_diagnostic("unterminated block comment", start..self.index);
                Kind::BlockComment
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
                Some(b'_') => self.read_numeric_separator(false),
                Some(b'0'..=b'9') => { /*  continue reading digits */ }
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

    // todo: start with making byte_at to next_byte in this fn
    fn read_zero(&mut self) -> Kind {
        let start = self.index;

        if let Some(byte) = self.next_byte() {
            match byte {
                b'.' => return self.read_floating_number(),

                b'n' => self.advance(2),

                b'x' | b'X' => match self.byte_at(1) {
                    Some(byte) if char::from(byte).is_ascii_hexdigit() => {
                        self.advance(1);
                        self.read_hex_number();

                        if let Some(b'n') = self.byte_at(0) {
                            self.advance(1);
                        }
                    }
                    _ => self.add_diagnostic("expected hex number", start..self.index + 1),
                },

                b'b' | b'B' => match self.byte_at(1) {
                    Some(b'0' | b'1') => {
                        self.advance(1);
                        self.read_binary_number();

                        if let Some(b'n') = self.byte_at(0) {
                            self.advance(1);
                        }
                    }
                    _ => self.add_diagnostic("expected binary number", start..self.index + 1),
                },

                b'o' | b'O' => match self.byte_at(1) {
                    Some(b'0'..=b'7') => {
                        self.advance(1);
                        self.read_octal_number();

                        if let Some(b'n') = self.byte_at(0) {
                            self.advance(1);
                        }
                    }
                    _ => self.add_diagnostic("expected octal number", start..self.index + 1),
                },

                _ => {
                    if byte == b'0' {
                        self.add_diagnostic("multiple zeros not allowed", start..self.index + 1);
                    }
                    return self.read_number(true);
                }
            }
        }

        Kind::Number
    }

    fn read_number(&mut self, has_leading_zero: bool) -> Kind {
        let start = self.index;

        loop {
            match self.next_byte() {
                Some(b'.') => return self.read_floating_number(),
                Some(b'_') => {
                    if has_leading_zero {
                        self.add_diagnostic(
                            "'_' cannot be used after a leading zero",
                            start..self.index + 1,
                        );
                    }

                    self.read_numeric_separator(false)
                }
                Some(b'0'..=b'9') => {
                    // * continue reading digits
                }
                Some(b'e' | b'E') => {
                    self.advance(1);
                    self.read_exponent_part();
                    break;
                }
                Some(b'n') => {
                    if has_leading_zero {
                        self.add_diagnostic("no leading zeros", start..self.index + 1);
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
            Some(b'<') => {
                if Some(b'=') == self.next_byte() {
                    self.advance(1);
                    Kind::LeftShiftEqual
                } else {
                    Kind::LeftShift
                }
            }
            Some(b'=') => {
                self.advance(1);
                Kind::LessThanOrEqual
            }
            _ => Kind::LessThan,
        }
    }

    pub fn relex_binary_op(&mut self, token: Token) -> Token {
        let start = token.start;

        if token.kind != Kind::GreaterThan {
            return token;
        }

        let kind = match self.byte_at(0) {
            Some(b'>') => {
                self.advance(1);

                match self.byte_at(0) {
                    Some(b'>') => {
                        self.advance(1);

                        match self.byte_at(0) {
                            Some(b'=') => {
                                self.advance(1);

                                Kind::UnsignedRightShiftEqual
                            }
                            _ => Kind::UnsignedRightShift,
                        }
                    }
                    Some(b'=') => {
                        self.advance(1);

                        Kind::RightShiftEqual
                    }
                    _ => Kind::RightShift,
                }
            }
            Some(b'=') => {
                self.advance(1);
                Kind::GreaterThanOrEqual
            }
            _ => return token,
        };

        Token {
            start,
            kind,
            is_on_new_line: token.is_on_new_line,
            end: self.index as u32,
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
            b"boolean" => Kind::TypeBoolean,

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
            b"interface" => Kind::Interface,
            b"implements" => Kind::Implements,

            b"let" => Kind::Let,

            b"new" => Kind::New,
            b"null" => Kind::Null,
            b"number" => Kind::TypeNumber,

            b"return" => Kind::Return,

            b"super" => Kind::Super,
            b"switch" => Kind::Switch,
            b"static" => Kind::Static,
            b"set" => Kind::Set,
            b"satisfies" => Kind::Satisfies,
            b"string" => Kind::TypeString,

            b"this" => Kind::This,
            b"typeof" => Kind::Typeof,
            b"throw" => Kind::Throw,
            b"try" => Kind::Try,
            b"true" => Kind::Boolean,
            b"type" => Kind::Type,

            b"var" => Kind::Var,
            b"void" => Kind::Void,

            b"While" => Kind::While,
            b"with" => Kind::With,

            _ => Kind::Identifier,
        }
    }

    fn resolve_string_literal(&mut self) -> Kind {
        let start = self.index;
        let quote = unsafe { self.get_byte() };
        self.advance(1);

        let mut is_escaped = false;

        loop {
            match self.byte_at(0) {
                Some(b'\\') => {
                    self.advance(1);
                    is_escaped = !is_escaped;
                    continue;
                }
                Some(ch) if ch == quote => {
                    self.advance(1);

                    if !is_escaped {
                        return Kind::String;
                    }
                }
                Some(ch) => {
                    if ch.is_ascii() {
                        match ch {
                            b'\n' | b'\r' if !is_escaped => {
                                self.add_diagnostic(
                                    "unterminated string literal",
                                    start..self.index,
                                );

                                return Kind::Error;
                            }
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

            is_escaped = false;
        }

        self.add_diagnostic("unterminated string literal", start..self.index);
        Kind::Error
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
            Equal => self.resolve_equal(),
            Caret => self.resolve_caret(),
            Identifier | Dollar => self.resolve_identifier(),
            Quote => self.resolve_string_literal(),

            GreaterThan => self.eat_byte(Kind::GreaterThan),
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

            _ => Kind::Error,
        }
    }

    pub fn lex(&mut self, ctx: LexContext) -> Token {
        let start;
        let (kind, end, is_on_new_line);

        if self.index >= self.code.len() {
            start = self.index;
            kind = Kind::EOF;
            is_on_new_line = false;
            end = start + 1;
        } else {
            match ctx {
                LexContext::Normal => {
                    let prev_line_number = self.line_number;
                    self.consume_whitespaces_and_newline();

                    is_on_new_line = self.line_number != prev_line_number;

                    if self.index >= self.code.len() {
                        kind = Kind::EOF;
                        start = self.index;
                        end = start + 1;
                    } else {
                        start = self.index;
                        kind = self.get_token();
                        end = self.index;
                    }
                }

                LexContext::Template => {
                    start = self.index;
                    kind = self.lex_template_literal();
                    is_on_new_line = false;
                    end = self.index;
                }

                LexContext::JSXChild => {
                    start = self.index;
                    kind = self.lex_jsx_child();
                    is_on_new_line = false;
                    end = self.index;
                }
            }
        }

        Token {
            kind,
            is_on_new_line,
            start: start as u32,
            end: end as u32,
        }
    }

    pub fn new(code: &'a str) -> Tokenizer<'a> {
        Tokenizer {
            code,
            index: 0,
            line_number: 1,
            diagnostics: Vec::new(),
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
