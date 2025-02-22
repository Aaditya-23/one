use nodes::{
    Children, ClosingElement, Element, Fragment, JSXAttribute, JSXElement, OpeningElement,
};

use crate::{
    nodes::{expressions::Expression, Identifier, Literal},
    parser::Parser,
    tokenizer::{LocationData, Token, TokenEntry},
};

pub mod nodes {
    use crate::nodes::{expressions::Expression, Identifier, Literal};

    #[derive(Debug, Clone)]
    pub enum JSXAttribute {
        Literal {
            name: Identifier,
            value: Literal,
        },
        JSXExpressionContainer {
            name: Identifier,
            expression: Expression,
        },
        Spread(Expression),
    }

    #[derive(Debug, Clone)]
    pub struct OpeningElement {
        pub name: Identifier,
        pub self_closing: bool,
        pub attributes: Vec<JSXAttribute>,
    }

    impl OpeningElement {
        pub fn new(name: Identifier, self_closing: bool, attributes: Vec<JSXAttribute>) -> Self {
            Self {
                name,
                self_closing,
                attributes,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum Children {
        JSXText(String),
        JSXExpressionContainer(Expression),
        JSXElement(JSXElement),
    }

    #[derive(Debug, Clone)]
    pub struct ClosingElement {
        name: Identifier,
    }

    impl ClosingElement {
        pub fn new(name: Identifier) -> Self {
            Self { name }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Element {
        pub opening_element: OpeningElement,
        pub children: Vec<Children>,
        pub closing_element: Option<ClosingElement>,
    }

    impl Element {
        pub fn new(
            opening_element: OpeningElement,
            children: Vec<Children>,
            closing_element: Option<ClosingElement>,
        ) -> Self {
            Self {
                opening_element,
                children,
                closing_element,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Fragment {
        pub opening_fragment: (),
        pub children: Vec<Children>,
        pub closing_fragment: (),
    }

    impl Fragment {
        pub fn new(children: Vec<Children>) -> Self {
            Self {
                opening_fragment: (),
                children,
                closing_fragment: (),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum JSXElement {
        Element(Element),
        Fragment(Fragment),
    }
}

#[derive(Debug)]
pub struct JSX<'a> {
    parser: &'a mut Parser,
    is_parsing_children: bool,
}

impl<'a> JSX<'a> {
    pub fn new(parser: &'a mut Parser) -> Self {
        Self {
            parser,
            is_parsing_children: false,
        }
    }

    pub fn skip_next_jsx_token(&mut self) {
        self.parser.next_token = self.lex_jsx();
    }

    pub fn expect_jsx(&mut self, expect: &str) {
        if self.parser.next_token.value != expect {
            panic!(
                "Expected {}, got \"{}\"",
                expect, self.parser.next_token.value
            );
        }

        self.skip_next_jsx_token();
    }

    pub fn expect_jsx_without_skipping_token(&mut self, expect: &str) {
        if self.parser.next_token.value != expect {
            panic!(
                "Expected {}, got \"{}\"",
                expect, self.parser.next_token.value
            );
        }
    }

    pub fn lex_jsx(&mut self) -> TokenEntry {
        let t = &mut self.parser.tokenizer;
        if t.index >= t.code.len() {
            return TokenEntry {
                type_: Token::EOF,
                value: "".to_string(),
                head: false,
                tail: false,
                regex: None,

                location: LocationData::new(t.record_start_cordinate(), t.record_start_cordinate())
            };
        }

        let ch = t.code[t.index];

        if ch == '/' {
            t.index += 1;

            return TokenEntry {
                type_: Token::Punctuator,
                value: ch.to_string(),
                head: false,
                tail: false,
                regex: None,
                location: LocationData::new(t.record_start_cordinate(), t.record_start_cordinate())
            };
        }

        t.get_next_token().unwrap()
    }

    pub fn get_next_jsx_token(&mut self) -> TokenEntry {
        let new_token = self.lex_jsx();
        let old_token = self.parser.next_token.clone();
        self.parser.next_token = new_token;

        old_token
    }

    // returns the token next to the lookahead token and restores the tokenizer state
    pub fn peek(&mut self) -> TokenEntry {
        let state = self.parser.tokenizer.capture_state();
        let token = self.lex_jsx();
        self.parser.tokenizer.restore_tokenizer(state);

        token
    }

    pub fn get_jsx_text(&mut self) -> String {
        let t = &mut self.parser.tokenizer;
        let start = t.index;

        while t.index < t.code.len() {
            let ch = t.code[t.index];
            if ch == '<' || ch == '{' {
                break;
            }

            t.index += 1;
        }

        t.code[start..t.index].iter().collect()
    }

    pub fn parse_attributes(&mut self) -> Vec<JSXAttribute> {
        let mut attributes = vec![];
        loop {
            if self.parser.match_next_token("/") || self.parser.match_next_token(">") {
                break;
            }

            let attribute;

            if let Token::Identifier = self.parser.next_token.type_ {
                let token = self.get_next_jsx_token();
                let name = Identifier::new(&token.value);

                self.expect_jsx("=");

                if let Token::String = self.parser.next_token.type_ {
                    let token = self.get_next_jsx_token();
                    let literal = Literal::new(&token.value);

                    attribute = JSXAttribute::Literal {
                        name,
                        value: literal,
                    };
                } else if self.parser.match_next_token("{") {
                    self.skip_next_jsx_token();

                    let expression = self.parser.parse_assignment_expression();
                    attribute = JSXAttribute::JSXExpressionContainer { name, expression };

                    self.expect_jsx("}");
                } else {
                    panic!("invalid token");
                }
            } else if self.parser.match_next_token("{") {
                self.skip_next_jsx_token();
                self.expect_jsx("...");

                let expression = self.parser.parse_assignment_expression();
                self.expect_jsx("}");

                attribute = JSXAttribute::Spread(expression);
            } else {
                panic!("invalid token");
            }

            attributes.push(attribute);
        }

        attributes
    }

    pub fn parse_jsx_expression_container(&mut self) -> Expression {
        // skip { token
        self.skip_next_jsx_token();

        let expression = self.parser.parse_assignment_expression();

        if self.is_parsing_children {
            self.expect_jsx_without_skipping_token("}");
        } else {
            self.expect_jsx("}");
        }

        expression
    }

    pub fn parse_jsx_children(&mut self) -> Vec<Children> {
        let mut children = vec![];

        let prev_state = self.is_parsing_children;
        self.is_parsing_children = true;

        loop {
            let child;
            let jsx_text = self.get_jsx_text();

            // skip the ending > token of the jsx tag here.
            self.skip_next_jsx_token();

            if jsx_text.len() > 0 {
                children.push(Children::JSXText(jsx_text));
            }

            if self.parser.match_next_token("<") {
                let token = self.peek();
                if token.value == "/" {
                    break;
                }

                child = Children::JSXElement(self.parse());
            } else if self.parser.match_next_token("{") {
                child = Children::JSXExpressionContainer(self.parse_jsx_expression_container())
            } else {
                panic!("invalid token");
            }

            children.push(child);
        }

        self.is_parsing_children = prev_state;
        children
    }

    pub fn parse_jsx_fragment(&mut self) -> Fragment {
        // do not skip the > token here, otherwise whitespaces which are children of the fragment will be skipped.

        let children = self.parse_jsx_children();

        self.expect_jsx("<");
        self.expect_jsx("/");

        if self.is_parsing_children {
            self.expect_jsx_without_skipping_token(">");
        } else {
            self.expect_jsx(">");
        }
        Fragment::new(children)
    }

    pub fn parse_jsx_element(&mut self) -> Element {
        if let Token::Identifier = self.parser.next_token.type_ {
            let token = self.get_next_jsx_token();
            let attributes = self.parse_attributes();

            let is_self_closing = if self.parser.match_next_token("/") {
                self.skip_next_jsx_token();
                true
            } else {
                false
            };

            let opening_element =
                OpeningElement::new(Identifier::new(&token.value), is_self_closing, attributes);

            if is_self_closing {
                return Element::new(opening_element, vec![], None);
            }

            // do not use expect_jsx here, otherwise whitespaces which are children of the element will be skipped.
            if !self.parser.match_next_token(">") {
                panic!("expected > token");
            }

            let children = self.parse_jsx_children();

            self.expect_jsx("<");
            self.expect_jsx("/");
            self.expect_jsx(&token.value);

            if self.is_parsing_children {
                self.expect_jsx_without_skipping_token(">");
            } else {
                self.expect_jsx(">");
            }

            let closing_element = ClosingElement::new(Identifier::new(&token.value));

            Element::new(opening_element, children, Some(closing_element))
        } else {
            panic!("invalid token",);
        }
    }

    pub fn parse(&mut self) -> JSXElement {
        self.expect_jsx("<");

        let jsx_el;

        if self.parser.match_next_token(">") {
            jsx_el = JSXElement::Fragment(self.parse_jsx_fragment());
        } else if self.parser.match_next_token("/") {
            panic!("invalid token");
        } else {
            jsx_el = JSXElement::Element(self.parse_jsx_element());
        }

        if !self.is_parsing_children {
            if self.parser.match_next_token("<") {
                panic!("adjacent JSX elements must be wrapped in an enclosing tag.")
            }
        }

        jsx_el
    }
}
