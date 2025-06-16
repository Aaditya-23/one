use bumpalo::{boxed::Box, collections::Vec as ArenaVec};

use crate::{
    ast::jsx::{
        Attribute, JSXAttribute, JSXAttributeName, JSXAttributeValue, JSXChild, JSXClosingElement,
        JSXClosingFragment, JSXElement, JSXExpressionContainer, JSXFragment, JSXIdentifier,
        JSXMemberExpression, JSXNameSpacedName, JSXOpeningElement, JSXOpeningFragment,
        JSXSpreadAttribute, JSXSpreadChild, JSXTagName, JSXText,
    },
    kind::Kind,
    parser::{ParseResult, Parser},
    tokenizer::LexContext,
};

impl<'a> Parser<'a> {
    pub fn parse_jsx_identifier(&mut self) -> JSXIdentifier {
        let start = self.token.start;

        self.bump();

        JSXIdentifier {
            start,
            end: self.prev_token_end,
        }
    }

    pub fn parse_jsx_tag_name(&mut self) -> ParseResult<JSXTagName<'a>> {
        let start = self.token.start;

        let mut name = if self.lexer.peek(LexContext::Normal).kind == Kind::Colon {
            let namespace = self.parse_jsx_identifier();

            // skip the colon
            self.bump();

            if !self.is_curr_token_identifier() && !self.is_curr_token_a_reserved_keyword() {
                self.report_unexpected_token(Kind::Identifier);
                return Err(());
            }

            JSXTagName::NamespacedName(Box::new_in(
                JSXNameSpacedName {
                    start,
                    namespace,
                    name: self.parse_jsx_identifier(),
                    end: self.prev_token_end,
                },
                self.arena,
            ))
        } else {
            JSXTagName::Identifier(Box::new_in(self.parse_jsx_identifier(), self.arena))
        };

        while self.kind(Kind::Dot) {
            self.bump();

            if !self.is_curr_token_identifier() && !self.is_curr_token_a_reserved_keyword() {
                self.report_unexpected_token(Kind::Identifier);
                return Err(());
            }

            name = JSXTagName::MemberExpression(Box::new_in(
                JSXMemberExpression {
                    start,
                    object: name,
                    property: self.parse_jsx_identifier(),
                    end: self.prev_token_end,
                },
                self.arena,
            ))
        }

        Ok(name)
    }

    pub fn parse_spread_child(&mut self) -> JSXSpreadChild<'a> {
        todo!()
    }

    pub fn parse_jsx_attribute_value(&mut self) -> ParseResult<JSXAttributeValue<'a>> {
        if self.kind(Kind::String) {
            return Ok(JSXAttributeValue::StringLiteral(Box::new_in(
                self.parse_string_literal(),
                self.arena,
            )));
        }

        let start = self.token.start;

        self.bump_token(Kind::BracesO)?;

        let value;

        if self.kind(Kind::Dot3) {
            value = JSXAttributeValue::SpreadChild(Box::new_in(
                JSXSpreadChild {
                    start,
                    expression: self.parse_expression()?,
                    end: self.prev_token_end,
                },
                self.arena,
            ))
        } else {
            value = JSXAttributeValue::ExpressionContainer(Box::new_in(
                JSXExpressionContainer {
                    start,
                    expression: self.parse_expression()?,
                    end: self.prev_token_end,
                },
                self.arena,
            ))
        }

        self.bump_token(Kind::BracesC)?;

        Ok(value)
    }

    pub fn parse_jsx_name_attribute(&mut self) -> ParseResult<Attribute<'a>> {
        let start = self.token.start;

        let name_or_namespace = self.parse_jsx_identifier();

        let attribute_name;
        if self.kind(Kind::Colon) {
            self.bump();

            let name = self.parse_jsx_identifier();
            attribute_name = JSXAttributeName::NamespacedName(Box::new_in(
                JSXNameSpacedName {
                    start,
                    namespace: name_or_namespace,
                    name,
                    end: self.prev_token_end,
                },
                self.arena,
            ));
        } else {
            attribute_name =
                JSXAttributeName::Identifier(Box::new_in(name_or_namespace, self.arena));
        }

        let value = if self.kind(Kind::Equal) {
            self.bump();
            Some(self.parse_jsx_attribute_value()?)
        } else {
            None
        };

        Ok(Attribute::Attribute(Box::new_in(
            JSXAttribute {
                start,
                name: attribute_name,
                value,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_jsx_attributes(&mut self) -> ParseResult<ArenaVec<'a, Attribute<'a>>> {
        let mut attributes = ArenaVec::new_in(self.arena);

        while !self.kind(Kind::EOF) {
            if self.kind(Kind::Identifier) || self.is_curr_token_a_reserved_keyword() {
                attributes.push(self.parse_jsx_name_attribute()?);
            } else if self.kind(Kind::BracesO) {
                let start = self.token.start;

                self.bump();
                self.bump_token(Kind::Dot3)?;

                let argument = self.parse_expression()?;

                attributes.push(Attribute::SpreadAttribute(Box::new_in(
                    JSXSpreadAttribute {
                        start,
                        argument,
                        end: self.prev_token_end,
                    },
                    self.arena,
                )))
            } else {
                break;
            }
        }

        Ok(attributes)
    }

    pub fn parse_jsx_opening_element(&mut self) -> ParseResult<JSXOpeningElement<'a>> {
        let start = self.token.start;

        // skip <
        self.bump();

        let name = self.parse_jsx_tag_name()?;
        let type_parameter_arguments = if self.kind(Kind::LessThan) {
            Some(self.parse_type_parameter_arguments()?)
        } else {
            None
        };

        let attributes = self.parse_jsx_attributes()?;

        let is_self_closing = if self.kind(Kind::Slash) {
            self.bump();
            true
        } else {
            false
        };

        self.bump_token(Kind::GreaterThan)?;

        Ok(JSXOpeningElement {
            start,
            name,
            self_closing: is_self_closing,
            attributes,
            type_parameter_arguments,
            end: self.prev_token_end,
        })
    }

    pub fn parse_jsx_exp_in_container(&mut self) -> ParseResult<JSXChild<'a>> {
        let start = self.token.start;

        // skip {
        self.bump();

        let child;

        if self.kind(Kind::Dot3) {
            self.bump();

            child = JSXChild::SpreadChild(Box::new_in(
                JSXSpreadChild {
                    start,
                    expression: self.parse_expression()?,
                    end: self.prev_token_end,
                },
                self.arena,
            ))
        } else {
            child = JSXChild::ExpressionContainer(Box::new_in(
                JSXExpressionContainer {
                    start,
                    expression: self.parse_expression()?,
                    end: self.prev_token_end,
                },
                self.arena,
            ))
        }

        self.assert_token(Kind::BracesC)?;
        self.bump_with_ctx(LexContext::JSXChild);

        Ok(child)
    }

    pub fn parse_jsx_children(&mut self) -> ParseResult<ArenaVec<'a, JSXChild<'a>>> {
        let mut children = ArenaVec::new_in(self.arena);

        self.token = self.lexer.relex_jsx_child(self.token.clone());

        loop {
            if self.kind(Kind::EOF) {
                self.add_diagnostic("unterminated jsx expression", self.token.range());
                return Err(());
            }

            let child = match self.token.kind {
                Kind::BracesO => self.parse_jsx_exp_in_container()?,
                Kind::LessThan => {
                    let peeked_token = self.lexer.peek(LexContext::Normal).kind;

                    if peeked_token == Kind::Slash {
                        break;
                    } else if peeked_token == Kind::GreaterThan {
                        let fragment = self.parse_jsx_fragment()?;
                        self.token = self.lexer.relex_jsx_child(self.token.clone());

                        JSXChild::Fragment(Box::new_in(fragment, self.arena))
                    } else {
                        let element = self.parse_jsx_element()?;
                        self.token = self.lexer.relex_jsx_child(self.token.clone());

                        JSXChild::Element(Box::new_in(element, self.arena))
                    }
                }
                Kind::JSXText => {
                    let start = self.token.start;

                    self.bump_with_ctx(LexContext::JSXChild);

                    JSXChild::Text(Box::new_in(
                        JSXText {
                            start,
                            end: self.prev_token_end,
                        },
                        self.arena,
                    ))
                }
                _ => {
                    self.add_diagnostic("invalid token", self.token.range());
                    return Err(());
                }
            };

            children.push(child);
        }

        Ok(children)
    }

    pub fn parse_jsx_closing_element(&mut self) -> ParseResult<JSXClosingElement<'a>> {
        let start = self.token.start;

        self.bump_token(Kind::LessThan)?;
        self.bump_token(Kind::Slash)?;

        let name = self.parse_jsx_tag_name()?;

        self.bump_token(Kind::GreaterThan)?;

        Ok(JSXClosingElement {
            start,
            name,
            end: self.prev_token_end,
        })
    }

    pub fn parse_jsx_element(&mut self) -> ParseResult<JSXElement<'a>> {
        let start = self.token.start;

        let opening_element = self.parse_jsx_opening_element()?;

        let children = if opening_element.self_closing {
            ArenaVec::new_in(self.arena)
        } else {
            self.parse_jsx_children()?
        };

        let closing_element = if opening_element.self_closing {
            None
        } else {
            Some(self.parse_jsx_closing_element()?)
        };

        Ok(JSXElement {
            start,
            opening_element,
            children,
            closing_element,
            end: self.prev_token_end,
        })
    }

    pub fn parse_jsx_fragment(&mut self) -> ParseResult<JSXFragment<'a>> {
        let start = self.token.start;

        // skip <
        self.bump();

        self.bump_token(Kind::GreaterThan)?;

        let opening_element = JSXOpeningFragment {
            start,
            end: self.prev_token_end,
        };

        let children = self.parse_jsx_children()?;

        let closing_el_start = self.token.start;

        self.bump_token(Kind::LessThan)?;
        self.bump_token(Kind::Slash)?;
        self.bump_token(Kind::GreaterThan)?;

        let closing_element = JSXClosingFragment {
            start: closing_el_start,
            end: self.prev_token_end,
        };

        Ok(JSXFragment {
            start,
            opening_element,
            children,
            closing_element,
            end: self.prev_token_end,
        })
    }
}
