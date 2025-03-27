use bumpalo::{boxed::Box, collections::Vec, Bump};

use crate::{
    allocator::CloneIn,
    ast::{
        ArrayExpression, ArrayPattern, ArrayPatternKind, ArrowFunction, ArrowFunctionBody,
        AssignmentExpression, AssignmentExpressionLHS, AssignmentOperator, AssignmentPattern,
        AwaitExpression, BinaryExpression, BinaryOperator, Block, BooleanLiteral, BreakStatement,
        CallExpression, CatchClause, Class, ClassBody, ClassMethod, ClassProperty,
        ConditionalExpression, ContinueStatement, DebuggerStatement, DoWhileLoop, Elision,
        EmptyStatement, Expression, ExpressionStatement, ForInLoop, ForInLoopLeft, ForLoop,
        ForLoopInit, Function, FunctionParams, Identifier, IdentifierOrLiteral, IfStatement,
        Import, ImportAttribute, ImportDeclaration, ImportDefaultSpecifier,
        ImportNamespaceSpecifier, ImportSpecifier, ImportSpecifierType, LabelledStatement,
        Location, LogicalExpression, LogicalOperator, MemberExpression, MethodDefinitionKind,
        NewExpression, NullLiteral, NumericLiteral, ObjectExpression, ObjectExpressionProperty,
        ObjectExpressionPropertyKind, ObjectPattern, ObjectPatternProperty,
        ObjectPatternPropertyKind, Pattern, PropertyKind, Regexp, RestElement, ReturnStatement,
        SequenceExpression, SpreadElement, Statement, StringLiteral, SwitchCase, SwitchStatement,
        TemplateElement, TemplateLiteral, ThisExpression, ThrowStatement, TryStatement,
        UnaryExpression, UnaryOperator, UpdateExpression, UpdateOperator, VariableDeclaration,
        VariableDeclarationKind, VariableDeclarator, WhileLoop, WithStatement, AST,
    },
    kind::Kind,
    tokenizer::{LexContext, Token, Tokenizer},
};

pub struct Parser<'a> {
    code: &'a str,
    lexer: Tokenizer<'a>,
    token: Token,
    prev_token_end: u32,
    in_paren: bool,
    arena: &'a Bump,
}

impl<'a> Parser<'a> {
    pub fn bump(&mut self) {
        self.prev_token_end = self.token.end;
        self.token = self.lexer.lex(LexContext::Normal);
    }

    pub fn bump_with_ctx(&mut self, ctx: LexContext) {
        self.prev_token_end = self.token.end;
        self.token = self.lexer.lex(ctx);
    }

    pub fn expect(&mut self, kind: Kind) {
        if self.token.kind != kind {
            panic!(
                "unexpected token, expected {:?} found {:?} at {}",
                kind, self.token.kind, self.lexer.line_number
            );
        }

        self.bump();
    }

    pub fn kind(&self, kind: Kind) -> bool {
        self.token.kind == kind
    }

    pub fn get_token_value(&self) -> &'a str {
        unsafe {
            self.code
                .get_unchecked(self.token.start as usize..self.token.end as usize)
        }
    }

    pub fn consume_token(&mut self) -> Kind {
        let kind = self.token.kind;
        self.bump();
        kind
    }

    pub fn get_unary_operator(&self) -> Option<UnaryOperator> {
        match self.token.kind {
            Kind::Plus => Some(UnaryOperator::Plus),
            Kind::Minus => Some(UnaryOperator::Minus),
            Kind::Tilde => Some(UnaryOperator::Tilde),
            Kind::Bang => Some(UnaryOperator::Bang),
            Kind::Typeof => Some(UnaryOperator::Typeof),
            Kind::Void => Some(UnaryOperator::Void),
            Kind::Delete => Some(UnaryOperator::Delete),
            _ => None,
        }
    }

    pub fn get_binary_precedence(&self) -> u8 {
        match self.token.kind {
            Kind::Question2 => 1,
            Kind::Pipe2 => 2,
            Kind::Ampersand2 => 3,
            Kind::Pipe => 4,
            Kind::Caret => 5,
            Kind::Ampersand => 6,
            Kind::Equal2 | Kind::NotEqual | Kind::Equal3 | Kind::NotEqual2 => 7,
            Kind::LessThan
            | Kind::LessThanOrEqual
            | Kind::GreaterThan
            | Kind::GreaterThanOrEqual => 8,
            Kind::LeftShift | Kind::RightShift | Kind::UnsignedRightShift => 9,
            Kind::Plus | Kind::Minus => 10,
            Kind::Star | Kind::Slash | Kind::Mod => 11,
            _ => 0,
        }
    }

    pub fn get_binary_operator(&mut self, kind: Kind) -> BinaryOperator {
        match kind {
            Kind::Equal2 => BinaryOperator::Equality,
            Kind::NotEqual => BinaryOperator::Inequality,
            Kind::Equal3 => BinaryOperator::StrictEquality,
            Kind::NotEqual2 => BinaryOperator::StrictInequality,
            Kind::LessThan => BinaryOperator::LessThan,
            Kind::LessThanOrEqual => BinaryOperator::LessThanOrEqual,
            Kind::GreaterThan => BinaryOperator::GreaterThan,
            Kind::GreaterThanOrEqual => BinaryOperator::GreaterThanOrEqual,
            Kind::LeftShift => BinaryOperator::LeftShift,
            Kind::RightShift => BinaryOperator::RightShift,
            Kind::UnsignedRightShift => BinaryOperator::UnsignedRightShift,
            Kind::Plus => BinaryOperator::Add,
            Kind::Minus => BinaryOperator::Subtract,
            Kind::Star => BinaryOperator::Multiply,
            Kind::Star2 => BinaryOperator::Exponentiation,
            Kind::Slash => BinaryOperator::Divide,
            Kind::Mod => BinaryOperator::Mod,
            Kind::Ampersand => BinaryOperator::BitwiseAND,
            Kind::Pipe => BinaryOperator::BitwiseOR,
            Kind::Caret => BinaryOperator::BitwiseXOR,
            Kind::Question2 => BinaryOperator::Coalescing,
            Kind::In => BinaryOperator::In,
            Kind::Instanceof => BinaryOperator::Instanceof,
            _ => unsafe { core::hint::unreachable_unchecked() },
        }
    }

    pub fn get_assignment_operator(&self, kind: Kind) -> Option<AssignmentOperator> {
        let op = match kind {
            Kind::Equal => AssignmentOperator::Equal,
            Kind::StarEqual => AssignmentOperator::Multiply,
            Kind::Star2Equal => AssignmentOperator::Exponentiation,
            Kind::SlashEqual => AssignmentOperator::Divide,
            Kind::ModEqual => AssignmentOperator::Mod,
            Kind::PlusEqual => AssignmentOperator::Plus,
            Kind::MinusEqual => AssignmentOperator::Minus,
            Kind::LeftShiftEqual => AssignmentOperator::LeftShift,
            Kind::RightShiftEqual => AssignmentOperator::RightShift,
            Kind::UnsignedRightShiftEqual => AssignmentOperator::UnsignedRightShift,
            Kind::AmpersandEqual => AssignmentOperator::BitwiseAND,
            Kind::PipeEqual => AssignmentOperator::BitwiseOR,
            Kind::CaretEqual => AssignmentOperator::BitwiseXOR,
            _ => return None,
        };

        Some(op)
    }

    pub fn get_logical_operator(&self, kind: Kind) -> LogicalOperator {
        match kind {
            Kind::Ampersand2 => LogicalOperator::And,
            Kind::Pipe2 => LogicalOperator::Or,
            _ => unsafe { core::hint::unreachable_unchecked() },
        }
    }

    pub fn parse_identifier(&mut self) -> Identifier {
        let start = self.token.start;
        self.bump();

        Identifier {
            start,
            end: self.prev_token_end,
            parenthesized: self.in_paren,
        }
    }

    pub fn parse_pattern_with_default_value(&mut self) -> Pattern<'a> {
        let start = self.token.start;
        let pattern = self.parse_pattern();

        if self.kind(Kind::Equal) {
            self.bump();
            let right = self.parse_assignment_expression();

            return Pattern::AssignmentPattern(Box::new_in(
                AssignmentPattern {
                    start,
                    left: pattern,
                    right,
                    end: self.prev_token_end,
                },
                self.arena,
            ));
        }

        pattern
    }

    pub fn parse_rest_element(&mut self) -> RestElement<'a> {
        let start = self.token.start;

        // skip ...
        self.bump();

        if self.token.kind != Kind::Identifier {
            panic!("invalid token kind");
        }

        let argument = self.parse_identifier_pattern();
        RestElement {
            start,
            argument,
            end: self.prev_token_end,
        }
    }

    pub fn parse_identifier_pattern(&mut self) -> Pattern<'a> {
        let identifier = self.parse_identifier();
        Pattern::IdentifierPattern(Box::new_in(identifier, self.arena))
    }

    pub fn parse_array_pattern(&mut self) -> Pattern<'a> {
        let start = self.token.start;

        // skip [
        self.bump();

        let mut elements = Vec::new_in(self.arena);

        while !matches!(self.token.kind, Kind::EOF | Kind::BracketC) {
            if self.kind(Kind::Comma) {
                elements.push(None);
            } else if self.kind(Kind::Dot3) {
                let array_pattern = ArrayPatternKind::RestElement(Box::new_in(
                    self.parse_rest_element(),
                    self.arena,
                ));

                elements.push(Some(array_pattern));
            } else {
                let array_pattern = ArrayPatternKind::Pattern(Box::new_in(
                    self.parse_pattern_with_default_value(),
                    self.arena,
                ));

                elements.push(Some(array_pattern));
            }

            if !self.kind(Kind::BracketC) {
                self.expect(Kind::Comma);
            }
        }

        self.expect(Kind::BracketC);

        Pattern::ArrayPattern(Box::new_in(
            ArrayPattern {
                start,
                elements,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_object_pattern_property(&mut self) -> ObjectPatternProperty<'a> {
        let start = self.token.start;
        let (key, value, shorthand, computed);

        if self.kind(Kind::BracketO) {
            self.bump();
            key = self.parse_assignment_expression();

            self.expect(Kind::BracketC);
            self.expect(Kind::Colon);

            value = self.parse_pattern_with_default_value();

            computed = true;
            shorthand = false;
        } else {
            if !self.kind(Kind::Identifier) {
                panic!("invalid token kind");
            }

            let id = self.parse_identifier();
            key = Expression::Identifier(Box::new_in(id.clone_in(self.arena), self.arena));

            if self.kind(Kind::Equal) {
                self.bump();
                let right = self.parse_assignment_expression();

                value = Pattern::AssignmentPattern(Box::new_in(
                    AssignmentPattern {
                        start,
                        left: Pattern::IdentifierPattern(Box::new_in(id, self.arena)),
                        right,
                        end: self.prev_token_end,
                    },
                    self.arena,
                ));

                shorthand = true;
            } else if self.kind(Kind::Colon) {
                self.bump();
                value = self.parse_pattern_with_default_value();
                shorthand = false;
            } else {
                value = Pattern::IdentifierPattern(Box::new_in(id, self.arena));
                shorthand = true;
            }

            computed = false;
        }

        ObjectPatternProperty {
            start,
            key,
            value,
            shorthand,
            computed,
            end: self.prev_token_end,
        }
    }

    pub fn parse_object_pattern(&mut self) -> Pattern<'a> {
        let start = self.token.start;
        let mut properties = Vec::new_in(self.arena);

        // skip {
        self.bump();

        while !matches!(self.token.kind, Kind::EOF | Kind::BracesC) {
            if self.token.kind == Kind::Dot3 {
                let rest_element = Box::new_in(self.parse_rest_element(), self.arena);
                properties.push(ObjectPatternPropertyKind::RestElement(rest_element));
            } else {
                let property = Box::new_in(self.parse_object_pattern_property(), self.arena);

                properties.push(ObjectPatternPropertyKind::Property(property))
            }

            if !self.kind(Kind::BracesC) {
                self.expect(Kind::Comma);
            }
        }

        self.expect(Kind::BracesC);

        Pattern::ObjectPattern(Box::new_in(
            ObjectPattern {
                start,
                properties,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_pattern(&mut self) -> Pattern<'a> {
        match self.token.kind {
            Kind::Identifier => self.parse_identifier_pattern(),
            Kind::BracketO => self.parse_array_pattern(),
            Kind::BracesO => self.parse_object_pattern(),
            _ => panic!("invalid pattern"),
        }
    }

    pub fn reinterpret_as_pattern(&mut self, exp: Expression<'a>) -> Pattern<'a> {
        match exp {
            Expression::Identifier(id) => Pattern::IdentifierPattern(id),
            Expression::ArrayExpression(array) => {
                let mut elements = Vec::new_in(self.arena);
                array.elements.iter().for_each(|el| match el {
                    Expression::Elision(_) => elements.push(None),
                    Expression::SpreadElement(inner) => {
                        let array_pattern = ArrayPatternKind::RestElement(Box::new_in(
                            RestElement {
                                start: inner.start,
                                argument: self
                                    .reinterpret_as_pattern(inner.argument.clone_in(self.arena)),
                                end: inner.end,
                            },
                            self.arena,
                        ));

                        elements.push(Some(array_pattern))
                    }
                    _ => elements.push(Some(ArrayPatternKind::Pattern(Box::new_in(
                        self.reinterpret_as_pattern(el.clone_in(self.arena)),
                        self.arena,
                    )))),
                });

                Pattern::ArrayPattern(Box::new_in(
                    ArrayPattern {
                        start: array.start,
                        elements,
                        end: array.end,
                    },
                    self.arena,
                ))
            }
            Expression::ObjectExpression(object) => {
                let mut properties = Vec::new_in(self.arena);

                object.properties.iter().for_each(|property| {
                    let object_pattern_property = match property {
                        ObjectExpressionPropertyKind::SpreadElement(spread_el) => {
                            ObjectPatternPropertyKind::RestElement(Box::new_in(
                                RestElement {
                                    start: spread_el.start,
                                    argument: self.reinterpret_as_pattern(
                                        spread_el.argument.clone_in(self.arena),
                                    ),
                                    end: spread_el.end,
                                },
                                self.arena,
                            ))
                        }
                        ObjectExpressionPropertyKind::Property(property) => {
                            ObjectPatternPropertyKind::Property(Box::new_in(
                                ObjectPatternProperty {
                                    start: property.start,
                                    computed: property.computed,
                                    shorthand: property.shorthand,
                                    key: property.key.clone_in(self.arena),
                                    value: self.reinterpret_as_pattern(
                                        property.value.clone_in(self.arena),
                                    ),
                                    end: property.end,
                                },
                                self.arena,
                            ))
                        }
                    };

                    properties.push(object_pattern_property);
                });

                Pattern::ObjectPattern(Box::new_in(
                    ObjectPattern {
                        start: object.start,
                        properties,
                        end: object.end,
                    },
                    self.arena,
                ))
            }
            Expression::AssignmentExpression(exp) => {
                let start = exp.start;
                let left = match &exp.left {
                    AssignmentExpressionLHS::Pattern(pattern) => {
                        pattern.as_ref().clone_in(self.arena)
                    }
                    AssignmentExpressionLHS::Expression(expression) => {
                        self.reinterpret_as_pattern(expression.as_ref().clone_in(self.arena))
                    }
                };

                Pattern::AssignmentPattern(Box::new_in(
                    AssignmentPattern {
                        start,
                        left,
                        right: exp.right.clone_in(self.arena),
                        end: exp.end,
                    },
                    self.arena,
                ))
            }
            _ => panic!(
                "invalid expression, {:#?} {}",
                self.token.kind, self.lexer.line_number
            ),
        }
    }

    pub fn parse_numeric_literal(&mut self) -> Expression<'a> {
        let start = self.token.start;

        // skip numeric literal
        self.bump();

        Expression::NumericLiteral(Box::new_in(
            NumericLiteral {
                start,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_string_literal(&mut self) -> StringLiteral {
        let start = self.token.start;
        self.bump();

        StringLiteral {
            start,
            end: self.prev_token_end,
        }
    }

    pub fn parse_string_literal_expression(&mut self) -> Expression<'a> {
        let literal = self.parse_string_literal();

        Expression::StringLiteral(Box::new_in(literal, self.arena))
    }

    pub fn parse_boolean_literal(&mut self) -> Expression<'a> {
        let start = self.token.start;
        self.bump();

        Expression::BooleanLiteral(Box::new_in(
            BooleanLiteral {
                start,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_null_literal(&mut self) -> Expression<'a> {
        let start = self.token.start;
        self.bump();

        Expression::NullLiteral(Box::new_in(
            NullLiteral {
                start,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_group_expression(&mut self) -> Expression<'a> {
        let start = self.token.start;

        // skip (
        self.bump();

        if self.kind(Kind::ParenC) {
            self.bump();
            self.expect(Kind::Arrow);

            let (body, is_exp) = self.parse_arrow_function_body();

            Expression::ArrowFunctionExpression(Box::new_in(
                ArrowFunction {
                    start,
                    async_: false,
                    expression: is_exp,
                    params: Vec::new_in(self.arena),
                    body,
                    end: self.prev_token_end,
                },
                self.arena,
            ))
        } else if self.kind(Kind::Dot3) {
            let rest_el = self.parse_rest_element();
            let mut params = Vec::new_in(self.arena);

            params.push(FunctionParams::RestElement(Box::new_in(
                rest_el, self.arena,
            )));

            self.expect(Kind::ParenC);
            self.expect(Kind::Arrow);

            let (body, is_exp) = self.parse_arrow_function_body();

            Expression::ArrowFunctionExpression(Box::new_in(
                ArrowFunction {
                    start,
                    async_: false,
                    expression: is_exp,
                    params,
                    body,
                    end: self.prev_token_end,
                },
                self.arena,
            ))
        } else {
            self.in_paren = true;
            let exp = self.parse_assignment_expression();

            if self.kind(Kind::Comma) {
                let mut expressions = Vec::new_in(self.arena);

                while !matches!(self.token.kind, Kind::EOF | Kind::ParenC) {
                    self.expect(Kind::Comma);

                    if self.kind(Kind::Dot3) {
                        let rest_element = self.parse_rest_element();
                        self.expect(Kind::ParenC);
                        self.expect(Kind::Arrow);

                        let mut params = Vec::new_in(self.arena);

                        expressions.into_iter().for_each(|exp| {
                            params.push(FunctionParams::Pattern(Box::new_in(
                                self.reinterpret_as_pattern(exp),
                                self.arena,
                            )))
                        });

                        params.push(FunctionParams::RestElement(Box::new_in(
                            rest_element,
                            self.arena,
                        )));

                        let (body, is_exp) = self.parse_arrow_function_body();

                        self.in_paren = false;

                        return Expression::ArrowFunctionExpression(Box::new_in(
                            ArrowFunction {
                                start,
                                async_: false,
                                expression: is_exp,
                                params,
                                body,
                                end: self.prev_token_end,
                            },
                            self.arena,
                        ));
                    } else {
                        expressions.push(self.parse_assignment_expression());
                    }
                }

                self.expect(Kind::ParenC);
                self.in_paren = false;

                if self.kind(Kind::Arrow) {
                    self.bump();

                    let mut params = Vec::new_in(self.arena);

                    expressions.into_iter().for_each(|exp| {
                        params.push(FunctionParams::Pattern(Box::new_in(
                            self.reinterpret_as_pattern(exp),
                            self.arena,
                        )));
                    });

                    let (body, expression) = self.parse_arrow_function_body();

                    Expression::ArrowFunctionExpression(Box::new_in(
                        ArrowFunction {
                            start,
                            async_: false,
                            expression,
                            params,
                            body,
                            end: self.prev_token_end,
                        },
                        self.arena,
                    ))
                } else {
                    Expression::SequenceExpression(Box::new_in(
                        SequenceExpression {
                            start,
                            expressions,
                            end: self.prev_token_end,
                            parenthesized: true,
                        },
                        self.arena,
                    ))
                }
            } else {
                self.expect(Kind::ParenC);
                self.in_paren = false;

                if self.kind(Kind::Arrow) {
                    self.bump();

                    let mut params = Vec::new_in(self.arena);
                    params.push(FunctionParams::Pattern(Box::new_in(
                        self.reinterpret_as_pattern(exp),
                        self.arena,
                    )));

                    let (body, is_exp) = self.parse_arrow_function_body();

                    Expression::ArrowFunctionExpression(Box::new_in(
                        ArrowFunction {
                            start,
                            async_: false,
                            expression: is_exp,
                            params,
                            body,
                            end: self.prev_token_end,
                        },
                        self.arena,
                    ))
                } else {
                    exp
                }
            }
        }
    }

    pub fn parse_spread_element(&mut self) -> Expression<'a> {
        let start = self.token.start;

        // skip ...
        self.bump();

        let argument = self.parse_assignment_expression();

        Expression::SpreadElement(Box::new_in(
            SpreadElement {
                start,
                argument,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_elision(&mut self) -> Expression<'a> {
        let start = self.token.start;
        self.bump();

        Expression::Elision(Box::new_in(
            Elision {
                start,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_array_expression(&mut self) -> Expression<'a> {
        let start = self.token.start;

        // skip [
        self.bump();

        let mut elements = Vec::new_in(self.arena);

        while !matches!(self.token.kind, Kind::EOF | Kind::BracketC) {
            if self.kind(Kind::Comma) {
                elements.push(self.parse_elision())
            } else {
                if self.kind(Kind::Dot3) {
                    elements.push(self.parse_spread_element())
                } else {
                    elements.push(self.parse_assignment_expression());
                }

                if !self.kind(Kind::BracketC) {
                    self.expect(Kind::Comma);
                }
            }
        }

        self.expect(Kind::BracketC);

        Expression::ArrayExpression(Box::new_in(
            ArrayExpression {
                start,
                elements,
                end: self.prev_token_end,
                parenthesized: self.in_paren,
            },
            self.arena,
        ))
    }

    pub fn parse_object_property_method(&mut self, is_async: bool) -> Expression<'a> {
        let start = self.token.start;
        let params = self.parse_function_parameters();
        let body = self.parse_block();

        Expression::FunctionExpression(Box::new_in(
            Function {
                start,
                id: None,
                params,
                body,
                generator: false,
                async_: is_async,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_object_expression_property(&mut self) -> ObjectExpressionProperty<'a> {
        let start = self.token.start;

        let (is_computed, is_shorthand, is_method, kind, value);
        let key;

        if matches!(
            self.token.kind,
            Kind::Identifier
                | Kind::Case
                | Kind::Default
                | Kind::In
                | Kind::As
                | Kind::Number
                | Kind::String
                | Kind::From
        ) {
            key = Expression::Identifier(Box::new_in(self.parse_identifier(), self.arena));
            is_computed = false;
        } else if self.kind(Kind::BracketO) {
            self.bump();
            key = self.parse_assignment_expression();
            self.expect(Kind::BracketC);
            is_computed = true
        } else {
            panic!(
                "invalid token kind {:?} {}",
                self.token.kind, self.lexer.line_number
            );
        }

        if self.kind(Kind::Colon) {
            self.bump();

            value = self.parse_assignment_expression();
            is_shorthand = false;
            is_method = false;
            kind = PropertyKind::Init;
        } else if self.kind(Kind::ParenO) {
            is_method = true;
            is_shorthand = false;
            kind = PropertyKind::Init;

            value = self.parse_object_property_method(false);
        } else if is_computed {
            panic!("invalid token kind")
        } else {
            value = key.clone_in(self.arena);
            is_shorthand = true;
            is_method = false;
            kind = PropertyKind::Init
        }

        ObjectExpressionProperty {
            start,
            computed: is_computed,
            shorthand: is_shorthand,
            method: is_method,
            kind,
            key,
            value,
            end: self.prev_token_end,
        }
    }

    pub fn parse_object_expression(&mut self) -> Expression<'a> {
        let start = self.token.start;
        let mut properties = Vec::new_in(self.arena);

        // skip {
        self.bump();

        while !matches!(self.token.kind, Kind::EOF | Kind::BracesC) {
            if self.kind(Kind::Dot3) {
                let el = match self.parse_spread_element() {
                    Expression::SpreadElement(spread_el) => spread_el,
                    _ => unsafe { core::hint::unreachable_unchecked() },
                };

                properties.push(ObjectExpressionPropertyKind::SpreadElement(el));
            } else {
                let property = Box::new_in(self.parse_object_expression_property(), self.arena);

                properties.push(ObjectExpressionPropertyKind::Property(property));
            }

            if !self.kind(Kind::BracesC) {
                self.expect(Kind::Comma)
            }
        }

        self.expect(Kind::BracesC);

        Expression::ObjectExpression(Box::new_in(
            ObjectExpression {
                start,
                properties,
                end: self.prev_token_end,
                parenthesized: self.in_paren,
            },
            self.arena,
        ))
    }

    pub fn parse_function_declaration(&mut self) -> Statement<'a> {
        let func = self.parse_function(false);

        Statement::FunctionDeclaration(Box::new_in(func, self.arena))
    }

    pub fn parse_function_expression(&mut self) -> Expression<'a> {
        let func = self.parse_function(true);

        Expression::FunctionExpression(Box::new_in(func, self.arena))
    }

    pub fn parse_this_expression(&mut self) -> Expression<'a> {
        let start = self.token.start;

        // skip this
        self.bump();

        Expression::ThisExpression(Box::new_in(
            ThisExpression {
                start,
                end: self.prev_token_end,
                parenthesized: self.in_paren,
            },
            self.arena,
        ))
    }

    pub fn parse_class_declaration(&mut self) -> Statement<'a> {
        Statement::ClassDeclaration(Box::new_in(self.parse_class(true), self.arena))
    }

    pub fn parse_class_expression(&mut self) -> Expression<'a> {
        Expression::ClassExpression(Box::new_in(self.parse_class(false), self.arena))
    }

    pub fn parse_class_element(&mut self) -> ClassBody<'a> {
        todo!()
        // let start = self.token.start;

        // if self.kind(Kind::Constructor) {
        //     self.bump();

        //     let params = self.parse_function_parameters();
        //     let body = self.parse_block();

        //     let key = Expression::Identifier(Box::new_in(self.parse_identifier(), self.arena));

        //     return ClassBody::MethodDefinition(Box::new_in(
        //         ClassMethod {
        //             start,
        //             computed: false,
        //             static_: false,
        //             async_: false,
        //             generator: false,
        //             key,
        //             params,
        //             body,
        //             kind: MethodDefinitionKind::Constructor,
        //             end: self.prev_token_end,
        //         },
        //         self.arena,
        //     ));
        // } else {
        //     let (key, is_static, definition_kind, is_computed, is_generator);

        //     let async_identifier;

        //     let ctx  = LexContext::Normal;

        //     is_static = if self.kind(Kind::Static) {
        //         self.bump();
        //         true
        //     } else {
        //         false
        //     };

        //     async_identifier = if self.kind(Kind::Async)  {
        //        Some(self.parse_identifier())
        //     } else {
        //         None
        //     };

        //     is_generator = if self.kind(Kind::Star) {
        //         self.bump();
        //         true
        //     } else {
        //         false
        //     };

        //     if self.kind(Kind::BracketO) {
        //         is_computed = true;
        //         definition_kind = MethodDefinitionKind::Method;

        //         // skip [
        //         self.bump();

        //         key = self.parse_assignment_expression();

        //         self.expect(Kind::BracketC);
        //     } else {
        //         let peeked = self.lexer.peek(ctx).kind;

        //         if matches!(self.token.kind, Kind::Get | Kind::Set) && peeked != Kind::ParenC {
        //             definition_kind = match self.token.kind {
        //                 Kind::Get => MethodDefinitionKind::Get,
        //                 Kind::Set => MethodDefinitionKind::Set,
        //                 _ => unsafe { core::hint::unreachable_unchecked() },
        //             };

        //             if async_identifier.is_some() || is_generator {
        //                 panic!("invalid token kind");
        //             }

        //             self.bump();

        //             if self.kind(Kind::BracketO) {
        //                 is_computed = true;
        //                 self.bump();

        //                 key = self.parse_assignment_expression();

        //                 self.expect(Kind::BracketC);
        //             } else {
        //                 is_computed = false;
        //                 key = Expression::Identifier(Box::new_in(
        //                     self.parse_identifier(),
        //                     self.arena,
        //                 ));
        //             }
        //         } else {
        //             definition_kind = MethodDefinitionKind::Method;
        //             is_computed = false;

        //                 key =
        //                 Expression::Identifier(Box::new_in(self.parse_identifier(), self.arena));
        //         }
        //     };

        //     if self.kind(Kind::ParenO) && !self.token.is_on_new_line {
        //         let params = self.parse_function_parameters();
        //         let body = self.parse_block();

        //         ClassBody::MethodDefinition(Box::new_in(
        //             ClassMethod {
        //                 start,
        //                 computed: is_computed,
        //                 static_: is_static,
        //                 async_: async_identifier.is_some(),
        //                 generator: is_generator,
        //                 key,
        //                 params,
        //                 body,
        //                 kind: definition_kind,
        //                 end: self.prev_token_end,
        //             },
        //             self.arena,
        //         ))
        //     } else {
        //         let value;

        //         if self.kind(Kind::Equal) {
        //             self.bump();
        //              value = Some(self.parse_assignment_expression());
        //         } else {
        //             value = None;
        //         }

        //         ClassBody::PropertyDefinition(Box::new_in(
        //             ClassProperty {
        //                 start,
        //                 computed: is_computed,
        //                 static_: is_static,
        //                 key,
        //                 value,
        //                 end: self.prev_token_end,
        //             },
        //             self.arena,
        //         ))
        //     }
        // }
    }

    pub fn parse_class_body(&mut self) -> Vec<'a, ClassBody<'a>> {
        let mut body = Vec::new_in(self.arena);

        self.expect(Kind::BracesO);

        while !matches!(self.token.kind, Kind::EOF | Kind::BracesC) {
            body.push(self.parse_class_element());
        }

        self.expect(Kind::BracesC);
        body
    }

    pub fn parse_class(&mut self, is_id_optional: bool) -> Class<'a> {
        let start = self.token.start;

        // skip class
        self.bump();

        let id = if self.kind(Kind::Identifier) {
            Some(self.parse_identifier())
        } else {
            None
        };

        if !is_id_optional && id.is_none() {
            panic!("class id is required");
        }

        let super_class = if self.kind(Kind::Extends) {
            self.bump();
            Some(self.parse_lhs_expression())
        } else {
            None
        };

        let body = self.parse_class_body();

        Class {
            start,
            id,
            body,
            super_class,
            end: self.prev_token_end,
        }
    }

    pub fn parse_regexp(&mut self) -> Expression<'a> {
        let start = self.token.start;

        let (pattern_end, flag) = self.lexer.lex_regexp();
        let pattern = unsafe {
            self.code
                .get_unchecked(start as usize..pattern_end as usize)
        };

        let end = self.lexer.index as u32 - 1;

        // update parser state
        self.bump();
        self.prev_token_end = end;

        Expression::RegularExpression(Box::new_in(
            Regexp {
                start,
                pattern,
                flag,
                end,
                parenthesized: self.in_paren,
            },
            self.arena,
        ))
    }

    pub fn parse_template_element(&mut self) -> TemplateElement {
        let start = self.token.start;
        let end;

        match self.token.kind {
            Kind::BackQuote => {
                end = self.token.end - 1;
            }
            Kind::DollarCurly => {
                end = self.token.end - 2;
            }
            _ => unsafe { core::hint::unreachable_unchecked() },
        }

        TemplateElement {
            start,
            tail: false,
            end,
        }
    }

    pub fn parse_template_literal(&mut self) -> Expression<'a> {
        let start = self.token.start;
        let mut expressions = Vec::new_in(self.arena);
        let mut quasis = Vec::new_in(self.arena);

        loop {
            self.bump_with_ctx(LexContext::Template);

            match self.token.kind {
                Kind::BackQuote => {
                    quasis.push(self.parse_template_element());
                    break;
                }

                Kind::DollarCurly => {
                    quasis.push(self.parse_template_element());
                    self.bump();
                    expressions.push(self.parse_expression());

                    if !self.kind(Kind::BracesC) {
                        panic!("unterminated template literal");
                    }
                }

                _ => unsafe { core::hint::unreachable_unchecked() },
            }
        }

        // skip the ending backquote
        self.bump();

        let len = quasis.len();
        quasis[len - 1].tail = true;

        Expression::TemplateLiteral(Box::new_in(
            TemplateLiteral {
                start,
                expressions,
                quasis,
                end: self.prev_token_end,
                parenthesized: self.in_paren,
            },
            self.arena,
        ))
    }

    pub fn parse_new_expression(&mut self) -> Expression<'a> {
        let start = self.token.start;

        // skip new
        self.bump();

        if self.kind(Kind::Dot) {
            // new.target

            todo!()
        } else {
            let callee = self.parse_lhs_expression();
            let arguments = if self.kind(Kind::ParenO) {
                self.parse_function_arguments()
            } else {
                Vec::new_in(self.arena)
            };

            Expression::NewExpression(Box::new_in(
                NewExpression {
                    start,
                    callee,
                    arguments,
                    end: self.prev_token_end,
                    parenthesized: self.in_paren,
                },
                self.arena,
            ))
        }
    }

    pub fn parse_await_expression(&mut self) -> Expression<'a> {
        let start = self.token.start;

        // skip await
        self.bump();

        let argument = self.parse_assignment_expression();

        Expression::AwaitExpression(Box::new_in(
            AwaitExpression {
                start,
                argument,
                end: self.prev_token_end,
                parenthesized: self.in_paren,
            },
            self.arena,
        ))
    }

    pub fn parse_arrow_function_body(&mut self) -> (ArrowFunctionBody<'a>, bool) {
        if self.kind(Kind::BracesO) {
            (
                ArrowFunctionBody::Block(Box::new_in(self.parse_block(), self.arena)),
                false,
            )
        } else {
            (
                ArrowFunctionBody::Expression(Box::new_in(
                    self.parse_assignment_expression(),
                    self.arena,
                )),
                true,
            )
        }
    }

    pub fn parse_function_arguments(&mut self) -> Vec<'a, Expression<'a>> {
        let mut args = Vec::new_in(self.arena);

        // skip (
        self.bump();

        while !matches!(self.token.kind, Kind::EOF | Kind::ParenC) {
            if self.kind(Kind::Dot3) {
                args.push(self.parse_spread_element());
            } else {
                args.push(self.parse_assignment_expression());
            }

            if !self.kind(Kind::ParenC) {
                self.expect(Kind::Comma);
            }
        }

        self.expect(Kind::ParenC);

        args
    }

    pub fn parse_function_parameters(&mut self) -> Vec<'a, FunctionParams<'a>> {
        let mut params = Vec::new_in(self.arena);

        // skip (
        self.bump();

        while !matches!(self.token.kind, Kind::EOF | Kind::ParenC) {
            if self.kind(Kind::Dot3) {
                let rest_el = self.parse_rest_element();
                params.push(FunctionParams::RestElement(Box::new_in(
                    rest_el, self.arena,
                )));

                break;
            } else {
                params.push(FunctionParams::Pattern(Box::new_in(
                    self.parse_pattern(),
                    self.arena,
                )));
            }
            if self.kind(Kind::Comma) {
                self.bump();
            }
        }

        self.expect(Kind::ParenC);

        params
    }

    pub fn parse_function(&mut self, is_id_optional: bool) -> Function<'a> {
        let start = self.token.start;

        let is_async = if self.kind(Kind::Async) {
            self.bump();
            true
        } else {
            false
        };

        self.expect(Kind::Function);

        let is_generator = if self.kind(Kind::Star) {
            self.bump();
            true
        } else {
            false
        };

        let id = if self.kind(Kind::Identifier) {
            Some(self.parse_identifier())
        } else {
            None
        };

        if !is_id_optional && id.is_none() {
            panic!("function id is required");
        }

        let params = self.parse_function_parameters();
        let body = self.parse_block();

        Function {
            start,
            async_: is_async,
            generator: is_generator,
            id,
            params,
            body,
            end: self.prev_token_end,
        }
    }

    pub fn parse_primary_expression(&mut self) -> Expression<'a> {
        match self.token.kind {
            Kind::Identifier => {
                let id = self.parse_identifier();
                Expression::Identifier(Box::new_in(id, self.arena))
            }
            Kind::Async => {
                let peeked = self.lexer.peek(LexContext::Normal).kind;

                if matches!(peeked, Kind::Function) {
                    self.parse_function_expression()
                } else {
                    Expression::Identifier(Box::new_in(self.parse_identifier(), self.arena))
                }
            }
            Kind::Number => self.parse_numeric_literal(),
            Kind::String => self.parse_string_literal_expression(),
            Kind::Boolean => self.parse_boolean_literal(),
            Kind::Null => self.parse_null_literal(),
            Kind::ParenO => self.parse_group_expression(),
            Kind::BracketO => self.parse_array_expression(),
            Kind::BracesO => self.parse_object_expression(),
            Kind::Function => self.parse_function_expression(),
            Kind::This => self.parse_this_expression(),
            Kind::Class => self.parse_class_expression(),
            Kind::Slash | Kind::SlashEqual => self.parse_regexp(),
            Kind::BackQuote => self.parse_template_literal(),
            _ => panic!("unexpected token",),
        }
    }

    pub fn parse_lhs_expression(&mut self) -> Expression<'a> {
        let (maybe_async, prev_line_number) = (self.kind(Kind::Async), self.lexer.line_number);

        let mut exp = match self.token.kind {
            Kind::Super => todo!(),
            Kind::New => self.parse_new_expression(),
            _ => self.parse_primary_expression(),
        };

        loop {
            let is_optional = if self.kind(Kind::QuestionDot) {
                self.bump();
                true
            } else {
                false
            };

            if self.kind(Kind::ParenO) {
                let func_arguments = self.parse_function_arguments();
                let is_async = maybe_async && prev_line_number == self.lexer.line_number;

                if self.kind(Kind::Arrow) {
                    let start = exp.start();
                    self.bump();

                    let mut params = Vec::new_in(self.arena);
                    params.push(FunctionParams::Pattern(Box::new_in(
                        self.reinterpret_as_pattern(exp),
                        self.arena,
                    )));

                    let (body, is_exp) = self.parse_arrow_function_body();

                    exp = Expression::ArrowFunctionExpression(Box::new_in(
                        ArrowFunction {
                            start,
                            async_: is_async,
                            expression: is_exp,
                            params,
                            body,
                            end: self.prev_token_end,
                        },
                        self.arena,
                    ));
                } else {
                    exp = Expression::CallExpression(Box::new_in(
                        CallExpression {
                            start: exp.start(),
                            callee: exp,
                            arguments: func_arguments,
                            optional: is_optional,
                            end: self.prev_token_end,
                        },
                        self.arena,
                    ))
                }
            } else if self.kind(Kind::BracketO) {
                self.bump();
                let property = self.parse_expression();
                self.expect(Kind::BracketC);

                exp = Expression::MemberExpression(Box::new_in(
                    MemberExpression {
                        start: exp.start(),
                        computed: true,
                        optional: is_optional,
                        object: exp,
                        property,
                        end: self.prev_token_end,
                    },
                    self.arena,
                ))
            } else if self.kind(Kind::Dot) || is_optional {
                if self.kind(Kind::Dot) {
                    self.bump();
                }

                let property =
                    Expression::Identifier(Box::new_in(self.parse_identifier(), self.arena));

                exp = Expression::MemberExpression(Box::new_in(
                    MemberExpression {
                        start: exp.start(),
                        computed: false,
                        optional: is_optional,
                        object: exp,
                        property,
                        end: self.prev_token_end,
                    },
                    self.arena,
                ))
            } else if self.kind(Kind::BackQuote) {
                todo!()
            } else {
                break;
            }
        }

        exp
    }

    pub fn parse_update_expression(&mut self) -> Expression<'a> {
        let start = self.token.start;

        if matches!(self.token.kind, Kind::Plus2 | Kind::Minus2) {
            let operator = match self.consume_token() {
                Kind::Plus2 => UpdateOperator::Increment,
                Kind::Minus2 => UpdateOperator::Decrement,
                _ => unsafe { core::hint::unreachable_unchecked() },
            };

            let argument = self.parse_unary_expression();

            Expression::UpdateExpression(Box::new_in(
                UpdateExpression {
                    start,
                    operator,
                    argument,
                    prefix: true,
                    end: self.prev_token_end,
                    parenthesized: self.in_paren,
                },
                self.arena,
            ))
        } else {
            let exp = self.parse_lhs_expression();
            if !self.token.is_on_new_line && matches!(self.token.kind, Kind::Plus2 | Kind::Minus2) {
                let operator = match self.consume_token() {
                    Kind::Plus2 => UpdateOperator::Increment,
                    Kind::Minus2 => UpdateOperator::Decrement,
                    _ => unsafe { core::hint::unreachable_unchecked() },
                };

                Expression::UpdateExpression(Box::new_in(
                    UpdateExpression {
                        start,
                        operator,
                        argument: exp,
                        prefix: false,
                        end: self.prev_token_end,
                        parenthesized: self.in_paren,
                    },
                    self.arena,
                ))
            } else {
                exp
            }
        }
    }

    pub fn parse_unary_expression(&mut self) -> Expression<'a> {
        let start = self.token.start;

        if let Some(operator) = self.get_unary_operator() {
            self.bump();

            Expression::UnaryExpression(Box::new_in(
                UnaryExpression {
                    start,
                    operator,
                    argument: self.parse_unary_expression(),
                    prefix: true,
                    end: self.prev_token_end,
                    parenthesized: self.in_paren,
                },
                self.arena,
            ))
        } else if self.kind(Kind::Await) {
            self.parse_await_expression()
        } else {
            self.parse_update_expression()
        }
    }

    pub fn parse_exponentiation_expression(&mut self) -> Expression<'a> {
        let exp = self.parse_unary_expression();

        match exp {
            Expression::UnaryExpression(_) => exp,
            _ => {
                if self.kind(Kind::Star2) {
                    let right = self.parse_exponentiation_expression();
                    Expression::BinaryExpression(Box::new_in(
                        BinaryExpression {
                            start: exp.start(),
                            left: exp,
                            operator: BinaryOperator::Exponentiation,
                            right,
                            end: self.prev_token_end,
                            parenthesized: self.in_paren,
                        },
                        self.arena,
                    ))
                } else {
                    exp
                }
            }
        }
    }

    pub fn parse_binary_expression(&mut self) -> Expression<'a> {
        let mut exp = self.parse_exponentiation_expression();

        while self.get_binary_precedence() > 0 {
            let operator = self.consume_token();
            let right = self.parse_exponentiation_expression();

            if matches!(operator, Kind::Ampersand2 | Kind::Pipe2) {
                exp = Expression::LogicalExpression(Box::new_in(
                    LogicalExpression {
                        start: exp.start(),
                        left: exp,
                        operator: self.get_logical_operator(operator),
                        right,
                        end: self.prev_token_end,
                        parenthesized: self.in_paren,
                    },
                    self.arena,
                ))
            } else {
                exp = Expression::BinaryExpression(Box::new_in(
                    BinaryExpression {
                        start: exp.start(),
                        left: exp,
                        operator: self.get_binary_operator(operator),
                        right,
                        end: self.prev_token_end,
                        parenthesized: self.in_paren,
                    },
                    self.arena,
                ))
            }
        }

        exp
    }

    pub fn parse_conditional_expression(&mut self) -> Expression<'a> {
        let mut exp = self.parse_binary_expression();

        if self.kind(Kind::Question) {
            self.bump();
            let consequent = self.parse_assignment_expression();
            self.expect(Kind::Colon);
            let alternate = self.parse_assignment_expression();

            exp = Expression::ConditionalExpression(Box::new_in(
                ConditionalExpression {
                    start: 0,
                    test: exp,
                    consequent,
                    alternate,
                    end: self.prev_token_end,
                    parenthesized: self.in_paren,
                },
                self.arena,
            ))
        }

        exp
    }

    pub fn parse_assignment_expression(&mut self) -> Expression<'a> {
        let (prev_kind, prev_line) = (self.token.kind, self.lexer.line_number);
        let mut exp = self.parse_conditional_expression();

        if prev_kind == Kind::Async
            && prev_line == self.lexer.line_number
            && self.kind(Kind::Identifier)
        {
            let id = self.parse_identifier_pattern();
            let mut params = Vec::new_in(self.arena);
            params.push(FunctionParams::Pattern(Box::new_in(id, self.arena)));

            self.expect(Kind::Arrow);

            let (body, expression) = self.parse_arrow_function_body();

            return Expression::ArrowFunctionExpression(Box::new_in(
                ArrowFunction {
                    start: exp.start(),
                    async_: true,
                    params,
                    body,
                    expression,
                    end: self.prev_token_end,
                },
                self.arena,
            ));
        }

        if self.kind(Kind::Arrow) {
            self.bump();

            let start = exp.start();
            let mut params = Vec::new_in(self.arena);
            params.push(FunctionParams::Pattern(Box::new_in(
                self.reinterpret_as_pattern(exp),
                self.arena,
            )));

            let (body, is_exp) = self.parse_arrow_function_body();

            exp = Expression::ArrowFunctionExpression(Box::new_in(
                ArrowFunction {
                    start,
                    async_: false,
                    expression: is_exp,
                    params,
                    body,
                    end: self.prev_token_end,
                },
                self.arena,
            ))
        } else if let Some(operator) = self.get_assignment_operator(self.token.kind) {
            let start = exp.start();
            self.bump();

            let left =
                match exp {
                    Expression::ArrayExpression(_) => AssignmentExpressionLHS::Pattern(
                        Box::new_in(self.reinterpret_as_pattern(exp), self.arena),
                    ),

                    Expression::ObjectExpression(_) => AssignmentExpressionLHS::Pattern(
                        Box::new_in(self.reinterpret_as_pattern(exp), self.arena),
                    ),

                    _ => AssignmentExpressionLHS::Expression(Box::new_in(exp, self.arena)),
                };

            exp = Expression::AssignmentExpression(Box::new_in(
                AssignmentExpression {
                    start,
                    operator,
                    left,
                    right: self.parse_assignment_expression(),
                    end: self.prev_token_end,
                    parenthesized: self.in_paren,
                },
                self.arena,
            ))
        }

        exp
    }

    pub fn parse_variable_declarator(
        &mut self,
        kind: VariableDeclarationKind,
    ) -> VariableDeclarator<'a> {
        let start = self.token.start;

        let id = self.parse_pattern();

        let init = if kind == VariableDeclarationKind::Const {
            self.expect(Kind::Equal);
            Some(self.parse_assignment_expression())
        } else {
            if self.token.kind == Kind::Equal {
                self.bump();
                Some(self.parse_assignment_expression())
            } else {
                None
            }
        };

        VariableDeclarator {
            id,
            init,
            start,
            end: self.prev_token_end,
        }
    }

    pub fn parse_variable_declaration(&mut self) -> Statement<'a> {
        let start = self.token.start;
        let mut declarations = Vec::new_in(self.arena);

        let kind = match &self.token.kind {
            Kind::Const => VariableDeclarationKind::Const,
            Kind::Let => VariableDeclarationKind::Let,
            Kind::Var => VariableDeclarationKind::Var,
            _ => panic!("invalid kind"),
        };
        self.bump();

        loop {
            let declarator = self.parse_variable_declarator(kind);
            declarations.push(declarator);

            if self.kind(Kind::Comma) {
                self.bump();
            } else {
                break;
            }
        }

        Statement::VariableDeclaration(Box::new_in(
            VariableDeclaration {
                kind,
                declarations,
                start,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_import_expression(&mut self) -> Statement<'a> {
        let start = self.token.start;

        // skip import
        self.bump();

        let callee = Expression::Import(Box::new_in(
            Import {
                start,
                end: self.prev_token_end,
            },
            self.arena,
        ));

        let arguments = self.parse_function_arguments();

        let exp = Expression::CallExpression(Box::new_in(
            CallExpression {
                start,
                callee,
                arguments,
                optional: false,
                end: self.prev_token_end,
            },
            self.arena,
        ));

        Statement::ExpressionStatement(Box::new_in(
            ExpressionStatement {
                start,
                exp,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_default_import_specifier(&mut self) -> ImportSpecifierType<'a> {
        let start = self.token.start;

        let local = self.parse_identifier();

        ImportSpecifierType::ImportDefaultSpecifier(Box::new_in(
            ImportDefaultSpecifier {
                start,
                local,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_namespace_import_specifier(&mut self) -> ImportSpecifierType<'a> {
        let start = self.token.start;

        // skip *
        self.bump();

        self.expect(Kind::As);

        if !self.kind(Kind::Identifier) {
            panic!("invalid token")
        }

        let local = self.parse_identifier();

        ImportSpecifierType::ImportNamespaceSpecifier(Box::new_in(
            ImportNamespaceSpecifier {
                start,
                local,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_named_import_specifier(&mut self) -> ImportSpecifierType<'a> {
        let start = self.token.start;

        let imported;

        if matches!(
            self.token.kind,
            Kind::Identifier | Kind::Async | Kind::Assert | Kind::As
        ) {
            imported =
                IdentifierOrLiteral::Identifier(Box::new_in(self.parse_identifier(), self.arena));
        } else if self.kind(Kind::String) {
            imported =
                IdentifierOrLiteral::Literal(Box::new_in(self.parse_string_literal(), self.arena));
        } else {
            panic!("invalid token")
        }

        let local = if self.kind(Kind::As) {
            if !matches!(self.token.kind, Kind::Identifier | Kind::Async) {
                panic!("invalid token");
            }

            Some(self.parse_identifier())
        } else {
            None
        };

        ImportSpecifierType::ImportSpecifier(Box::new_in(
            ImportSpecifier {
                start,
                imported,
                local,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_import_assertions(&mut self) -> Vec<'a, ImportAttribute<'a>> {
        let mut assertions = Vec::new_in(self.arena);

        if self.kind(Kind::Assert) {
            todo!()
        }

        assertions
    }

    pub fn parse_import_declaration(&mut self) -> Statement<'a> {
        let start = self.token.start;

        if self.kind(Kind::ParenO) {
            return self.parse_import_expression();
        }

        // skip import
        self.bump();

        let mut specifiers = Vec::new_in(self.arena);

        if !self.kind(Kind::String) {
            if matches!(self.token.kind, Kind::Identifier | Kind::Async) {
                specifiers.push(self.parse_default_import_specifier());
            }

            if self.kind(Kind::Comma) {
                self.bump();
            }

            if self.kind(Kind::Star) {
                specifiers.push(self.parse_namespace_import_specifier());
            } else if self.kind(Kind::BracesO) {
                self.bump();

                while !matches!(self.token.kind, Kind::EOF | Kind::BracesC) {
                    specifiers.push(self.parse_named_import_specifier());

                    if !self.kind(Kind::BracesC) {
                        self.expect(Kind::Comma);
                    }
                }

                self.expect(Kind::BracesC);
            }

            self.expect(Kind::From);
        }

        if !self.kind(Kind::String) {
            panic!("invalid token");
        }

        let source = self.parse_string_literal();
        let assertions = self.parse_import_assertions();

        Statement::ImportDeclaration(Box::new_in(
            ImportDeclaration {
                start,
                specifiers,
                source,
                assertions,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_block(&mut self) -> Block<'a> {
        let start = self.token.start;
        let mut body = Vec::new_in(self.arena);

        // skip {
        self.bump();

        while !matches!(self.token.kind, Kind::EOF | Kind::BracesC) {
            body.push(self.parse_statement());
        }

        self.expect(Kind::BracesC);

        Block {
            start,
            body,
            end: self.prev_token_end,
        }
    }

    pub fn parse_block_statement(&mut self) -> Statement<'a> {
        let block = self.parse_block();

        Statement::BlockStatement(Box::new_in(block, self.arena))
    }

    pub fn parse_empty_statement(&mut self) -> Statement<'a> {
        let start = self.token.start;

        // skip ;
        self.bump();

        Statement::EmptyStatement(Box::new_in(
            EmptyStatement {
                start,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_break_statement(&mut self) -> Statement<'a> {
        let start = self.token.start;

        // skip break
        self.bump();

        let label = if self.kind(Kind::Identifier) {
            let id = self.parse_identifier();
            Some(id)
        } else {
            None
        };

        Statement::BreakStatement(Box::new_in(
            BreakStatement {
                start,
                label,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_continue_statement(&mut self) -> Statement<'a> {
        let start = self.token.start;

        // skip continue
        self.bump();

        let label = if self.kind(Kind::Identifier) {
            let id = self.parse_identifier();
            Some(id)
        } else {
            None
        };

        Statement::ContinueStatement(Box::new_in(
            ContinueStatement {
                start,
                label,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_debugger_statement(&mut self) -> Statement<'a> {
        let start = self.token.start;

        // skip debugger
        self.bump();

        Statement::DebuggerStatement(Box::new_in(
            DebuggerStatement {
                start,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_do_while_loop(&mut self) -> Statement<'a> {
        let start = self.token.start;

        // skip do
        self.bump();
        let body = self.parse_statement();

        self.expect(Kind::While);
        self.expect(Kind::ParenO);

        let test = self.parse_expression();

        self.expect(Kind::ParenC);

        Statement::DoWhileLoop(Box::new_in(
            DoWhileLoop {
                start,
                test,
                body,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_while_loop(&mut self) -> Statement<'a> {
        let start = self.token.start;

        // skip while
        self.bump();

        self.expect(Kind::ParenO);
        let test = self.parse_expression();
        self.expect(Kind::ParenC);

        let body = self.parse_statement();

        Statement::WhileLoop(Box::new_in(
            WhileLoop {
                start,
                test,
                body,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_for_loop(&mut self) -> Statement<'a> {
        let start = self.token.start;

        // skip for
        self.bump();

        self.expect(Kind::ParenO);

        let init;

        if self.kind(Kind::Semicolon) {
            init = None;
        } else if matches!(self.token.kind, Kind::Const | Kind::Let | Kind::Var) {
            let declaration = match self.parse_variable_declaration() {
                Statement::VariableDeclaration(v_declaration) => v_declaration,
                _ => unsafe { core::hint::unreachable_unchecked() },
            };

            init = Some(ForLoopInit::VariableDeclaration(declaration));
        } else {
            init = Some(ForLoopInit::Expression(Box::new_in(
                self.parse_expression(),
                self.arena,
            )));
        }

        if init.is_none() || self.kind(Kind::Semicolon) {
            self.expect(Kind::Semicolon);

            let test = if self.kind(Kind::Semicolon) {
                self.bump();
                None
            } else {
                let temp = Some(self.parse_expression());
                self.expect(Kind::Semicolon);
                temp
            };

            let update = if self.kind(Kind::ParenC) {
                None
            } else {
                Some(self.parse_expression())
            };

            self.expect(Kind::ParenC);

            let body = self.parse_statement();

            Statement::ForLoop(Box::new_in(
                ForLoop {
                    start,
                    init,
                    test,
                    update,
                    body,
                    end: self.prev_token_end,
                },
                self.arena,
            ))
        } else if self.kind(Kind::In) {
            self.bump();

            let left = match unsafe { init.unwrap_unchecked() } {
                ForLoopInit::VariableDeclaration(v_declaration) => {
                    ForInLoopLeft::VariableDeclaration(v_declaration)
                }
                ForLoopInit::Expression(boxed_exp) => {
                    let exp = boxed_exp.as_ref().clone_in(self.arena);
                    ForInLoopLeft::Pattern(Box::new_in(
                        self.reinterpret_as_pattern(exp),
                        self.arena,
                    ))
                }
            };

            let right = self.parse_expression();

            self.expect(Kind::ParenC);

            let body = self.parse_statement();

            Statement::ForInLoop(Box::new_in(
                ForInLoop {
                    start,
                    left,
                    right,
                    body,
                    end: self.prev_token_end,
                },
                self.arena,
            ))
        } else {
            panic!("invalid for loop")
        }
    }

    pub fn parse_if_statement(&mut self) -> Statement<'a> {
        let start = self.token.start;

        // skip if
        self.bump();

        self.expect(Kind::ParenO);
        let test = self.parse_expression();
        self.expect(Kind::ParenC);

        let consequent = self.parse_statement();

        let alternate = if self.kind(Kind::Else) {
            self.bump();
            Some(self.parse_statement())
        } else {
            None
        };

        Statement::IfStatement(Box::new_in(
            IfStatement {
                start,
                test,
                consequent,
                alternate,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_return_statement(&mut self) -> Statement<'a> {
        let start = self.token.start;

        // skip return
        self.bump();

        let argument;

        if matches!(self.token.kind, Kind::EOF | Kind::Semicolon) || self.token.is_on_new_line {
            argument = None;
        } else {
            argument = Some(self.parse_expression());
        }

        Statement::ReturnStatement(Box::new_in(
            ReturnStatement {
                start,
                argument,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_switch_case(&mut self) -> SwitchCase<'a> {
        let start = self.token.start;

        let test;
        let mut consequent = Vec::new_in(self.arena);

        if self.kind(Kind::Default) {
            self.bump();
            test = None;
        } else {
            self.expect(Kind::Case);
            test = Some(self.parse_expression());
        }

        self.expect(Kind::Colon);

        while !matches!(
            self.token.kind,
            Kind::EOF | Kind::BracesC | Kind::Case | Kind::Default
        ) {
            consequent.push(self.parse_statement());
        }

        SwitchCase {
            start,
            test,
            consequent,
            end: self.prev_token_end,
        }
    }

    pub fn parse_switch_statement(&mut self) -> Statement<'a> {
        let start = self.token.start;
        let mut cases = Vec::new_in(self.arena);

        // skip switch
        self.bump();

        self.expect(Kind::ParenO);
        let discriminant = self.parse_expression();
        self.expect(Kind::ParenC);

        self.expect(Kind::BracesO);

        while !matches!(self.token.kind, Kind::EOF | Kind::BracesC) {
            cases.push(self.parse_switch_case());
        }

        self.expect(Kind::BracesC);

        Statement::SwitchStatement(Box::new_in(
            SwitchStatement {
                start,
                discriminant,
                cases,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_throw_statement(&mut self) -> Statement<'a> {
        let start = self.token.start;

        // skip throw
        self.bump();

        let argument = self.parse_expression();

        Statement::ThrowStatement(Box::new_in(
            ThrowStatement {
                start,
                argument,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_catch_clause(&mut self) -> CatchClause<'a> {
        let start = self.token.start;

        // skip catch
        self.bump();

        self.expect(Kind::ParenO);
        let param = self.parse_pattern();
        self.expect(Kind::ParenC);

        if !self.kind(Kind::BracesO) {
            panic!("catch clause must have a block");
        }

        let body = self.parse_block();

        CatchClause {
            start,
            param,
            body,
            end: self.prev_token_end,
        }
    }

    pub fn parse_try_statement(&mut self) -> Statement<'a> {
        let start = self.token.start;

        // skip try
        self.bump();

        if !self.kind(Kind::BracesO) {
            panic!("try statement must have a block");
        }

        let block = self.parse_block();

        let handler = if self.kind(Kind::Catch) {
            Some(self.parse_catch_clause())
        } else {
            None
        };

        let finalizer = if self.kind(Kind::Finally) {
            self.bump();

            if !self.kind(Kind::BracesO) {
                panic!("finally clause must have a block");
            }

            Some(self.parse_block())
        } else {
            None
        };

        Statement::TryStatement(Box::new_in(
            TryStatement {
                start,
                block,
                handler,
                finalizer,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_with_statement(&mut self) -> Statement<'a> {
        let start = self.token.start;

        // skip with
        self.bump();

        self.expect(Kind::ParenO);
        let object = self.parse_expression();
        self.expect(Kind::ParenC);

        let body = self.parse_statement();

        Statement::WithStatement(Box::new_in(
            WithStatement {
                start,
                object,
                body,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_labelled_statement(&mut self) -> Statement<'a> {
        let start = self.token.start;

        let exp = self.parse_expression();

        match exp {
            Expression::Identifier(boxed_identifier) if self.kind(Kind::Colon) => {
                self.bump();

                let label = boxed_identifier.as_ref().clone_in(self.arena);

                Statement::LabelledStatement(Box::new_in(
                    LabelledStatement {
                        start,
                        label,
                        body: self.parse_statement(),
                        end: self.prev_token_end,
                    },
                    self.arena,
                ))
            }
            _ => Statement::ExpressionStatement(Box::new_in(
                ExpressionStatement {
                    start,
                    exp,
                    end: self.prev_token_end,
                },
                self.arena,
            )),
        }
    }

    pub fn parse_expression(&mut self) -> Expression<'a> {
        let exp = self.parse_assignment_expression();

        if self.kind(Kind::Comma) {
            let mut expressions = Vec::new_in(self.arena);
            let start = exp.start();

            expressions.push(exp);

            while !matches!(self.token.kind, Kind::EOF) {
                if !self.kind(Kind::Comma) {
                    break;
                }

                // skip ,
                self.bump();

                expressions.push(self.parse_assignment_expression());
            }

            return Expression::SequenceExpression(Box::new_in(
                SequenceExpression {
                    start,
                    expressions,
                    end: self.prev_token_end,
                    parenthesized: true,
                },
                self.arena,
            ));
        }

        exp
    }

    pub fn parse_expression_statement(&mut self) -> Statement<'a> {
        let exp = self.parse_expression();

        Statement::ExpressionStatement(Box::new_in(
            ExpressionStatement {
                start: exp.start(),
                exp,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_statement(&mut self) -> Statement<'a> {
        match self.token.kind {
            Kind::Const | Kind::Let | Kind::Var => self.parse_variable_declaration(),
            Kind::Import => self.parse_import_declaration(),
            Kind::Export => todo!(),
            Kind::Function => self.parse_function_declaration(),
            Kind::Class => self.parse_class_declaration(),
            Kind::BracesO => self.parse_block_statement(),
            Kind::Semicolon => self.parse_empty_statement(),
            Kind::Break => self.parse_break_statement(),
            Kind::Continue => self.parse_continue_statement(),
            Kind::Debugger => self.parse_debugger_statement(),
            Kind::Do => self.parse_do_while_loop(),
            Kind::While => self.parse_while_loop(),
            Kind::For => self.parse_for_loop(),
            Kind::If => self.parse_if_statement(),
            Kind::Return => self.parse_return_statement(),
            Kind::Switch => self.parse_switch_statement(),
            Kind::Throw => self.parse_throw_statement(),
            Kind::Try => self.parse_try_statement(),
            Kind::With => self.parse_with_statement(),
            Kind::Identifier => self.parse_labelled_statement(),
            Kind::Async => {
                let peeked = self.lexer.peek(LexContext::Normal).kind;

                if matches!(peeked, Kind::Function) {
                    self.parse_function_declaration()
                } else {
                    self.parse_labelled_statement()
                }
            }
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse(&mut self) -> Vec<'a, Statement<'a>> {
        let mut ast = Vec::new_in(self.arena);

        loop {
            if self.kind(Kind::EOF) {
                break;
            }

            ast.push(self.parse_statement());
        }

        ast
    }

    pub fn new(arena: &'a Bump, code: &'a str) -> Self {
        let mut lexer = Tokenizer::new(code);
        let token = lexer.lex(LexContext::Normal);

        Parser {
            code,
            lexer,
            token,
            prev_token_end: 0,
            in_paren: false,
            arena,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{fs::read_to_string, time::Instant};

    #[test]
    fn test_parser() {
        let code = read_to_string("../../data/input.js").unwrap();
        let arena = Bump::new();

        let mut p = Parser::new(&arena, code.as_str());
        let ast = p.parse();

        for s in ast.iter() {
            println!("{:#?}", s);
        }
    }

    #[test]
    fn parser_performance() {
        let code = read_to_string("../../data/input.js").unwrap();
        let arena = Bump::new();

        let mut p = Parser::new(&arena, code.as_str());

        let start = Instant::now();

        p.parse();

        let end = start.elapsed();
        println!("{}", end.as_millis());
    }
}
