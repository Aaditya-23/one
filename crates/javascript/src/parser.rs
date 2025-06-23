use std::ops::Range;

use bumpalo::{boxed::Box, collections::Vec as ArenaVec, Bump};

use crate::{
    allocator::CloneIn,
    ast::{
        javascript::{
            ArrayElement, ArrayExpression, ArrayPattern, ArrayPatternKind, ArrowFunction,
            ArrowFunctionBody, AssignmentExpression, AssignmentExpressionLHS, AssignmentOperator,
            AssignmentPattern, AwaitExpression, BinaryExpression, BinaryOperator, Block,
            BooleanLiteral, BreakStatement, CallExpression, CatchClause, Class, ClassBody,
            ClassMethod, ClassProperty, ConditionalExpression, ContinueStatement,
            DebuggerStatement, Declaration, DoWhileLoop, Elision, EmptyStatement,
            ExportAllDeclaration, ExportDefaultDeclaration, ExportNamedDeclaration,
            ExportSpecifier, Expression, ExpressionStatement, ForInLoop, ForInLoopLeft, ForLoop,
            ForLoopInit, Function, FunctionArgument, FunctionParam, Identifier,
            IdentifierOrLiteral, IfStatement, Import, ImportAttribute, ImportDeclaration,
            ImportDefaultSpecifier, ImportNamespaceSpecifier, ImportSpecifier, ImportSpecifierType,
            LabelledStatement, Location, MemberExpression, MetaProperty, MethodDefinitionKind,
            NewExpression, NullLiteral, NumericLiteral, ObjectExpression, ObjectExpressionMethod,
            ObjectExpressionMethodKind, ObjectExpressionProperty, ObjectExpressionPropertyKind,
            ObjectPattern, ObjectPatternProperty, ObjectPatternPropertyKind, Pattern, Regexp,
            RestElement, ReturnStatement, SequenceExpression, SpreadElement, Statement,
            StringLiteral, SwitchCase, SwitchStatement, TaggedTemplateLiteral, TemplateElement,
            TemplateLiteral, ThisExpression, ThrowStatement, TryStatement, UnaryExpression,
            UnaryOperator, UpdateExpression, UpdateOperator, VariableDeclaration,
            VariableDeclarationKind, VariableDeclarator, WhileLoop, WithStatement,
        },
        typescript::TsInstantiationExpression,
    },
    kind::Kind,
    tokenizer::{LexContext, LexerState, Token, Tokenizer},
    DiagnosticError, ParserDiagnostics,
};

pub type ParseResult<T> = core::result::Result<T, ()>;

pub struct Program<'a> {
    pub ast: ArenaVec<'a, Statement<'a>>,
    pub diagnostics: Vec<ParserDiagnostics<'a>>,
    pub fatal_error: bool,
    pub comments: Vec<()>,
}

pub struct ParserContext {
    pub in_method_type_declaration: bool,
}

impl Default for ParserContext {
    fn default() -> Self {
        Self {
            in_method_type_declaration: false,
        }
    }
}

pub struct Extensions {
    pub ts: bool,
    pub jsx: bool,
}

pub struct ParserState {
    pub lexer: LexerState,
    pub token: Token,
    pub prev_token_end: u32,
    pub in_paren: bool,
}

pub struct Parser<'a> {
    code: &'a str,
    pub(crate) lexer: Tokenizer<'a>,
    pub(crate) ctx: ParserContext,
    pub(crate) token: Token,
    pub(crate) prev_token_end: u32,
    pub(crate) in_paren: bool,
    pub(crate) arena: &'a Bump,
    extensions: Extensions,
}

impl<'a> Parser<'a> {
    pub fn capture_state(&self) -> ParserState {
        ParserState {
            lexer: self.lexer.capture_state(),
            token: self.token.clone(),
            prev_token_end: self.prev_token_end,
            in_paren: self.in_paren,
        }
    }

    pub fn restore_from_state(&mut self, state: ParserState) {
        self.lexer.restore_from_state(state.lexer);
        self.token = state.token;
        self.prev_token_end = state.prev_token_end;
        self.in_paren = state.in_paren;
    }

    pub fn bump(&mut self) {
        self.prev_token_end = self.token.end;
        self.token = self.lexer.lex(LexContext::Normal);
    }

    pub fn bump_with_ctx(&mut self, ctx: LexContext) {
        self.prev_token_end = self.token.end;
        self.token = self.lexer.lex(ctx);
    }

    pub fn bump_token(&mut self, kind: Kind) -> Result<(), ()> {
        if self.token.kind != kind {
            self.lexer.diagnostics.push(ParserDiagnostics {
                error: DiagnosticError::UnexpectedToken {
                    expected: kind,
                    found: self.token.kind,
                },
                position: self.token.start as usize..self.token.end as usize,
            });

            return Err(());
        }

        self.bump();
        Ok(())
    }

    pub fn assert(&mut self, condition: bool, message: &'a str) -> Result<(), ()> {
        if !condition {
            self.lexer.diagnostics.push(ParserDiagnostics {
                error: DiagnosticError::Other(message),
                position: self.token.start as usize..self.token.end as usize,
            });

            return Err(());
        }

        Ok(())
    }

    pub fn assert_token(&mut self, kind: Kind) -> Result<(), ()> {
        if !self.kind(kind) {
            self.lexer.diagnostics.push(ParserDiagnostics {
                error: DiagnosticError::UnexpectedToken {
                    expected: kind,
                    found: self.token.kind,
                },
                position: self.token.start as usize..self.token.end as usize,
            });

            return Err(());
        }

        Ok(())
    }

    pub fn kind(&self, kind: Kind) -> bool {
        self.token.kind == kind
    }

    pub fn add_diagnostic(&mut self, message: &'a str, position: Range<u32>) {
        self.lexer.diagnostics.push(ParserDiagnostics {
            error: DiagnosticError::Other(message),
            position: position.start as usize..position.end as usize,
        });
    }

    pub fn report_unexpected_token(&mut self, expected: Kind) {
        self.lexer.diagnostics.push(ParserDiagnostics {
            error: DiagnosticError::UnexpectedToken {
                expected,
                found: self.token.kind,
            },
            position: self.token.start as usize..self.token.end as usize,
        });
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

    pub fn try_parse_or_rewind<T>(
        &mut self,
        func: impl FnOnce(&mut Self) -> Option<T>,
    ) -> Option<T> {
        let state = self.capture_state();

        let result = func(self);

        if result.is_none() {
            self.restore_from_state(state);
            None
        } else {
            result
        }
    }

    pub fn is_curr_token_identifier(&self) -> bool {
        is_identifier(self.token.kind)
    }

    pub fn is_curr_token_a_reserved_keyword(&self) -> bool {
        is_reserved_keyword(self.token.kind)
    }

    pub fn is_start_of_exp(&self) -> bool {
        // todo: add yield and #
        match self.token.kind {
            Kind::Bang
            | Kind::ParenO
            | Kind::BracketO
            | Kind::BracesO
            | Kind::Plus2
            | Kind::Minus2
            | Kind::Tilde
            | Kind::Plus
            | Kind::Minus
            | Kind::Throw
            | Kind::New
            | Kind::Typeof
            | Kind::Void
            | Kind::Delete
            | Kind::Dot3
            | Kind::This
            | Kind::Function
            | Kind::Class
            | Kind::Import
            | Kind::Super
            | Kind::LessThan
            | Kind::Slash
            | Kind::SlashEqual
            | Kind::BackQuote
            | Kind::Boolean
            | Kind::Number
            | Kind::String
            | Kind::Null => true,
            kind if is_identifier(kind) => true,
            _ => false,
        }
    }

    pub fn parse_identifier(&mut self) -> Identifier<'a> {
        let start = self.token.start;
        self.bump();

        Identifier {
            start,
            parenthesized: self.in_paren,
            type_annotation: None,
            end: self.prev_token_end,
        }
    }

    pub fn parse_pattern_with_default_value(&mut self) -> ParseResult<Pattern<'a>> {
        let start = self.token.start;
        let pattern = self.parse_pattern()?;

        if self.kind(Kind::Equal) {
            self.assert(self.ctx.in_method_type_declaration, "invalid token kind")?;

            self.bump();
            let right = self.parse_assignment_expression()?;

            return Ok(Pattern::AssignmentPattern(Box::new_in(
                AssignmentPattern {
                    start,
                    left: pattern,
                    right,
                    end: self.prev_token_end,
                },
                self.arena,
            )));
        }

        Ok(pattern)
    }

    pub fn parse_rest_element(&mut self) -> ParseResult<RestElement<'a>> {
        let start = self.token.start;

        // skip ...
        self.bump();

        self.assert_token(Kind::Identifier)?;

        let argument = self.parse_identifier_pattern()?;

        Ok(RestElement {
            start,
            argument,
            end: self.prev_token_end,
        })
    }

    pub fn parse_identifier_pattern(&mut self) -> ParseResult<Pattern<'a>> {
        let mut identifier = self.parse_identifier();

        if self.extensions.ts && self.kind(Kind::Colon) {
            self.bump();
            identifier.type_annotation = Some(self.parse_type_annotation()?);
        }

        Ok(Pattern::Identifier(Box::new_in(identifier, self.arena)))
    }

    pub fn parse_array_pattern(&mut self) -> ParseResult<Pattern<'a>> {
        let start = self.token.start;

        // skip [
        self.bump();

        let mut elements = ArenaVec::new_in(self.arena);

        while !matches!(self.token.kind, Kind::EOF | Kind::BracketC) {
            if self.kind(Kind::Comma) {
                elements.push(None);
            } else if self.kind(Kind::Dot3) {
                let array_pattern = ArrayPatternKind::RestElement(Box::new_in(
                    self.parse_rest_element()?,
                    self.arena,
                ));

                elements.push(Some(array_pattern));
            } else {
                let array_pattern = ArrayPatternKind::Pattern(Box::new_in(
                    self.parse_pattern_with_default_value()?,
                    self.arena,
                ));

                elements.push(Some(array_pattern));
            }

            if !self.kind(Kind::BracketC) {
                self.bump_token(Kind::Comma)?;
            }
        }

        self.bump_token(Kind::BracketC)?;

        let type_annotation = if self.extensions.ts && self.kind(Kind::Colon) {
            self.bump();
            Some(self.parse_type_annotation()?)
        } else {
            None
        };

        Ok(Pattern::ArrayPattern(Box::new_in(
            ArrayPattern {
                start,
                elements,
                type_annotation,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_object_pattern_property(&mut self) -> ParseResult<ObjectPatternProperty<'a>> {
        let start = self.token.start;
        let (key, value, shorthand, computed);

        if self.kind(Kind::BracketO) {
            self.bump();
            key = self.parse_assignment_expression()?;

            self.bump_token(Kind::BracketC)?;
            self.bump_token(Kind::Colon)?;

            value = self.parse_pattern_with_default_value()?;

            computed = true;
            shorthand = false;
        } else {
            self.assert_token(Kind::Identifier)?;

            let id = self.parse_identifier();
            key = Expression::Identifier(Box::new_in(id.clone_in(self.arena), self.arena));

            if self.kind(Kind::Equal) {
                self.bump();
                let right = self.parse_assignment_expression()?;

                value = Pattern::AssignmentPattern(Box::new_in(
                    AssignmentPattern {
                        start,
                        left: Pattern::Identifier(Box::new_in(id, self.arena)),
                        right,
                        end: self.prev_token_end,
                    },
                    self.arena,
                ));

                shorthand = true;
            } else if self.kind(Kind::Colon) {
                self.bump();
                value = self.parse_pattern_with_default_value()?;
                shorthand = false;
            } else {
                value = Pattern::Identifier(Box::new_in(id, self.arena));
                shorthand = true;
            }

            computed = false;
        }

        Ok(ObjectPatternProperty {
            start,
            key,
            value,
            shorthand,
            computed,
            end: self.prev_token_end,
        })
    }

    pub fn parse_object_pattern(&mut self) -> ParseResult<Pattern<'a>> {
        let start = self.token.start;
        let mut properties = ArenaVec::new_in(self.arena);

        // skip {
        self.bump();

        while !matches!(self.token.kind, Kind::EOF | Kind::BracesC) {
            if self.token.kind == Kind::Dot3 {
                let rest_element = Box::new_in(self.parse_rest_element()?, self.arena);
                properties.push(ObjectPatternPropertyKind::RestElement(rest_element));
            } else {
                let property = Box::new_in(self.parse_object_pattern_property()?, self.arena);

                properties.push(ObjectPatternPropertyKind::Property(property))
            }

            if !self.kind(Kind::BracesC) {
                self.bump_token(Kind::Comma)?;
            }
        }

        self.bump_token(Kind::BracesC)?;

        let type_annotation = if self.extensions.ts && self.kind(Kind::Colon) {
            self.bump();
            Some(self.parse_type_annotation()?)
        } else {
            None
        };

        Ok(Pattern::ObjectPattern(Box::new_in(
            ObjectPattern {
                start,
                properties,
                type_annotation,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_pattern(&mut self) -> ParseResult<Pattern<'a>> {
        let start = self.token.start;

        match self.token.kind {
            Kind::Identifier => self.parse_identifier_pattern(),
            Kind::BracketO => self.parse_array_pattern(),
            Kind::BracesO => self.parse_object_pattern(),
            _ => {
                self.add_diagnostic("invalid pattern", start..start + 1);
                Err(())
            }
        }
    }

    pub fn reinterpret_as_pattern(&mut self, exp: Expression<'a>) -> ParseResult<Pattern<'a>> {
        match exp {
            Expression::Identifier(id) => Ok(Pattern::Identifier(id)),
            Expression::ArrayExpression(array) => {
                let array = Box::into_inner(array);

                let mut elements = ArenaVec::new_in(self.arena);

                let (start, end) = (array.start, array.end);

                for el in array.elements.into_iter() {
                    match el {
                        ArrayElement::Elision(_) => elements.push(None),
                        ArrayElement::SpreadElement(inner) => {
                            let spread_el = Box::into_inner(inner);

                            let argument = self.reinterpret_as_pattern(spread_el.argument)?;

                            let array_pattern = ArrayPatternKind::RestElement(Box::new_in(
                                RestElement {
                                    start: spread_el.start,
                                    argument,
                                    end: spread_el.end,
                                },
                                self.arena,
                            ));

                            elements.push(Some(array_pattern))
                        }
                        ArrayElement::Expression(exp) => {
                            let exp = Box::into_inner(exp);

                            elements.push(Some(ArrayPatternKind::Pattern(Box::new_in(
                                self.reinterpret_as_pattern(exp)?,
                                self.arena,
                            ))))
                        }
                    }
                }

                Ok(Pattern::ArrayPattern(Box::new_in(
                    ArrayPattern {
                        start,
                        elements,
                        type_annotation: None,
                        end,
                    },
                    self.arena,
                )))
            }
            Expression::ObjectExpression(inner) => {
                let object = Box::into_inner(inner);

                let mut properties = ArenaVec::new_in(self.arena);

                for property in object.properties.into_iter() {
                    let object_pattern_property = match property {
                        ObjectExpressionPropertyKind::SpreadElement(inner) => {
                            let spread_el = Box::into_inner(inner);

                            ObjectPatternPropertyKind::RestElement(Box::new_in(
                                RestElement {
                                    start: spread_el.start,
                                    argument: self.reinterpret_as_pattern(spread_el.argument)?,
                                    end: spread_el.end,
                                },
                                self.arena,
                            ))
                        }
                        ObjectExpressionPropertyKind::Method(inner) => {
                            self.add_diagnostic("invalid token", inner.start..inner.end);

                            return Err(());
                        }
                        ObjectExpressionPropertyKind::Property(inner) => {
                            let property = Box::into_inner(inner);

                            ObjectPatternPropertyKind::Property(Box::new_in(
                                ObjectPatternProperty {
                                    start: property.start,
                                    computed: property.computed,
                                    shorthand: property.shorthand,
                                    key: property.key,
                                    value: self.reinterpret_as_pattern(property.value)?,
                                    end: property.end,
                                },
                                self.arena,
                            ))
                        }
                    };

                    properties.push(object_pattern_property);
                }

                Ok(Pattern::ObjectPattern(Box::new_in(
                    ObjectPattern {
                        start: object.start,
                        properties,
                        type_annotation: None,
                        end: object.end,
                    },
                    self.arena,
                )))
            }
            Expression::AssignmentExpression(exp) => {
                let exp = Box::into_inner(exp);
                let start = exp.start;
                let left = match exp.left {
                    AssignmentExpressionLHS::Pattern(pattern) => Box::into_inner(pattern),
                    AssignmentExpressionLHS::Expression(exp) => {
                        let exp = Box::into_inner(exp);
                        self.reinterpret_as_pattern(exp)?
                    }
                };

                Ok(Pattern::AssignmentPattern(Box::new_in(
                    AssignmentPattern {
                        start,
                        left,
                        right: exp.right,
                        end: exp.end,
                    },
                    self.arena,
                )))
            }
            _ => {
                self.add_diagnostic("invalid expression", exp.start()..self.token.end);
                return Err(());
            }
        }
    }

    pub fn parse_numeric_literal(&mut self) -> NumericLiteral {
        let start = self.token.start;

        // skip numeric literal
        self.bump();

        NumericLiteral {
            start,
            end: self.prev_token_end,
        }
    }

    pub fn parse_numeric_literal_expression(&mut self) -> Expression<'a> {
        let literal = self.parse_numeric_literal();
        Expression::NumericLiteral(Box::new_in(literal, self.arena))
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

    pub fn parse_boolean_literal(&mut self) -> BooleanLiteral {
        let start = self.token.start;
        self.bump();

        BooleanLiteral {
            start,
            end: self.prev_token_end,
        }
    }
    pub fn parse_boolean_literal_expression(&mut self) -> Expression<'a> {
        let literal = self.parse_boolean_literal();
        Expression::BooleanLiteral(Box::new_in(literal, self.arena))
    }

    pub fn parse_null_literal(&mut self) -> NullLiteral {
        let start = self.token.start;
        self.bump();

        NullLiteral {
            start,
            end: self.prev_token_end,
        }
    }

    pub fn parse_null_literal_expression(&mut self) -> Expression<'a> {
        let literal = self.parse_null_literal();
        Expression::NullLiteral(Box::new_in(literal, self.arena))
    }

    pub fn parse_group_expression(&mut self) -> ParseResult<Expression<'a>> {
        let start = self.token.start;

        // skip (
        self.bump();

        if self.kind(Kind::ParenC) {
            self.add_diagnostic(
                "parenthesized expression cannot be empty",
                start..self.token.end,
            );

            Err(())
        } else {
            self.in_paren = true;
            let exp = self.parse_assignment_expression()?;

            if self.kind(Kind::Comma) {
                let mut expressions = ArenaVec::new_in(self.arena);

                while !matches!(self.token.kind, Kind::EOF | Kind::ParenC) {
                    self.bump_token(Kind::Comma)?;
                    expressions.push(self.parse_assignment_expression()?);
                }

                self.bump_token(Kind::ParenC)?;
                self.in_paren = false;

                Ok(Expression::SequenceExpression(Box::new_in(
                    SequenceExpression {
                        start,
                        expressions,
                        end: self.prev_token_end,
                        parenthesized: true,
                    },
                    self.arena,
                )))
            } else {
                self.bump_token(Kind::ParenC)?;
                self.in_paren = false;

                Ok(exp)
            }
        }
    }

    pub fn parse_spread_element(&mut self) -> ParseResult<SpreadElement<'a>> {
        let start = self.token.start;

        // skip ...
        self.bump();

        let argument = self.parse_assignment_expression()?;

        Ok(SpreadElement {
            start,
            argument,
            end: self.prev_token_end,
        })
    }

    pub fn parse_elision(&mut self) -> Elision {
        let start = self.token.start;
        self.bump();

        Elision {
            start,
            end: self.prev_token_end,
        }
    }

    pub fn parse_array_expression(&mut self) -> ParseResult<Expression<'a>> {
        let start = self.token.start;

        // skip [
        self.bump();

        let mut elements = ArenaVec::new_in(self.arena);

        while !matches!(self.token.kind, Kind::EOF | Kind::BracketC) {
            let el;

            if self.kind(Kind::Comma) {
                el = ArrayElement::Elision(Box::new_in(self.parse_elision(), self.arena));
            } else {
                if self.kind(Kind::Dot3) {
                    el = ArrayElement::SpreadElement(Box::new_in(
                        self.parse_spread_element()?,
                        self.arena,
                    ));
                } else {
                    el = ArrayElement::Expression(Box::new_in(
                        self.parse_assignment_expression()?,
                        self.arena,
                    ));
                }

                if !self.kind(Kind::BracketC) {
                    self.bump_token(Kind::Comma)?;
                }
            }

            elements.push(el)
        }

        self.bump_token(Kind::BracketC)?;

        Ok(Expression::ArrayExpression(Box::new_in(
            ArrayExpression {
                start,
                elements,
                end: self.prev_token_end,
                parenthesized: self.in_paren,
            },
            self.arena,
        )))
    }

    pub fn parse_object_expression_method(
        &mut self,
        is_async: bool,
        is_generator: bool,
        method_kind: ObjectExpressionMethodKind,
    ) -> ParseResult<ObjectExpressionPropertyKind<'a>> {
        let start = self.token.start;

        let (id, is_computed);

        if self.kind(Kind::BracketO) {
            self.bump();

            id = self.parse_assignment_expression()?;
            is_computed = true;

            self.bump_token(Kind::BracketC)?;
        } else {
            self.assert_token(Kind::Identifier)?;

            id = Expression::Identifier(Box::new_in(self.parse_identifier(), self.arena));
            is_computed = false;
        }

        let type_parameters = if self.extensions.ts && self.kind(Kind::LessThan) {
            Some(self.parse_type_parameter_declaration()?)
        } else {
            None
        };

        let params = self.parse_function_parameters()?;
        let body = self.parse_block()?;

        Ok(ObjectExpressionPropertyKind::Method(Box::new_in(
            ObjectExpressionMethod {
                start,
                async_: is_async,
                generator: is_generator,
                computed: is_computed,
                id,
                params,
                body,
                type_parameters,
                kind: method_kind,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_object_expression_property_or_method(
        &mut self,
    ) -> ParseResult<ObjectExpressionPropertyKind<'a>> {
        let start = self.token.start;

        let mut async_token = None;

        if self.kind(Kind::Async) {
            async_token = Some(self.token.clone());
            self.bump();
        }

        if self.kind(Kind::Star) {
            self.bump();

            return self.parse_object_expression_method(
                async_token.is_some(),
                true,
                ObjectExpressionMethodKind::Method,
            );
        }

        if matches!(self.token.kind, Kind::Get | Kind::Set) {
            let peeked_token = self.lexer.peek(LexContext::Normal).kind;

            if peeked_token == Kind::BracketO || is_identifier(peeked_token) {
                self.assert(async_token.is_some(), "invalid token kind")?;

                let method_kind = if self.token.kind == Kind::Get {
                    ObjectExpressionMethodKind::Get
                } else {
                    ObjectExpressionMethodKind::Set
                };

                self.bump();

                return self.parse_object_expression_method(false, false, method_kind);
            }
        }

        if self.is_curr_token_identifier() {
            if async_token.is_some() {
                return self.parse_object_expression_method(
                    true,
                    false,
                    ObjectExpressionMethodKind::Method,
                );
            } else {
                let key = Expression::Identifier(Box::new_in(self.parse_identifier(), self.arena));

                let (value, is_shorthand);

                if self.kind(Kind::Colon) {
                    self.bump();

                    value = self.parse_assignment_expression()?;
                    is_shorthand = false;
                } else {
                    value = key.clone_in(self.arena);
                    is_shorthand = true;
                }

                Ok(ObjectExpressionPropertyKind::Property(Box::new_in(
                    ObjectExpressionProperty {
                        start,
                        computed: false,
                        shorthand: is_shorthand,
                        key,
                        value,
                        end: self.prev_token_end,
                    },
                    self.arena,
                )))
            }
        } else if self.kind(Kind::BracketO) {
            if async_token.is_some() {
                return self.parse_object_expression_method(
                    true,
                    false,
                    ObjectExpressionMethodKind::Method,
                );
            } else {
                self.bump();

                let key = self.parse_assignment_expression()?;
                self.bump_token(Kind::BracketC)?;

                if self.kind(Kind::ParenO) || (self.extensions.ts && self.kind(Kind::LessThan)) {
                    return self.parse_object_expression_method(
                        false,
                        false,
                        ObjectExpressionMethodKind::Method,
                    );
                }

                let (value, is_shorthand);

                if self.kind(Kind::Colon) {
                    self.bump();

                    value = self.parse_assignment_expression()?;
                    is_shorthand = false;
                } else {
                    value = key.clone_in(self.arena);
                    is_shorthand = true;
                }

                Ok(ObjectExpressionPropertyKind::Property(Box::new_in(
                    ObjectExpressionProperty {
                        start,
                        computed: true,
                        shorthand: is_shorthand,
                        key,
                        value,
                        end: self.prev_token_end,
                    },
                    self.arena,
                )))
            }
        } else if let Some(token) = async_token {
            let key = Expression::Identifier(Box::new_in(
                Identifier {
                    start: token.start,
                    parenthesized: false,
                    type_annotation: None,
                    end: token.end,
                },
                self.arena,
            ));

            if self.kind(Kind::ParenO) || (self.extensions.ts && self.kind(Kind::LessThan)) {
                return self.parse_object_expression_method(
                    true,
                    false,
                    ObjectExpressionMethodKind::Method,
                );
            }

            let (value, is_shorthand);

            if self.kind(Kind::Colon) {
                self.bump();

                value = self.parse_assignment_expression()?;
                is_shorthand = false;
            } else {
                value = key.clone_in(self.arena);
                is_shorthand = true;
            }

            Ok(ObjectExpressionPropertyKind::Property(Box::new_in(
                ObjectExpressionProperty {
                    start,
                    computed: false,
                    shorthand: is_shorthand,
                    key,
                    value,
                    end: self.prev_token_end,
                },
                self.arena,
            )))
        } else {
            self.add_diagnostic("invalid token", self.token.start..self.token.end);
            Err(())
        }
    }

    pub fn parse_object_expression(&mut self) -> ParseResult<Expression<'a>> {
        let start = self.token.start;
        let mut properties = ArenaVec::new_in(self.arena);

        // skip {
        self.bump();

        while !matches!(self.token.kind, Kind::EOF | Kind::BracesC) {
            if self.kind(Kind::Dot3) {
                let el = Box::new_in(self.parse_spread_element()?, self.arena);

                properties.push(ObjectExpressionPropertyKind::SpreadElement(el));
            } else {
                let property_or_method = self.parse_object_expression_property_or_method()?;

                properties.push(property_or_method);
            }

            if !self.kind(Kind::BracesC) {
                self.bump_token(Kind::Comma)?;
            }
        }

        self.bump_token(Kind::BracesC)?;

        Ok(Expression::ObjectExpression(Box::new_in(
            ObjectExpression {
                start,
                properties,
                end: self.prev_token_end,
                parenthesized: self.in_paren,
            },
            self.arena,
        )))
    }

    pub fn parse_function_declaration(&mut self) -> ParseResult<Statement<'a>> {
        let func = self.parse_function(false)?;

        Ok(Statement::FunctionDeclaration(Box::new_in(
            func, self.arena,
        )))
    }

    pub fn parse_function_expression(&mut self) -> ParseResult<Expression<'a>> {
        let func = self.parse_function(true)?;

        Ok(Expression::FunctionExpression(Box::new_in(
            func, self.arena,
        )))
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

    pub fn parse_class_declaration(&mut self) -> ParseResult<Statement<'a>> {
        Ok(Statement::ClassDeclaration(Box::new_in(
            self.parse_class(true)?,
            self.arena,
        )))
    }

    pub fn parse_class_expression(&mut self) -> ParseResult<Expression<'a>> {
        Ok(Expression::ClassExpression(Box::new_in(
            self.parse_class(false)?,
            self.arena,
        )))
    }

    pub fn parse_class_element(&mut self) -> ParseResult<ClassBody<'a>> {
        let start = self.token.start;

        if self.kind(Kind::Constructor) {
            let key = Expression::Identifier(Box::new_in(self.parse_identifier(), self.arena));
            let params = self.parse_function_parameters()?;
            let body = self.parse_block()?;

            return Ok(ClassBody::MethodDefinition(Box::new_in(
                ClassMethod {
                    start,
                    static_: false,
                    computed: false,
                    generator: false,
                    async_: false,
                    key,
                    params,
                    body,
                    kind: MethodDefinitionKind::Constructor,
                    end: self.prev_token_end,
                },
                self.arena,
            )));
        }

        let (mut static_token, mut async_token) = (None, None);
        let mut is_computed = false;
        let key;

        if self.kind(Kind::Static) {
            static_token = Some(self.token.clone());
            self.bump();
        }

        if self.kind(Kind::Async) {
            async_token = Some(self.token.clone());
            self.bump();
        }

        if async_token.is_none() && matches!(self.token.kind, Kind::Get | Kind::Set) {
            let peeked_token = self.lexer.peek(LexContext::Normal).kind;

            if peeked_token == Kind::BracketO || is_identifier(peeked_token) {
                let definition_kind = if self.kind(Kind::Get) {
                    MethodDefinitionKind::Get
                } else {
                    MethodDefinitionKind::Set
                };

                self.bump();

                if self.kind(Kind::BracketO) {
                    self.bump();
                    is_computed = true;

                    key = self.parse_assignment_expression()?;
                    self.bump_token(Kind::BracketC)?;
                } else {
                    key = Expression::Identifier(Box::new_in(self.parse_identifier(), self.arena));
                }

                let params = self.parse_function_parameters()?;
                let body = self.parse_block()?;

                return Ok(ClassBody::MethodDefinition(Box::new_in(
                    ClassMethod {
                        start,
                        static_: static_token.is_some(),
                        computed: is_computed,
                        generator: false,
                        async_: false,
                        key,
                        params,
                        body,
                        kind: definition_kind,
                        end: self.prev_token_end,
                    },
                    self.arena,
                )));
            }
        }

        if self.is_curr_token_identifier() {
            key = Expression::Identifier(Box::new_in(self.parse_identifier(), self.arena));

            self.assert(
                async_token.is_some() && !self.kind(Kind::ParenO),
                "invalid token",
            )?;
        } else if self.kind(Kind::BracketO) {
            is_computed = true;
            self.bump();

            key = self.parse_assignment_expression()?;
            self.bump_token(Kind::BracketC)?;

            self.assert(
                async_token.is_some() && !self.kind(Kind::ParenO),
                "invalid token",
            )?;
        } else if let Some(token) = async_token {
            key = Expression::Identifier(Box::new_in(
                Identifier {
                    start: token.start,
                    parenthesized: false,
                    type_annotation: None,
                    end: token.end,
                },
                self.arena,
            ));

            async_token = None;
        } else if let Some(token) = static_token {
            key = Expression::Identifier(Box::new_in(
                Identifier {
                    start: token.start,
                    parenthesized: false,
                    type_annotation: None,
                    end: token.end,
                },
                self.arena,
            ));

            static_token = None;
        } else {
            self.add_diagnostic("invalid token", self.token.start..self.token.end);
            return Err(());
        }

        if self.kind(Kind::ParenO) {
            let params = self.parse_function_parameters()?;
            let body = self.parse_block()?;

            return Ok(ClassBody::MethodDefinition(Box::new_in(
                ClassMethod {
                    start,
                    static_: static_token.is_some(),
                    computed: is_computed,
                    generator: false,
                    async_: async_token.is_some(),
                    key,
                    params,
                    body,
                    kind: MethodDefinitionKind::Method,
                    end: self.prev_token_end,
                },
                self.arena,
            )));
        }

        let value;

        if self.kind(Kind::Equal) {
            value = Some(self.parse_assignment_expression()?);
        } else {
            value = None;
        }

        return Ok(ClassBody::PropertyDefinition(Box::new_in(
            ClassProperty {
                start,
                key,
                value,
                computed: is_computed,
                static_: static_token.is_some(),
                end: self.prev_token_end,
            },
            self.arena,
        )));
    }

    pub fn parse_class_body(&mut self) -> ParseResult<ArenaVec<'a, ClassBody<'a>>> {
        let mut body = ArenaVec::new_in(self.arena);

        self.bump_token(Kind::BracesO)?;

        while !matches!(self.token.kind, Kind::EOF | Kind::BracesC) {
            body.push(self.parse_class_element()?);
        }

        self.bump_token(Kind::BracesC)?;
        Ok(body)
    }

    pub fn parse_class(&mut self, is_id_optional: bool) -> ParseResult<Class<'a>> {
        let start = self.token.start;

        // skip class
        self.bump();

        let id = if self.kind(Kind::Identifier) {
            Some(self.parse_identifier())
        } else {
            None
        };

        self.assert(!is_id_optional && id.is_none(), "class id is required")?;

        let type_parameters = if self.extensions.ts && self.kind(Kind::LessThan) {
            Some(self.parse_type_parameter_declaration()?)
        } else {
            None
        };

        let super_class = if self.kind(Kind::Extends) {
            self.bump();
            Some(self.parse_lhs_expression()?)
        } else {
            None
        };

        let mut implements = ArenaVec::new_in(self.arena);

        if self.kind(Kind::Implements) {
            self.bump();

            while matches!(self.token.kind, Kind::EOF | Kind::BracketO) {
                implements.push(self.parse_ts_interface_heritage()?);

                if self.kind(Kind::Comma) {
                    self.bump();
                }
            }
        }

        let body = self.parse_class_body()?;

        Ok(Class {
            start,
            id,
            body,
            super_class,
            type_parameters,
            implements,
            end: self.prev_token_end,
        })
    }

    pub fn parse_regexp(&mut self) -> ParseResult<Expression<'a>> {
        let start = self.token.start;

        let (pattern_end, flag) = if let Some(inner) = self.lexer.lex_regexp() {
            inner
        } else {
            return Err(());
        };

        let end = self.lexer.index as u32 - 1;

        // update parser state
        self.bump();
        self.prev_token_end = end;

        Ok(Expression::RegularExpression(Box::new_in(
            Regexp {
                start,
                pattern: start..pattern_end,
                flag,
                end,
                parenthesized: self.in_paren,
            },
            self.arena,
        )))
    }

    pub fn parse_template_element(&mut self) -> TemplateElement {
        TemplateElement {
            start: self.token.start,
            tail: false,
            end: self.token.end,
        }
    }

    pub fn parse_template_literal(&mut self) -> ParseResult<Expression<'a>> {
        let start = self.token.start;
        let mut expressions = ArenaVec::new_in(self.arena);
        let mut quasis = ArenaVec::new_in(self.arena);

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
                    expressions.push(self.parse_expression()?);

                    self.assert(!self.kind(Kind::BracesC), "unterminated template literal")?;
                }

                Kind::Error => return Err(()),

                _ => unsafe { core::hint::unreachable_unchecked() },
            }
        }

        // skip the ending backquote
        self.bump();

        let len = quasis.len();
        quasis[len - 1].tail = true;

        Ok(Expression::TemplateLiteral(Box::new_in(
            TemplateLiteral {
                start,
                expressions,
                quasis,
                end: self.prev_token_end,
                parenthesized: self.in_paren,
            },
            self.arena,
        )))
    }

    pub fn parse_super(&mut self) -> ParseResult<Expression<'a>> {
        let exp = Expression::Identifier(Box::new_in(self.parse_identifier(), self.arena));

        if matches!(self.token.kind, Kind::ParenO | Kind::Dot | Kind::BracketO) {
            return Ok(exp);
        }

        self.add_diagnostic("invalid token", self.token.start..self.token.end);
        Err(())
    }

    pub fn parse_new_expression(&mut self) -> ParseResult<Expression<'a>> {
        let start = self.token.start;

        let token = self.token.clone();

        // skip new
        self.bump();

        if self.kind(Kind::Dot) {
            self.bump();

            let meta = Identifier {
                start: token.start,
                parenthesized: false,
                type_annotation: None,
                end: token.end,
            };

            let property = self.parse_identifier();

            Ok(Expression::MetaProperty(Box::new_in(
                MetaProperty {
                    start,
                    meta,
                    property,
                    end: self.prev_token_end,
                },
                self.arena,
            )))
        } else {
            let callee = self.parse_lhs_expression()?;
            let arguments = if self.kind(Kind::ParenO) {
                self.parse_function_arguments()?
            } else {
                ArenaVec::new_in(self.arena)
            };

            Ok(Expression::NewExpression(Box::new_in(
                NewExpression {
                    start,
                    callee,
                    arguments,
                    end: self.prev_token_end,
                    parenthesized: self.in_paren,
                },
                self.arena,
            )))
        }
    }

    pub fn parse_await_expression(&mut self) -> ParseResult<Expression<'a>> {
        let start = self.token.start;

        // skip await
        self.bump();

        let argument = self.parse_assignment_expression()?;

        Ok(Expression::AwaitExpression(Box::new_in(
            AwaitExpression {
                start,
                argument,
                end: self.prev_token_end,
                parenthesized: self.in_paren,
            },
            self.arena,
        )))
    }

    pub fn parse_arrow_function_body(&mut self) -> ParseResult<(ArrowFunctionBody<'a>, bool)> {
        if self.kind(Kind::BracesO) {
            Ok((
                ArrowFunctionBody::Block(Box::new_in(self.parse_block()?, self.arena)),
                false,
            ))
        } else {
            Ok((
                ArrowFunctionBody::Expression(Box::new_in(
                    self.parse_assignment_expression()?,
                    self.arena,
                )),
                true,
            ))
        }
    }

    pub fn parse_function_arguments(&mut self) -> ParseResult<ArenaVec<'a, FunctionArgument<'a>>> {
        let mut args = ArenaVec::new_in(self.arena);

        // skip (
        self.bump();

        while !matches!(self.token.kind, Kind::EOF | Kind::ParenC) {
            let el;

            if self.kind(Kind::Dot3) {
                el = FunctionArgument::SpreadElement(Box::new_in(
                    self.parse_spread_element()?,
                    self.arena,
                ))
            } else {
                el = FunctionArgument::Expression(Box::new_in(
                    self.parse_assignment_expression()?,
                    self.arena,
                ));
            }

            args.push(el);

            if !self.kind(Kind::ParenC) {
                self.bump_token(Kind::Comma)?;
            }
        }

        self.bump_token(Kind::ParenC)?;

        Ok(args)
    }

    pub fn parse_function_parameters(&mut self) -> ParseResult<ArenaVec<'a, FunctionParam<'a>>> {
        let mut params = ArenaVec::new_in(self.arena);

        // skip (
        self.bump();

        while !matches!(self.token.kind, Kind::EOF | Kind::ParenC) {
            if self.kind(Kind::Dot3) {
                let rest_el = self.parse_rest_element()?;

                params.push(FunctionParam::RestElement(Box::new_in(rest_el, self.arena)));

                break;
            } else {
                params.push(FunctionParam::Pattern(Box::new_in(
                    self.parse_pattern_with_default_value()?,
                    self.arena,
                )));
            }

            if self.kind(Kind::Comma) {
                self.bump();
            }
        }

        self.bump_token(Kind::ParenC)?;

        Ok(params)
    }

    pub fn parse_function(&mut self, is_id_optional: bool) -> ParseResult<Function<'a>> {
        let start = self.token.start;

        let is_async = if self.kind(Kind::Async) {
            self.bump();
            true
        } else {
            false
        };

        self.bump_token(Kind::Function)?;

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

        self.assert(!is_id_optional && id.is_none(), "function id is required")?;

        let type_parameters = if self.extensions.ts && self.kind(Kind::LessThan) {
            Some(self.parse_type_parameter_declaration()?)
        } else {
            None
        };

        let params = self.parse_function_parameters()?;
        let body = self.parse_block()?;

        Ok(Function {
            start,
            async_: is_async,
            generator: is_generator,
            id,
            params,
            body,
            type_parameters,
            end: self.prev_token_end,
        })
    }

    pub fn parse_primary_expression(&mut self) -> ParseResult<Expression<'a>> {
        match self.token.kind {
            Kind::Identifier => {
                let id = self.parse_identifier();
                Ok(Expression::Identifier(Box::new_in(id, self.arena)))
            }
            Kind::Async => {
                let peeked = self.lexer.peek(LexContext::Normal).kind;

                if matches!(peeked, Kind::Function) {
                    self.parse_function_expression()
                } else {
                    Ok(Expression::Identifier(Box::new_in(
                        self.parse_identifier(),
                        self.arena,
                    )))
                }
            }
            Kind::Number => Ok(self.parse_numeric_literal_expression()),
            Kind::String => Ok(self.parse_string_literal_expression()),
            Kind::Boolean => Ok(self.parse_boolean_literal_expression()),
            Kind::Null => Ok(self.parse_null_literal_expression()),
            Kind::ParenO => self.parse_group_expression(),
            Kind::BracketO => self.parse_array_expression(),
            Kind::BracesO => self.parse_object_expression(),
            Kind::Function => self.parse_function_expression(),
            Kind::This => Ok(self.parse_this_expression()),
            Kind::Class => self.parse_class_expression(),
            Kind::Slash | Kind::SlashEqual => self.parse_regexp(),
            Kind::BackQuote => self.parse_template_literal(),
            Kind::LessThan if self.extensions.jsx => {
                let peeked_token = self.lexer.peek(LexContext::Normal).kind;
                if peeked_token == Kind::GreaterThan {
                    Ok(Expression::JSXFragment(Box::new_in(
                        self.parse_jsx_fragment()?,
                        self.arena,
                    )))
                } else {
                    Ok(Expression::JSXElement(Box::new_in(
                        self.parse_jsx_element()?,
                        self.arena,
                    )))
                }
            }
            Kind::LessThan if self.extensions.ts => self.parse_ts_type_assertion_exp(),
            _ => {
                self.add_diagnostic("unexpected token", self.token.start..self.token.end);
                Err(())
            }
        }
    }

    pub fn parse_lhs_expression(&mut self) -> ParseResult<Expression<'a>> {
        let mut exp = match self.token.kind {
            Kind::Super => self.parse_super(),
            Kind::New => self.parse_new_expression(),
            _ => self.parse_primary_expression(),
        }?;

        loop {
            let is_optional = if self.kind(Kind::QuestionDot) {
                self.bump();
                true
            } else {
                false
            };

            if self.kind(Kind::ParenO) {
                let func_arguments = self.parse_function_arguments()?;

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
            } else if self.kind(Kind::BracketO) {
                self.bump();
                let property = self.parse_expression()?;

                self.bump_token(Kind::BracketC)?;

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
                ));
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
                let quasi = match self.parse_template_literal()? {
                    Expression::TemplateLiteral(inner) => Box::into_inner(inner),
                    _ => unsafe { core::hint::unreachable_unchecked() },
                };

                exp = Expression::TaggedTemplateLiteral(Box::new_in(
                    TaggedTemplateLiteral {
                        start: exp.start(),
                        tag: exp,
                        quasi,
                        end: self.prev_token_end,
                    },
                    self.arena,
                ))
            } else if self.extensions.ts && self.kind(Kind::LessThan) {
                let Some(type_parameter_arguments) =
                    self.try_parse_or_rewind(Self::parse_type_parameter_arguments_in_exp)
                else {
                    break;
                };

                exp = Expression::TsInstantiationExpression(Box::new_in(
                    TsInstantiationExpression {
                        start: exp.start(),
                        expression: exp,
                        type_parameter_arguments,
                        end: self.prev_token_end,
                    },
                    self.arena,
                ))
            } else if self.extensions.ts && self.kind(Kind::Bang) && !self.token.is_on_new_line {
                exp = self.parse_ts_non_null_expression(exp);
            } else {
                break;
            }
        }

        Ok(exp)
    }

    pub fn parse_update_expression(&mut self) -> ParseResult<Expression<'a>> {
        let start = self.token.start;

        if matches!(self.token.kind, Kind::Plus2 | Kind::Minus2) {
            let operator = match self.consume_token() {
                Kind::Plus2 => UpdateOperator::Increment,
                Kind::Minus2 => UpdateOperator::Decrement,
                _ => unsafe { core::hint::unreachable_unchecked() },
            };

            let argument = self.parse_unary_expression()?;

            Ok(Expression::UpdateExpression(Box::new_in(
                UpdateExpression {
                    start,
                    operator,
                    argument,
                    prefix: true,
                    end: self.prev_token_end,
                    parenthesized: self.in_paren,
                },
                self.arena,
            )))
        } else {
            let exp = self.parse_lhs_expression()?;

            if !self.token.is_on_new_line && matches!(self.token.kind, Kind::Plus2 | Kind::Minus2) {
                let operator = match self.consume_token() {
                    Kind::Plus2 => UpdateOperator::Increment,
                    Kind::Minus2 => UpdateOperator::Decrement,
                    _ => unsafe { core::hint::unreachable_unchecked() },
                };

                Ok(Expression::UpdateExpression(Box::new_in(
                    UpdateExpression {
                        start,
                        operator,
                        argument: exp,
                        prefix: false,
                        end: self.prev_token_end,
                        parenthesized: self.in_paren,
                    },
                    self.arena,
                )))
            } else {
                Ok(exp)
            }
        }
    }

    pub fn parse_unary_expression(&mut self) -> ParseResult<Expression<'a>> {
        let start = self.token.start;

        if let Some(operator) = UnaryOperator::from(self.token.kind) {
            self.bump();

            Ok(Expression::UnaryExpression(Box::new_in(
                UnaryExpression {
                    start,
                    operator,
                    argument: self.parse_unary_expression()?,
                    prefix: true,
                    end: self.prev_token_end,
                    parenthesized: self.in_paren,
                },
                self.arena,
            )))
        } else if self.kind(Kind::Await) {
            self.parse_await_expression()
        } else {
            self.parse_update_expression()
        }
    }

    pub fn parse_binary_expression(&mut self) -> ParseResult<Expression<'a>> {
        let mut stack: ArenaVec<'_, (Option<BinaryOperator>, Expression<'a>)> =
            ArenaVec::new_in(self.arena);

        stack.push((None, self.parse_unary_expression()?));

        loop {
            self.token = self.lexer.relex_binary_op(self.token.clone());
            let Some(op) = BinaryOperator::from(self.token.kind) else {
                break;
            };

            let (prev_op, _) = unsafe { stack.last().unwrap_unchecked() };

            let prev_precedence = if let Some(op) = prev_op {
                if BinaryOperator::As == *op {
                    let (_, prev_exp) = unsafe { stack.pop().unwrap_unchecked() };

                    stack.push((None, self.parse_ts_as_expression(prev_exp)?));
                    continue;
                } else if BinaryOperator::Satisfies == *op {
                    let (_, prev_exp) = unsafe { stack.pop().unwrap_unchecked() };

                    stack.push((None, self.parse_ts_satisfies_expression(prev_exp)?));
                    continue;
                }

                op.precedence()
            } else {
                0
            };

            let precedence = op.precedence();

            // skip the operator
            self.bump();

            if precedence < prev_precedence {
                let (mut temp_op, mut exp) = unsafe { stack.pop().unwrap_unchecked() };

                while let Some((curr_op, left)) = stack.pop() {
                    let end = exp.end();

                    exp = Expression::BinaryExpression(Box::new_in(
                        BinaryExpression {
                            start: left.start(),
                            left,
                            operator: unsafe { temp_op.unwrap_unchecked() },
                            right: exp,
                            end,
                            parenthesized: self.in_paren,
                        },
                        self.arena,
                    ));

                    temp_op = curr_op;
                }

                stack.push((None, exp));
                stack.push((Some(op), self.parse_unary_expression()?))
            } else {
                stack.push((Some(op), self.parse_unary_expression()?));
            }
        }

        let (mut temp_op, mut exp) = unsafe { stack.pop().unwrap_unchecked() };

        while let Some((curr_op, left)) = stack.pop() {
            let end = exp.end();

            exp = Expression::BinaryExpression(Box::new_in(
                BinaryExpression {
                    start: left.start(),
                    left,
                    operator: unsafe { temp_op.unwrap_unchecked() },
                    right: exp,
                    end,
                    parenthesized: self.in_paren,
                },
                self.arena,
            ));

            temp_op = curr_op;
        }

        Ok(exp)
    }

    pub fn parse_conditional_expression(&mut self) -> ParseResult<Expression<'a>> {
        let mut exp = self.parse_binary_expression()?;

        if self.kind(Kind::Question) {
            self.bump();
            let consequent = self.parse_assignment_expression()?;
            self.bump_token(Kind::Colon)?;

            let alternate = self.parse_assignment_expression()?;

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

        Ok(exp)
    }

    pub fn speculatively_parse_arrow_function(&mut self) -> Option<Expression<'a>> {
        let start = self.token.start;

        let is_async = if self.kind(Kind::Async) {
            self.bump();
            true
        } else {
            false
        };

        let type_parameters = if self.kind(Kind::LessThan) {
            if !self.extensions.ts {
                return None;
            }

            let Ok(type_params) = self.parse_type_parameter_declaration() else {
                return None;
            };

            if !self.kind(Kind::ParenO) {
                return None;
            }

            Some(type_params)
        } else {
            None
        };

        let params = if self.kind(Kind::ParenO) {
            let Ok(params) = self.parse_function_parameters() else {
                return None;
            };

            params
        } else if self.is_curr_token_identifier() {
            let ident = Pattern::Identifier(Box::new_in(self.parse_identifier(), self.arena));

            bumpalo::vec![in self.arena;
            FunctionParam::Pattern(Box::new_in(ident, self.arena))
            ]
        } else {
            return None;
        };

        let Ok((body, is_exp)) = self.parse_arrow_function_body() else {
            return None;
        };

        Some(Expression::ArrowFunctionExpression(Box::new_in(
            ArrowFunction {
                start,
                async_: is_async,
                params,
                type_parameters,
                body,
                expression: is_exp,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_assignment_expression(&mut self) -> ParseResult<Expression<'a>> {
        if let Some(arrow_exp) = self.try_parse_or_rewind(Self::speculatively_parse_arrow_function)
        {
            return Ok(arrow_exp);
        }

        let mut exp = self.parse_conditional_expression()?;

        if let Some(operator) = AssignmentOperator::from(self.token.kind) {
            let start = exp.start();
            self.bump();

            let left =
                match exp {
                    Expression::ArrayExpression(_) => AssignmentExpressionLHS::Pattern(
                        Box::new_in(self.reinterpret_as_pattern(exp)?, self.arena),
                    ),

                    Expression::ObjectExpression(_) => AssignmentExpressionLHS::Pattern(
                        Box::new_in(self.reinterpret_as_pattern(exp)?, self.arena),
                    ),

                    _ => AssignmentExpressionLHS::Expression(Box::new_in(exp, self.arena)),
                };

            exp = Expression::AssignmentExpression(Box::new_in(
                AssignmentExpression {
                    start,
                    operator,
                    left,
                    right: self.parse_assignment_expression()?,
                    end: self.prev_token_end,
                    parenthesized: self.in_paren,
                },
                self.arena,
            ))
        }

        Ok(exp)
    }

    pub fn parse_variable_declarator(
        &mut self,
        kind: VariableDeclarationKind,
    ) -> ParseResult<VariableDeclarator<'a>> {
        let start = self.token.start;

        let id = self.parse_pattern()?;

        let init = if kind == VariableDeclarationKind::Const {
            self.bump_token(Kind::Equal)?;
            Some(self.parse_assignment_expression()?)
        } else {
            if self.token.kind == Kind::Equal {
                self.bump();
                Some(self.parse_assignment_expression()?)
            } else {
                None
            }
        };

        Ok(VariableDeclarator {
            start,
            id,
            init,
            end: self.prev_token_end,
        })
    }

    pub fn parse_variable_declaration(
        &mut self,
        declaration_kind: VariableDeclarationKind,
    ) -> ParseResult<Statement<'a>> {
        let start = self.token.start;
        let mut declarations = ArenaVec::new_in(self.arena);

        // skip var, let, or const
        self.bump();

        loop {
            let declarator = self.parse_variable_declarator(declaration_kind)?;
            declarations.push(declarator);

            if self.kind(Kind::Comma) {
                self.bump();
            } else {
                break;
            }
        }

        Ok(Statement::VariableDeclaration(Box::new_in(
            VariableDeclaration {
                kind: declaration_kind,
                declarations,
                start,
                end: self.prev_token_end,
            },
            self.arena,
        )))
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

    pub fn parse_namespace_import_specifier(&mut self) -> ParseResult<ImportSpecifierType<'a>> {
        let start = self.token.start;

        // skip *
        self.bump();

        self.bump_token(Kind::As)?;

        self.assert_token(Kind::Identifier)?;

        let local = self.parse_identifier();

        Ok(ImportSpecifierType::ImportNamespaceSpecifier(Box::new_in(
            ImportNamespaceSpecifier {
                start,
                local,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_named_import_specifier(&mut self) -> ParseResult<ImportSpecifierType<'a>> {
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
            self.add_diagnostic("invalid token", self.token.start..self.token.end);
            return Err(());
        }

        let local = if self.kind(Kind::As) {
            if !matches!(self.token.kind, Kind::Identifier | Kind::Async) {
                self.add_diagnostic("invalid token", self.token.start..self.token.end);
                return Err(());
            }

            Some(self.parse_identifier())
        } else {
            None
        };

        Ok(ImportSpecifierType::ImportSpecifier(Box::new_in(
            ImportSpecifier {
                start,
                imported,
                local,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_import_assertions(&mut self) -> ParseResult<ArenaVec<'a, ImportAttribute<'a>>> {
        let mut assertions = ArenaVec::new_in(self.arena);

        if self.kind(Kind::Assert) {
            self.bump();

            self.bump_token(Kind::BracesO)?;

            while !matches!(self.token.kind, Kind::EOF | Kind::BracesC) {
                let start = self.token.start;
                let (key, value);

                key = if self.kind(Kind::Identifier) {
                    IdentifierOrLiteral::Identifier(Box::new_in(
                        self.parse_identifier(),
                        self.arena,
                    ))
                } else if self.kind(Kind::String) {
                    IdentifierOrLiteral::Literal(Box::new_in(
                        self.parse_string_literal(),
                        self.arena,
                    ))
                } else {
                    self.add_diagnostic("invalid token", self.token.start..self.token.end);
                    return Err(());
                };

                value = if self.kind(Kind::String) {
                    self.parse_string_literal()
                } else {
                    self.add_diagnostic("invalid token", self.token.start..self.token.end);
                    return Err(());
                };

                assertions.push(ImportAttribute {
                    start,
                    key,
                    value,
                    end: self.prev_token_end,
                });

                if !self.kind(Kind::BracesC) {
                    self.bump_token(Kind::Comma)?;
                }
            }

            self.bump_token(Kind::BracesC)?;
        }

        Ok(assertions)
    }

    pub fn parse_import_declaration(&mut self) -> ParseResult<Statement<'a>> {
        let start = self.token.start;

        // skip import
        self.bump();

        let mut specifiers = ArenaVec::new_in(self.arena);

        if !self.kind(Kind::String) {
            if matches!(self.token.kind, Kind::Identifier | Kind::Async) {
                specifiers.push(self.parse_default_import_specifier());
            }

            if self.kind(Kind::Comma) {
                self.bump();
            }

            if self.kind(Kind::Star) {
                specifiers.push(self.parse_namespace_import_specifier()?);
            } else if self.kind(Kind::BracesO) {
                self.bump();

                while !matches!(self.token.kind, Kind::EOF | Kind::BracesC) {
                    specifiers.push(self.parse_named_import_specifier()?);

                    if !self.kind(Kind::BracesC) {
                        self.bump_token(Kind::Comma)?;
                    }
                }

                self.bump_token(Kind::BracesC)?;
            }

            self.bump_token(Kind::From)?;
        }

        self.assert_token(Kind::String)?;

        let source = self.parse_string_literal();
        let assertions = self.parse_import_assertions()?;

        Ok(Statement::ImportDeclaration(Box::new_in(
            ImportDeclaration {
                start,
                specifiers,
                source,
                assertions,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_export_all_declaration(&mut self) -> ParseResult<Statement<'a>> {
        let start = self.token.start;

        // skip export
        self.bump();

        // skip *
        self.bump();

        let exported;

        if self.kind(Kind::As) {
            self.bump();

            if self.is_curr_token_identifier() {
                exported = Some(IdentifierOrLiteral::Identifier(Box::new_in(
                    self.parse_identifier(),
                    self.arena,
                )))
            } else {
                exported = Some(IdentifierOrLiteral::Literal(Box::new_in(
                    self.parse_string_literal(),
                    self.arena,
                )))
            }
        } else {
            exported = None;
        }

        self.bump_token(Kind::From)?;

        let source = self.parse_string_literal();

        Ok(Statement::ExportAllDeclaration(Box::new_in(
            ExportAllDeclaration {
                start,
                source,
                exported,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_default_export_declaration(&mut self) -> ParseResult<Statement<'a>> {
        let start = self.token.start;

        // skip export
        self.bump();

        // skip default
        self.bump();

        let declaration = match self.token.kind {
            Kind::Function => {
                Declaration::Function(Box::new_in(self.parse_function(false)?, self.arena))
            }
            Kind::Class => Declaration::Class(Box::new_in(self.parse_class(false)?, self.arena)),
            Kind::Let | Kind::Const | Kind::Var => {
                let declaration_kind = match self.token.kind {
                    Kind::Let => VariableDeclarationKind::Let,
                    Kind::Const => VariableDeclarationKind::Const,
                    Kind::Var => VariableDeclarationKind::Var,
                    _ => unsafe { core::hint::unreachable_unchecked() },
                };

                let declaration = match self.parse_variable_declaration(declaration_kind)? {
                    Statement::VariableDeclaration(v_declaration) => v_declaration,
                    _ => unsafe { core::hint::unreachable_unchecked() },
                };

                Declaration::Variable(declaration)
            }
            _ => {
                self.add_diagnostic("invalid token", self.token.range());
                return Err(());
            }
        };

        Ok(Statement::ExportDefaultDeclaration(Box::new_in(
            ExportDefaultDeclaration {
                start,
                declaration,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_export_named_declaration(&mut self) -> ParseResult<Statement<'a>> {
        let start = self.token.start;

        // skip export
        self.bump();

        let declaration = match self.token.kind {
            Kind::Function => {
                Declaration::Function(Box::new_in(self.parse_function(false)?, self.arena))
            }
            Kind::Class => Declaration::Class(Box::new_in(self.parse_class(false)?, self.arena)),
            Kind::Let | Kind::Const | Kind::Var => {
                let declaration_kind = match self.token.kind {
                    Kind::Let => VariableDeclarationKind::Let,
                    Kind::Const => VariableDeclarationKind::Const,
                    Kind::Var => VariableDeclarationKind::Var,
                    _ => unsafe { core::hint::unreachable_unchecked() },
                };

                let declaration = match self.parse_variable_declaration(declaration_kind)? {
                    Statement::VariableDeclaration(v_declaration) => v_declaration,
                    _ => unsafe { core::hint::unreachable_unchecked() },
                };

                Declaration::Variable(declaration)
            }
            _ => {
                self.add_diagnostic("invalid token", self.token.range());
                return Err(());
            }
        };

        Ok(Statement::ExportNamedDeclaration(Box::new_in(
            ExportNamedDeclaration {
                start,
                specifiers: ArenaVec::new_in(self.arena),
                declaration: Some(declaration),
                source: None,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_export_named_declaration_with_specifiers(&mut self) -> ParseResult<Statement<'a>> {
        let start = self.token.start;
        let mut specifiers = ArenaVec::new_in(self.arena);

        // skip export
        self.bump();

        // skip {
        self.bump();

        while matches!(self.token.kind, Kind::BracesC | Kind::EOF) {
            let start = self.token.start;

            let (local, exported);

            local = match self.token.kind {
                Kind::Identifier => IdentifierOrLiteral::Identifier(Box::new_in(
                    self.parse_identifier(),
                    self.arena,
                )),
                Kind::String => IdentifierOrLiteral::Literal(Box::new_in(
                    self.parse_string_literal(),
                    self.arena,
                )),
                _ => {
                    self.add_diagnostic("invalid token", self.token.range());
                    return Err(());
                }
            };

            if self.kind(Kind::As) {
                self.bump();

                exported = match self.token.kind {
                    Kind::Identifier => IdentifierOrLiteral::Identifier(Box::new_in(
                        self.parse_identifier(),
                        self.arena,
                    )),
                    Kind::String => IdentifierOrLiteral::Literal(Box::new_in(
                        self.parse_string_literal(),
                        self.arena,
                    )),
                    _ => {
                        self.add_diagnostic("invalid token", self.token.range());
                        return Err(());
                    }
                };
            } else {
                exported = local.clone_in(self.arena);
            }

            specifiers.push(ExportSpecifier {
                start,
                local,
                exported,
                end: self.prev_token_end,
            });

            if !self.kind(Kind::BracesC) {
                self.bump_token(Kind::Comma)?;
            }
        }

        self.bump_token(Kind::BracesC)?;

        let source;

        if self.kind(Kind::From) {
            self.bump();
            source = Some(self.parse_string_literal());
        } else {
            source = None;
        }

        self.bump_token(Kind::BracesC)?;

        Ok(Statement::ExportNamedDeclaration(Box::new_in(
            ExportNamedDeclaration {
                start,
                declaration: None,
                specifiers,
                source,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_export_declaration(&mut self) -> ParseResult<Statement<'a>> {
        match self.token.kind {
            Kind::Star => self.parse_export_all_declaration(),
            Kind::Default => self.parse_default_export_declaration(),
            Kind::Const | Kind::Let | Kind::Var => self.parse_export_named_declaration(),
            Kind::BracesO => self.parse_export_named_declaration_with_specifiers(),
            _ => {
                self.add_diagnostic("invalid token", self.token.range());
                return Err(());
            }
        }
    }

    pub fn parse_block(&mut self) -> ParseResult<Block<'a>> {
        let start = self.token.start;
        let mut body = ArenaVec::new_in(self.arena);

        // skip {
        self.bump();

        while !matches!(self.token.kind, Kind::EOF | Kind::BracesC) {
            body.push(self.parse_statement()?);
        }

        self.bump_token(Kind::BracesC)?;

        Ok(Block {
            start,
            body,
            end: self.prev_token_end,
        })
    }

    pub fn parse_block_statement(&mut self) -> ParseResult<Statement<'a>> {
        Ok(Statement::BlockStatement(Box::new_in(
            self.parse_block()?,
            self.arena,
        )))
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

    pub fn parse_do_while_loop(&mut self) -> ParseResult<Statement<'a>> {
        let start = self.token.start;

        // skip do
        self.bump();
        let body = self.parse_statement()?;

        self.bump_token(Kind::While)?;
        self.bump_token(Kind::ParenO)?;

        let test = self.parse_expression()?;

        self.bump_token(Kind::ParenC)?;

        Ok(Statement::DoWhileLoop(Box::new_in(
            DoWhileLoop {
                start,
                test,
                body,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_while_loop(&mut self) -> ParseResult<Statement<'a>> {
        let start = self.token.start;

        // skip while
        self.bump();

        self.bump_token(Kind::ParenO)?;
        let test = self.parse_expression()?;
        self.bump_token(Kind::ParenC)?;

        let body = self.parse_statement()?;

        Ok(Statement::WhileLoop(Box::new_in(
            WhileLoop {
                start,
                test,
                body,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_for_loop(&mut self) -> ParseResult<Statement<'a>> {
        let start = self.token.start;

        // skip for
        self.bump();

        self.bump_token(Kind::ParenO)?;

        let init;

        if self.kind(Kind::Semicolon) {
            init = None;
        } else if matches!(self.token.kind, Kind::Const | Kind::Let | Kind::Var) {
            let declaration_kind = match self.token.kind {
                Kind::Let => VariableDeclarationKind::Let,
                Kind::Const => VariableDeclarationKind::Const,
                Kind::Var => VariableDeclarationKind::Var,
                _ => unsafe { core::hint::unreachable_unchecked() },
            };

            let declaration = match self.parse_variable_declaration(declaration_kind)? {
                Statement::VariableDeclaration(v_declaration) => v_declaration,
                _ => unsafe { core::hint::unreachable_unchecked() },
            };

            init = Some(ForLoopInit::VariableDeclaration(declaration));
        } else {
            init = Some(ForLoopInit::Expression(Box::new_in(
                self.parse_expression()?,
                self.arena,
            )));
        }

        if init.is_none() || self.kind(Kind::Semicolon) {
            self.bump_token(Kind::Semicolon)?;

            let test = if self.kind(Kind::Semicolon) {
                self.bump();
                None
            } else {
                let temp = Some(self.parse_expression()?);
                self.bump_token(Kind::Semicolon)?;
                temp
            };

            let update = if self.kind(Kind::ParenC) {
                None
            } else {
                Some(self.parse_expression()?)
            };

            self.bump_token(Kind::ParenC)?;

            let body = self.parse_statement()?;

            Ok(Statement::ForLoop(Box::new_in(
                ForLoop {
                    start,
                    init,
                    test,
                    update,
                    body,
                    end: self.prev_token_end,
                },
                self.arena,
            )))
        } else if self.kind(Kind::In) {
            self.bump();

            let left = match unsafe { init.unwrap_unchecked() } {
                ForLoopInit::VariableDeclaration(v_declaration) => {
                    ForInLoopLeft::VariableDeclaration(v_declaration)
                }
                ForLoopInit::Expression(exp) => {
                    let exp = Box::into_inner(exp);

                    ForInLoopLeft::Pattern(Box::new_in(
                        self.reinterpret_as_pattern(exp)?,
                        self.arena,
                    ))
                }
            };

            let right = self.parse_expression()?;

            self.bump_token(Kind::ParenC)?;

            let body = self.parse_statement()?;

            Ok(Statement::ForInLoop(Box::new_in(
                ForInLoop {
                    start,
                    left,
                    right,
                    body,
                    end: self.prev_token_end,
                },
                self.arena,
            )))
        } else {
            self.add_diagnostic("invalid for loop", self.token.range());
            return Err(());
        }
    }

    pub fn parse_if_statement(&mut self) -> ParseResult<Statement<'a>> {
        let start = self.token.start;

        // skip if
        self.bump();

        self.bump_token(Kind::ParenO)?;
        let test = self.parse_expression()?;
        self.bump_token(Kind::ParenC)?;

        let consequent = self.parse_statement()?;

        let alternate = if self.kind(Kind::Else) {
            self.bump();
            Some(self.parse_statement()?)
        } else {
            None
        };

        Ok(Statement::IfStatement(Box::new_in(
            IfStatement {
                start,
                test,
                consequent,
                alternate,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_return_statement(&mut self) -> ParseResult<Statement<'a>> {
        let start = self.token.start;

        // skip return
        self.bump();

        let argument;

        if matches!(self.token.kind, Kind::EOF | Kind::Semicolon) || self.token.is_on_new_line {
            argument = None;
        } else {
            argument = Some(self.parse_expression()?);
        }

        Ok(Statement::ReturnStatement(Box::new_in(
            ReturnStatement {
                start,
                argument,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_switch_case(&mut self) -> ParseResult<SwitchCase<'a>> {
        let start = self.token.start;

        let test;
        let mut consequent = ArenaVec::new_in(self.arena);

        if self.kind(Kind::Default) {
            self.bump();
            test = None;
        } else {
            self.bump_token(Kind::Case)?;
            test = Some(self.parse_expression()?);
        }

        self.bump_token(Kind::Colon)?;

        while !matches!(
            self.token.kind,
            Kind::EOF | Kind::BracesC | Kind::Case | Kind::Default
        ) {
            consequent.push(self.parse_statement()?);
        }

        Ok(SwitchCase {
            start,
            test,
            consequent,
            end: self.prev_token_end,
        })
    }

    pub fn parse_switch_statement(&mut self) -> ParseResult<Statement<'a>> {
        let start = self.token.start;
        let mut cases = ArenaVec::new_in(self.arena);

        // skip switch
        self.bump();

        self.bump_token(Kind::ParenO)?;
        let discriminant = self.parse_expression()?;
        self.bump_token(Kind::ParenC)?;

        self.bump_token(Kind::BracesO)?;

        while !matches!(self.token.kind, Kind::EOF | Kind::BracesC) {
            cases.push(self.parse_switch_case()?);
        }

        self.bump_token(Kind::BracesC)?;

        Ok(Statement::SwitchStatement(Box::new_in(
            SwitchStatement {
                start,
                discriminant,
                cases,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_throw_statement(&mut self) -> ParseResult<Statement<'a>> {
        let start = self.token.start;

        // skip throw
        self.bump();

        let argument = self.parse_expression()?;

        Ok(Statement::ThrowStatement(Box::new_in(
            ThrowStatement {
                start,
                argument,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_catch_clause(&mut self) -> ParseResult<CatchClause<'a>> {
        let start = self.token.start;

        // skip catch
        self.bump();

        self.bump_token(Kind::ParenO)?;
        let param = self.parse_pattern()?;
        self.bump_token(Kind::ParenC)?;

        self.assert_token(Kind::BracesO)?;

        let body = self.parse_block()?;

        Ok(CatchClause {
            start,
            param,
            body,
            end: self.prev_token_end,
        })
    }

    pub fn parse_try_statement(&mut self) -> ParseResult<Statement<'a>> {
        let start = self.token.start;

        // skip try
        self.bump();

        self.assert_token(Kind::BracesO)?;

        let block = self.parse_block()?;

        let handler = if self.kind(Kind::Catch) {
            Some(self.parse_catch_clause()?)
        } else {
            None
        };

        let finalizer = if self.kind(Kind::Finally) {
            self.bump();

            self.assert_token(Kind::BracesO)?;

            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(Statement::TryStatement(Box::new_in(
            TryStatement {
                start,
                block,
                handler,
                finalizer,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_with_statement(&mut self) -> ParseResult<Statement<'a>> {
        let start = self.token.start;

        // skip with
        self.bump();

        self.bump_token(Kind::ParenO)?;
        let object = self.parse_expression()?;
        self.bump_token(Kind::ParenC)?;

        let body = self.parse_statement()?;

        Ok(Statement::WithStatement(Box::new_in(
            WithStatement {
                start,
                object,
                body,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_labelled_statement(&mut self) -> ParseResult<Statement<'a>> {
        let start = self.token.start;

        let exp = self.parse_expression()?;

        match exp {
            Expression::Identifier(ident) if self.kind(Kind::Colon) => {
                self.bump();

                let label = Box::into_inner(ident);

                Ok(Statement::LabelledStatement(Box::new_in(
                    LabelledStatement {
                        start,
                        label,
                        body: self.parse_statement()?,
                        end: self.prev_token_end,
                    },
                    self.arena,
                )))
            }
            _ => Ok(Statement::ExpressionStatement(Box::new_in(
                ExpressionStatement {
                    start,
                    exp,
                    end: self.prev_token_end,
                },
                self.arena,
            ))),
        }
    }

    pub fn parse_expression(&mut self) -> ParseResult<Expression<'a>> {
        let exp = self.parse_assignment_expression()?;

        if self.kind(Kind::Comma) {
            let mut expressions = ArenaVec::new_in(self.arena);
            let start = exp.start();

            expressions.push(exp);

            while !matches!(self.token.kind, Kind::EOF) {
                if !self.kind(Kind::Comma) {
                    break;
                }

                // skip ,
                self.bump();

                expressions.push(self.parse_assignment_expression()?);
            }

            return Ok(Expression::SequenceExpression(Box::new_in(
                SequenceExpression {
                    start,
                    expressions,
                    end: self.prev_token_end,
                    parenthesized: true,
                },
                self.arena,
            )));
        }

        Ok(exp)
    }

    pub fn parse_statement(&mut self) -> ParseResult<Statement<'a>> {
        let statement = match self.token.kind {
            Kind::Const => self.parse_variable_declaration(VariableDeclarationKind::Const),
            Kind::Let => self.parse_variable_declaration(VariableDeclarationKind::Let),
            Kind::Var => self.parse_variable_declaration(VariableDeclarationKind::Var),
            Kind::Import => self.parse_import_declaration(),
            Kind::Export => self.parse_export_declaration(),
            Kind::Function => self.parse_function_declaration(),
            Kind::Class => self.parse_class_declaration(),
            Kind::BracesO => self.parse_block_statement(),
            Kind::Semicolon => Ok(self.parse_empty_statement()),
            Kind::Break => Ok(self.parse_break_statement()),
            Kind::Continue => Ok(self.parse_continue_statement()),
            Kind::Debugger => Ok(self.parse_debugger_statement()),
            Kind::Do => self.parse_do_while_loop(),
            Kind::While => self.parse_while_loop(),
            Kind::For => self.parse_for_loop(),
            Kind::If => self.parse_if_statement(),
            Kind::Return => self.parse_return_statement(),
            Kind::Switch => self.parse_switch_statement(),
            Kind::Throw => self.parse_throw_statement(),
            Kind::Try => self.parse_try_statement(),
            Kind::With => self.parse_with_statement(),
            Kind::Async => {
                let peeked = self.lexer.peek(LexContext::Normal).kind;

                if matches!(peeked, Kind::Function) {
                    self.parse_function_declaration()
                } else {
                    self.parse_labelled_statement()
                }
            }
            Kind::Type if self.extensions.ts => {
                let peeked_token = self.lexer.peek(LexContext::Normal);

                if is_identifier(peeked_token.kind) && !peeked_token.is_on_new_line {
                    self.parse_ts_type_alias_declaration()
                } else {
                    self.parse_labelled_statement()
                }
            }
            Kind::Interface if self.extensions.ts => {
                let peeked_token = self.lexer.peek(LexContext::Normal);

                if is_identifier(peeked_token.kind) && !peeked_token.is_on_new_line {
                    self.parse_ts_interface_declaration()
                } else {
                    self.parse_labelled_statement()
                }
            }
            _ => self.parse_labelled_statement(),
        };

        if self.kind(Kind::Semicolon) {
            self.bump()
        } else if !self.token.is_on_new_line {
            self.assert(
                !self.kind(Kind::EOF) && !self.kind(Kind::BracesC),
                "invalid token",
            )?;
        }

        statement
    }

    pub fn parse(mut self) -> Program<'a> {
        let mut ast = ArenaVec::new_in(self.arena);
        let mut fatal_error = false;

        loop {
            if self.kind(Kind::EOF) {
                break;
            }

            if let Ok(stmt) = self.parse_statement() {
                ast.push(stmt);
            } else {
                fatal_error = true;
                break;
            }
        }

        Program {
            ast,
            diagnostics: self.lexer.diagnostics,
            fatal_error,
            comments: Vec::new(),
        }
    }

    pub fn new(arena: &'a Bump, code: &'a str, extensions: Extensions) -> Self {
        let mut lexer = Tokenizer::new(code);
        let token = lexer.lex(LexContext::Normal);

        Parser {
            code,
            lexer,
            ctx: ParserContext::default(),
            token,
            prev_token_end: 0,
            in_paren: false,
            arena,
            extensions,
        }
    }
}

pub fn is_identifier(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::Identifier
            | Kind::As
            | Kind::From
            | Kind::Boolean
            | Kind::Async
            | Kind::Assert
            | Kind::Constructor
            | Kind::Get
            | Kind::Set
    )
}

pub fn is_reserved_keyword(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::Break
            | Kind::Case
            | Kind::Catch
            | Kind::Class
            | Kind::Continue
            | Kind::Const
            | Kind::Debugger
            | Kind::Delete
            | Kind::Do
            | Kind::Default
            | Kind::Export
            | Kind::Else
            | Kind::Extends
            | Kind::Function
            | Kind::For
            | Kind::Finally
            | Kind::If
            | Kind::In
            | Kind::Instanceof
            | Kind::Import
            | Kind::Implements
            | Kind::Let
            | Kind::New
            | Kind::Null
            | Kind::Return
            | Kind::Super
            | Kind::Switch
            | Kind::Static
            | Kind::This
            | Kind::Typeof
            | Kind::Throw
            | Kind::Try
            | Kind::Var
            | Kind::Void
            | Kind::While
            | Kind::With
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{fs::read_to_string, time::Instant};

    #[test]
    fn test_parser() {
        let code = read_to_string("../../data/input.js").unwrap();
        let arena = Bump::new();

        let mut p = Parser::new(
            &arena,
            code.as_str(),
            Extensions {
                ts: true,
                jsx: false,
            },
        );

        let Program { ast, .. } = p.parse();

        for s in ast.iter() {
            println!("{:#?}", s);
        }
    }

    #[test]
    fn parser_performance() {
        let code = read_to_string("../../data/input.js").unwrap();
        let arena = Bump::new();

        let mut p = Parser::new(
            &arena,
            code.as_str(),
            Extensions {
                ts: true,
                jsx: false,
            },
        );

        let start = Instant::now();

        p.parse();

        let end = start.elapsed();
        println!("{}", end.as_millis());
    }
}
