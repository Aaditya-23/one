use bumpalo::{boxed::Box, collections::Vec, Bump};

use crate::{
    ast::{
        javascript::{
            BinaryOperator, Expression, Identifier, Location, MemberExpression, Statement,
        },
        typescript::{
            IdentifierOrMemberExpression, IdentifierOrQualifiedName, TsAsExpression,
            TsIndexedAccess, TsInstantiationExpression, TsInterfaceDeclaration,
            TsInterfaceHeritage, TsMethodSignature, TsNonNullExpression, TsPropertyMember,
            TsPropertySignature, TsSatisfiesExpression, TsTypeAliasDeclaration, TsTypeAnnotation,
            TsTypeAny, TsTypeArray, TsTypeAssertion, TsTypeIdentifier, TsTypeIntersection,
            TsTypeLiteral, TsTypeObjectLiteral, TsTypeOperator, TsTypeOperatorKind,
            TsTypeParameter, TsTypeParameterArguments, TsTypeParameterDeclaration,
            TsTypeParenthesized, TsTypeQualifiedName, TsTypeReference, TsTypeTuple,
            TsTypeUndefined, TsTypeUnion,
        },
    },
    kind::Kind,
    parser::{ParseResult, Parser},
};

impl<'a> Parser<'a> {
    pub fn can_follow_type_arguments_in_exp(&self) -> bool {
        match self.token.kind {
            Kind::ParenO | Kind::BackQuote | Kind::EOF => true,
            Kind::LessThan | Kind::GreaterThan | Kind::Plus | Kind::Minus => false,
            kind => {
                self.token.is_on_new_line
                    || BinaryOperator::from(kind).is_some()
                    || self.is_start_of_exp()
            }
        }
    }

    pub fn parse_type_parameter_arguments_in_exp(
        &mut self,
    ) -> Option<TsTypeParameterArguments<'a>> {
        let Ok(type_parameter_arguments) = self.parse_type_parameter_arguments() else {
            return None;
        };

        if self.can_follow_type_arguments_in_exp() {
            Some(type_parameter_arguments)
        } else {
            None
        }
    }

    pub fn parse_type_identifier(&mut self) -> TsTypeIdentifier {
        let start = self.token.start;

        // skip an identifier
        self.bump();

        TsTypeIdentifier {
            start,
            end: self.prev_token_end,
        }
    }

    pub fn parse_type_string(&mut self) -> TsTypeAnnotation<'a> {
        TsTypeAnnotation::String(Box::new_in(self.parse_string_literal(), self.arena))
    }

    pub fn parse_type_number(&mut self) -> TsTypeAnnotation<'a> {
        TsTypeAnnotation::Number(Box::new_in(self.parse_numeric_literal(), self.arena))
    }

    pub fn parse_type_boolean(&mut self) -> TsTypeAnnotation<'a> {
        TsTypeAnnotation::Boolean(Box::new_in(self.parse_boolean_literal(), self.arena))
    }

    pub fn parse_type_any(&mut self) -> TsTypeAnnotation<'a> {
        let start = self.token.start;

        // skip any
        self.bump();

        TsTypeAnnotation::Any(Box::new_in(
            TsTypeAny {
                start,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_type_undefined(&mut self) -> TsTypeAnnotation<'a> {
        let start = self.token.start;

        // skip undefined
        self.bump();

        TsTypeAnnotation::Undefined(Box::new_in(
            TsTypeUndefined {
                start,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_type_null(&mut self) -> TsTypeAnnotation<'a> {
        TsTypeAnnotation::Null(Box::new_in(self.parse_null_literal(), self.arena))
    }

    pub fn parse_type_literal(&mut self) -> TsTypeAnnotation<'a> {
        let start = self.token.start;

        // skip string literal, number literal, or boolean literal
        self.bump();

        TsTypeAnnotation::Literal(Box::new_in(
            TsTypeLiteral {
                start,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_ts_property_member(&mut self) -> ParseResult<TsPropertyMember<'a>> {
        let start = self.token.start;

        let (mut readonly_token, mut is_computed, mut is_optional) = (None, false, false);

        let key;

        if self.kind(Kind::Readonly) {
            readonly_token = Some(self.token.clone());
            self.bump();
        }

        if self.kind(Kind::BracketO) {
            is_computed = true;
            self.bump();

            key = self.parse_expression()?;

            self.bump_token(Kind::BracketC)?;
        } else if self.is_curr_token_identifier() {
            key = Expression::Identifier(Box::new_in(self.parse_identifier(), self.arena));
        } else if let Some(token) = readonly_token {
            key = Expression::Identifier(Box::new_in(
                Identifier {
                    start: token.start,
                    parenthesized: false,
                    type_annotation: None,
                    end: token.end,
                },
                self.arena,
            ));

            readonly_token = None;
        } else {
            self.add_diagnostic("invalid token", self.token.range());
            return Err(());
        }

        if self.kind(Kind::Question) {
            is_optional = true;
            self.bump();
        }

        if self.kind(Kind::LessThan) || self.kind(Kind::ParenO) {
            let type_parameters = if self.kind(Kind::LessThan) {
                Some(self.parse_type_parameter_declaration()?)
            } else {
                None
            };

            self.ctx.in_method_type_declaration = true;
            let params = self.parse_function_parameters()?;
            self.ctx.in_method_type_declaration = false;

            let type_annotation = if self.kind(Kind::Colon) {
                self.bump();
                Some(self.parse_type_annotation()?)
            } else {
                None
            };

            Ok(TsPropertyMember::MethodSignature(Box::new_in(
                TsMethodSignature {
                    start,
                    computed: is_computed,
                    optional: is_optional,
                    key,
                    type_annotation,
                    params,
                    type_parameters,
                    end: self.prev_token_end,
                },
                self.arena,
            )))
        } else {
            let type_annotation = if self.kind(Kind::Colon) {
                self.bump();
                Some(self.parse_type_annotation()?)
            } else {
                None
            };

            Ok(TsPropertyMember::PropertySignature(Box::new_in(
                TsPropertySignature {
                    start,
                    key,
                    type_annotation,
                    computed: is_computed,
                    optional: is_optional,
                    readonly: readonly_token.is_some(),
                    end: self.prev_token_end,
                },
                self.arena,
            )))
        }
    }

    pub fn parse_object_literal(&mut self) -> ParseResult<TsTypeAnnotation<'a>> {
        let start = self.token.start;

        // skip {
        self.bump();

        let mut members = Vec::new_in(self.arena);

        while !matches!(self.token.kind, Kind::BracesC | Kind::EOF) {
            let member = self.parse_ts_property_member()?;
            members.push(member);

            if matches!(self.token.kind, Kind::Comma | Kind::Semicolon) {
                self.bump();
            } else if !self.kind(Kind::BracesC) {
                self.assert(!self.token.is_on_new_line, "invalid token")?;
            }
        }

        self.bump_token(Kind::BracesC)?;

        Ok(TsTypeAnnotation::ObjectLiteral(Box::new_in(
            TsTypeObjectLiteral {
                start,
                members,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_type_tuple(&mut self) -> ParseResult<TsTypeAnnotation<'a>> {
        let start = self.token.start;
        let mut elements = Vec::new_in(self.arena);

        // skip [
        self.bump();

        while !matches!(self.token.kind, Kind::BracketC | Kind::EOF) {
            elements.push(self.parse_type_annotation()?);

            if self.kind(Kind::Comma) {
                self.bump();
            }
        }

        self.bump_token(Kind::BracketC)?;

        Ok(TsTypeAnnotation::Tuple(Box::new_in(
            TsTypeTuple {
                start,
                elements,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_type_qualified_name(
        &mut self,
        left: TsTypeIdentifier,
    ) -> ParseResult<TsTypeAnnotation<'a>> {
        let start = left.start;

        // skip .
        self.bump();

        let right = IdentifierOrQualifiedName::Identifier(Box::new_in(
            self.parse_type_identifier(),
            self.arena,
        ));

        let type_parameter_arguments = if self.kind(Kind::LessThan) {
            Some(self.parse_type_parameter_arguments()?)
        } else {
            None
        };

        let mut annotation = TsTypeQualifiedName {
            start,
            left: IdentifierOrQualifiedName::Identifier(Box::new_in(left, self.arena)),
            right,
            type_parameter_arguments,
            end: self.prev_token_end,
        };

        while annotation.type_parameter_arguments.is_none() && self.kind(Kind::Dot) {
            self.bump();

            if !self.is_curr_token_identifier() {
                self.report_unexpected_token(Kind::Identifier);
                return Err(());
            }

            let right = IdentifierOrQualifiedName::Identifier(Box::new_in(
                self.parse_type_identifier(),
                self.arena,
            ));

            let type_parameter_arguments = if self.kind(Kind::LessThan) {
                Some(self.parse_type_parameter_arguments()?)
            } else {
                None
            };

            annotation = TsTypeQualifiedName {
                start: annotation.start,
                left: IdentifierOrQualifiedName::QualifiedName(Box::new_in(annotation, self.arena)),
                right,
                type_parameter_arguments,
                end: self.prev_token_end,
            }
        }

        Ok(TsTypeAnnotation::QualifiedName(Box::new_in(
            annotation, self.arena,
        )))
    }

    pub fn parse_maybe_type_reference(&mut self) -> ParseResult<TsTypeAnnotation<'a>> {
        let start = self.token.start;

        let name = self.parse_type_identifier();
        let mut type_parameter_arguments = None;

        if self.kind(Kind::LessThan) {
            type_parameter_arguments = Some(self.parse_type_parameter_arguments()?);
        } else if self.kind(Kind::Dot) {
            return self.parse_type_qualified_name(name);
        }

        Ok(TsTypeAnnotation::Reference(Box::new_in(
            TsTypeReference {
                start,
                name,
                type_parameter_arguments,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_type_operator(&mut self) -> ParseResult<TsTypeAnnotation<'a>> {
        let start = self.token.start;

        let operator = match self.token.kind {
            Kind::Keyof => TsTypeOperatorKind::Keyof,
            Kind::Typeof => TsTypeOperatorKind::Typeof,
            _ => unsafe { core::hint::unreachable_unchecked() },
        };

        let type_annotation = self.parse_type_annotation()?;

        Ok(TsTypeAnnotation::TypeOperator(Box::new_in(
            TsTypeOperator {
                start,
                operator,
                type_annotation,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_type_parameter_declaration(
        &mut self,
    ) -> ParseResult<TsTypeParameterDeclaration<'a>> {
        let start = self.token.start;

        // skip <
        self.bump();

        let mut params = Vec::new_in(self.arena);

        while !matches!(self.token.kind, Kind::EOF | Kind::GreaterThan) {
            let start = self.token.start;

            if !self.is_curr_token_identifier() {
                self.report_unexpected_token(Kind::Identifier);
                return Err(());
            }

            let name = self.parse_type_identifier();

            let constraint = if self.kind(Kind::Extends) {
                self.bump();
                Some(self.parse_type_annotation()?)
            } else {
                None
            };

            let default = if self.kind(Kind::Equal) {
                self.bump();
                Some(self.parse_type_annotation()?)
            } else {
                None
            };

            params.push(TsTypeParameter {
                start,
                name,
                constraint,
                default,
                end: self.prev_token_end,
            });

            if self.kind(Kind::Comma) {
                self.bump();
            }
        }

        self.bump_token(Kind::GreaterThan)?;

        Ok(TsTypeParameterDeclaration {
            start,
            params,
            end: self.prev_token_end,
        })
    }

    pub fn parse_type_parameter_arguments(&mut self) -> ParseResult<TsTypeParameterArguments<'a>> {
        let start = self.token.start;

        // skip <
        self.bump();

        let mut params = Vec::new_in(self.arena);

        while !matches!(self.token.kind, Kind::EOF | Kind::GreaterThan) {
            params.push(self.parse_type_annotation()?);

            if self.kind(Kind::Comma) {
                self.bump();
            }
        }

        self.bump_token(Kind::GreaterThan)?;

        Ok(TsTypeParameterArguments {
            start,
            params,
            end: self.prev_token_end,
        })
    }

    pub fn parse_type_array_or_indexed_access(
        &mut self,
        initial_annotation: TsTypeAnnotation<'a>,
    ) -> ParseResult<TsTypeAnnotation<'a>> {
        let start = initial_annotation.start();

        // skip [
        self.bump();

        if self.kind(Kind::BracketC) {
            self.bump();

            Ok(TsTypeAnnotation::Array(Box::new_in(
                TsTypeArray {
                    start,
                    element: initial_annotation,
                    end: self.prev_token_end,
                },
                self.arena,
            )))
        } else {
            let indexed_access = TsTypeAnnotation::IndexedAccess(Box::new_in(
                TsIndexedAccess {
                    start,
                    object: initial_annotation,
                    index: self.parse_type_annotation()?,
                    end: self.prev_token_end,
                },
                self.arena,
            ));

            self.bump_token(Kind::BracketC)?;

            Ok(indexed_access)
        }
    }

    pub fn parse_type_parenthesized(&mut self) -> ParseResult<TsTypeAnnotation<'a>> {
        let start = self.token.start;

        // skip (
        self.bump();

        let type_annotation = self.parse_type_annotation()?;

        self.bump_token(Kind::ParenC)?;

        Ok(TsTypeAnnotation::Parenthesized(Box::new_in(
            TsTypeParenthesized {
                start,
                type_annotation,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_type_union(
        &mut self,
        initial_annotation: TsTypeAnnotation<'a>,
    ) -> ParseResult<TsTypeAnnotation<'a>> {
        let start = initial_annotation.start();

        // skip |
        self.bump();

        let mut types = Vec::new_in(self.arena);
        types.push(initial_annotation);

        let mut next_type = self.parse_type()?;

        loop {
            if self.kind(Kind::Pipe) {
                self.bump();
                types.push(next_type);

                next_type = self.parse_type()?;
            } else if self.kind(Kind::Ampersand) {
                next_type = self.parse_type_intersection(next_type)?;
            } else {
                break;
            }
        }

        types.push(next_type);

        Ok(TsTypeAnnotation::Union(Box::new_in(
            TsTypeUnion {
                start,
                types,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_type_intersection(
        &mut self,
        initial_annotation: TsTypeAnnotation<'a>,
    ) -> ParseResult<TsTypeAnnotation<'a>> {
        let start = initial_annotation.start();

        // skip &
        self.bump();

        let mut types = Vec::new_in(self.arena);
        types.push(initial_annotation);

        let mut next_type = self.parse_type()?;

        loop {
            if self.kind(Kind::Ampersand) {
                self.bump();

                types.push(next_type);
                next_type = self.parse_type()?;
            } else if self.kind(Kind::Pipe) {
                next_type = self.parse_type_union(next_type)?;
            } else {
                break;
            }
        }

        types.push(next_type);

        Ok(TsTypeAnnotation::Intersection(Box::new_in(
            TsTypeIntersection {
                start,
                types,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_ts_type_alias_declaration(&mut self) -> ParseResult<Statement<'a>> {
        let start = self.token.start;

        // skip type
        self.bump();

        if !self.is_curr_token_identifier() {
            self.report_unexpected_token(Kind::Identifier);
            return Err(());
        }

        let id = self.parse_type_identifier();

        let type_parameters = if self.kind(Kind::LessThan) {
            Some(self.parse_type_parameter_declaration()?)
        } else {
            None
        };

        self.bump_token(Kind::Equal)?;

        let type_annotation = self.parse_type_annotation()?;

        Ok(Statement::TsTypeAliasDeclaration(Box::new_in(
            TsTypeAliasDeclaration {
                start,
                id,
                type_parameters,
                type_annotation,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_ts_interface_heritage(&mut self) -> ParseResult<TsInterfaceHeritage<'a>> {
        if !self.is_curr_token_identifier() {
            self.report_unexpected_token(Kind::Identifier);
            return Err(());
        }

        let identifier = self.parse_identifier();

        if matches!(
            self.token.kind,
            Kind::QuestionDot | Kind::Dot | Kind::BracketO
        ) {
            let mut exp = Expression::Identifier(Box::new_in(identifier, self.arena));

            loop {
                let (is_optional, is_computed, property);

                if self.kind(Kind::QuestionDot) {
                    self.bump();
                    is_optional = true;
                } else {
                    is_optional = false;
                };

                if !is_optional && self.kind(Kind::Dot) {
                    is_computed = false;

                    self.bump();

                    if !self.is_curr_token_identifier() {
                        self.report_unexpected_token(Kind::Identifier);
                        return Err(());
                    }

                    property =
                        Expression::Identifier(Box::new_in(self.parse_identifier(), self.arena));
                } else if self.kind(Kind::BracketO) {
                    is_computed = true;

                    self.bump();
                    property = self.parse_expression()?;
                    self.bump_token(Kind::BracketC)?;
                } else if is_optional {
                    is_computed = false;

                    if !self.is_curr_token_identifier() {
                        self.report_unexpected_token(Kind::Identifier);
                        return Err(());
                    }

                    property =
                        Expression::Identifier(Box::new_in(self.parse_identifier(), self.arena));
                } else {
                    break;
                }

                exp = Expression::MemberExpression(Box::new_in(
                    MemberExpression {
                        start: exp.start(),
                        object: exp,
                        property,
                        optional: is_optional,
                        computed: is_computed,
                        end: self.prev_token_end,
                    },
                    self.arena,
                ));

                if self.kind(Kind::LessThan) {
                    break;
                }
            }

            let member_exp = match exp {
                Expression::MemberExpression(inner) => Box::into_inner(inner),
                _ => unsafe { core::hint::unreachable_unchecked() },
            };

            let type_parameter_arguments = if self.kind(Kind::LessThan) {
                Some(self.parse_type_parameter_arguments()?)
            } else {
                None
            };

            Ok(TsInterfaceHeritage {
                start: member_exp.start,
                expression: IdentifierOrMemberExpression::MemberExpression(Box::new_in(
                    member_exp, self.arena,
                )),
                type_parameter_arguments,
                end: self.prev_token_end,
            })
        } else {
            let type_parameter_arguments = if self.kind(Kind::LessThan) {
                Some(self.parse_type_parameter_arguments()?)
            } else {
                None
            };

            Ok(TsInterfaceHeritage {
                start: identifier.start,
                expression: IdentifierOrMemberExpression::Identifier(Box::new_in(
                    identifier, self.arena,
                )),
                type_parameter_arguments,
                end: self.prev_token_end,
            })
        }
    }

    pub fn parse_ts_interface_declaration(&mut self) -> ParseResult<Statement<'a>> {
        let start = self.token.start;

        // skip interface
        self.bump();

        if !self.is_curr_token_identifier() {
            self.report_unexpected_token(Kind::Identifier);
            return Err(());
        }

        let id = self.parse_type_identifier();
        let type_parameters = if self.kind(Kind::LessThan) {
            Some(self.parse_type_parameter_declaration()?)
        } else {
            None
        };

        let mut extensions = Vec::new_in(self.arena);

        if self.kind(Kind::Extends) {
            self.bump();

            while !matches!(self.token.kind, Kind::EOF | Kind::BracesO) {
                extensions.push(self.parse_ts_interface_heritage()?);

                if !self.kind(Kind::BracesC) {
                    self.bump_token(Kind::Comma)?;
                }
            }
        }

        let body = match self.parse_object_literal()? {
            TsTypeAnnotation::ObjectLiteral(obj) => Box::into_inner(obj),
            _ => unsafe { core::hint::unreachable_unchecked() },
        };

        Ok(Statement::TsInterfaceDeclaration(Box::new_in(
            TsInterfaceDeclaration {
                start,
                id,
                body,
                type_parameters,
                extends: extensions,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_ts_as_expression(&mut self, exp: Expression<'a>) -> ParseResult<Expression<'a>> {
        // skip as
        self.bump();

        Ok(Expression::TsAsExpression(Box::new_in(
            TsAsExpression {
                start: exp.start(),
                expression: exp,
                type_annotation: self.parse_type_annotation()?,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_ts_satisfies_expression(
        &mut self,
        exp: Expression<'a>,
    ) -> ParseResult<Expression<'a>> {
        // skip satisfies
        self.bump();

        Ok(Expression::TsSatisfiesExpression(Box::new_in(
            TsSatisfiesExpression {
                start: exp.start(),
                expression: exp,
                type_annotation: self.parse_type_annotation()?,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_ts_non_null_expression(&mut self, exp: Expression<'a>) -> Expression<'a> {
        // skip !
        self.bump();

        Expression::TsNonNullExpression(Box::new_in(
            TsNonNullExpression {
                start: exp.start(),
                expression: exp,
                end: self.prev_token_end,
            },
            self.arena,
        ))
    }

    pub fn parse_ts_type_assertion_exp(&mut self) -> ParseResult<Expression<'a>> {
        let start = self.token.start;

        // skip <
        self.bump();

        let type_annotation = self.parse_type_annotation()?;

        self.bump_token(Kind::GreaterThan)?;
        let exp = self.parse_unary_expression()?;

        Ok(Expression::TsTypeAssertionExpression(Box::new_in(
            TsTypeAssertion {
                start,
                expression: exp,
                type_annotation,
                end: self.prev_token_end,
            },
            self.arena,
        )))
    }

    pub fn parse_type(&mut self) -> ParseResult<TsTypeAnnotation<'a>> {
        let mut annotation = match self.token.kind {
            Kind::TypeString => self.parse_type_string(),
            Kind::TypeNumber => self.parse_type_number(),
            Kind::TypeBoolean => self.parse_type_boolean(),
            Kind::Any => self.parse_type_any(),
            Kind::Undefined => self.parse_type_undefined(),
            Kind::Null => self.parse_type_null(),
            Kind::String | Kind::Number | Kind::Boolean => self.parse_type_literal(),
            Kind::BracesO => self.parse_object_literal()?,
            Kind::BracketO => self.parse_type_tuple()?,
            Kind::Keyof | Kind::Typeof => self.parse_type_operator()?,
            Kind::ParenO => self.parse_type_parenthesized()?,
            _ => {
                if self.is_curr_token_identifier() {
                    self.parse_maybe_type_reference()?
                } else {
                    self.add_diagnostic("invalid token", self.token.range());
                    return Err(());
                }
            }
        };

        while self.kind(Kind::BracketO) {
            annotation = self.parse_type_array_or_indexed_access(annotation)?;
        }

        Ok(annotation)
    }

    pub fn parse_type_annotation(&mut self) -> ParseResult<TsTypeAnnotation<'a>> {
        let type_ = self.parse_type()?;

        let annotation = if self.kind(Kind::Pipe) {
            self.parse_type_union(type_)?
        } else if self.kind(Kind::Ampersand) {
            self.parse_type_intersection(type_)?
        } else {
            type_
        };

        Ok(annotation)
    }
}
