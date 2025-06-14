use crate::allocator::CloneIn;
use bumpalo::{boxed::Box, collections::Vec, Bump};
use macros::{CloneIn, Location};

use super::javascript::{Expression, FunctionParams, Identifier, Location, MemberExpression};

#[derive(Debug, CloneIn)]
pub struct TsTypeIdentifier {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsTypeString {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsTypeNumber {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsTypeBoolean {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsTypeAny {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsTypeUndefined {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsTypeNull {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsTypeLiteral {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsPropertySignature<'a> {
    pub start: u32,
    pub key: Expression<'a>,
    pub type_annotation: Option<TsTypeAnnotation<'a>>,
    pub computed: bool,
    pub optional: bool,
    pub readonly: bool,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsMethodSignature<'a> {
    pub start: u32,
    pub key: Expression<'a>,
    pub type_annotation: Option<TsTypeAnnotation<'a>>,
    pub params: Vec<'a, FunctionParams<'a>>,
    pub type_parameters: Option<TsTypeParameterDeclaration<'a>>,
    pub computed: bool,
    pub optional: bool,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum TsPropertyMember<'a> {
    PropertySignature(Box<'a, TsPropertySignature<'a>>),
    MethodSignature(Box<'a, TsMethodSignature<'a>>),
}

#[derive(Debug, CloneIn)]
pub struct TsTypeObjectLiteral<'a> {
    pub start: u32,
    pub members: Vec<'a, TsPropertyMember<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsTypeArray<'a> {
    pub start: u32,
    pub element: TsTypeAnnotation<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsIndexedAccess<'a> {
    pub start: u32,
    pub object: TsTypeAnnotation<'a>,
    pub index: TsTypeAnnotation<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsTypeReference<'a> {
    pub start: u32,
    pub name: TsTypeIdentifier,
    pub type_parameter_arguments: Option<TsTypeParameterArguments<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum TsTypeOperatorKind {
    Typeof,
    Keyof,
}

#[derive(Debug, CloneIn)]
pub struct TsTypeOperator<'a> {
    pub start: u32,
    pub operator: TsTypeOperatorKind,
    pub type_annotation: TsTypeAnnotation<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsTypeUnion<'a> {
    pub start: u32,
    pub types: Vec<'a, TsTypeAnnotation<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsTypeIntersection<'a> {
    pub start: u32,
    pub types: Vec<'a, TsTypeAnnotation<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsTypeTuple<'a> {
    pub start: u32,
    pub elements: Vec<'a, TsTypeAnnotation<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn, Location)]
pub enum IdentifierOrQualifiedName<'a> {
    Identifier(Box<'a, TsTypeIdentifier>),
    QualifiedName(Box<'a, TsTypeQualifiedName<'a>>),
}

#[derive(Debug, CloneIn)]
pub struct TsTypeQualifiedName<'a> {
    pub start: u32,
    pub left: IdentifierOrQualifiedName<'a>,
    pub right: IdentifierOrQualifiedName<'a>,
    pub type_parameter_arguments: Option<TsTypeParameterArguments<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsTypeParameter<'a> {
    pub start: u32,
    pub name: TsTypeIdentifier,
    pub constraint: Option<TsTypeAnnotation<'a>>,
    pub default: Option<TsTypeAnnotation<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsTypeParameterDeclaration<'a> {
    pub start: u32,
    pub params: Vec<'a, TsTypeParameter<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsTypeParameterArguments<'a> {
    pub start: u32,
    pub params: Vec<'a, TsTypeAnnotation<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsTypeParenthesized<'a> {
    pub start: u32,
    pub type_annotation: TsTypeAnnotation<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn, Location)]
pub enum TsTypeAnnotation<'a> {
    String(Box<'a, TsTypeString>),
    Number(Box<'a, TsTypeNumber>),
    Boolean(Box<'a, TsTypeBoolean>),
    Any(Box<'a, TsTypeAny>),
    Undefined(Box<'a, TsTypeUndefined>),
    Null(Box<'a, TsTypeNull>),
    Literal(Box<'a, TsTypeLiteral>),
    ObjectLiteral(Box<'a, TsTypeObjectLiteral<'a>>),
    Array(Box<'a, TsTypeArray<'a>>),
    IndexedAccess(Box<'a, TsIndexedAccess<'a>>),
    Reference(Box<'a, TsTypeReference<'a>>),
    TypeOperator(Box<'a, TsTypeOperator<'a>>),
    Union(Box<'a, TsTypeUnion<'a>>),
    Intersection(Box<'a, TsTypeIntersection<'a>>),
    Tuple(Box<'a, TsTypeTuple<'a>>),
    QualifiedName(Box<'a, TsTypeQualifiedName<'a>>),
    Parenthesized(Box<'a, TsTypeParenthesized<'a>>),
}

#[derive(Debug, CloneIn)]
pub struct TsTypeAliasDeclaration<'a> {
    pub start: u32,
    pub id: TsTypeIdentifier,
    pub type_parameters: Option<TsTypeParameterDeclaration<'a>>,
    pub type_annotation: TsTypeAnnotation<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn, Location)]
pub enum IdentifierOrMemberExpression<'a> {
    Identifier(Box<'a, Identifier<'a>>),
    MemberExpression(Box<'a, MemberExpression<'a>>),
}

#[derive(Debug, CloneIn)]
pub struct TsInterfaceHeritage<'a> {
    pub start: u32,
    pub expression: IdentifierOrMemberExpression<'a>,
    pub type_parameter_arguments: Option<TsTypeParameterArguments<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsInterfaceDeclaration<'a> {
    pub start: u32,
    pub id: TsTypeIdentifier,
    pub body: TsTypeObjectLiteral<'a>,
    pub type_parameters: Option<TsTypeParameterDeclaration<'a>>,
    pub extends: Vec<'a, TsInterfaceHeritage<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsInstantiationExpression<'a> {
    pub start: u32,
    pub expression: Expression<'a>,
    pub type_parameter_arguments: TsTypeParameterArguments<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsAsExpression<'a> {
    pub start: u32,
    pub expression: Expression<'a>,
    pub type_annotation: TsTypeAnnotation<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsSatisfiesExpression<'a> {
    pub start: u32,
    pub expression: Expression<'a>,
    pub type_annotation: TsTypeAnnotation<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TsNonNullExpression<'a> {
    pub start: u32,
    pub expression: Expression<'a>,
    pub end: u32,
}
