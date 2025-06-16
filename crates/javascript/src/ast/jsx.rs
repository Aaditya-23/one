use bumpalo::{boxed::Box, collections::Vec as ArenaVec, Bump};
use macros::{CloneIn, Location};

use crate::allocator::CloneIn;
use crate::ast::{
    javascript::{Expression, StringLiteral},
    typescript::TsTypeParameterArguments,
};

#[derive(Debug, CloneIn)]

pub struct JSXIdentifier {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, CloneIn)]

pub struct JSXMemberExpression<'a> {
    pub start: u32,
    pub object: JSXTagName<'a>,
    pub property: JSXIdentifier,
    pub end: u32,
}

#[derive(Debug, CloneIn)]

pub struct JSXExpressionContainer<'a> {
    pub start: u32,
    pub expression: Expression<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct JSXSpreadChild<'a> {
    pub start: u32,
    pub expression: Expression<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]

pub enum JSXAttributeValue<'a> {
    StringLiteral(Box<'a, StringLiteral>),
    ExpressionContainer(Box<'a, JSXExpressionContainer<'a>>),
    SpreadChild(Box<'a, JSXSpreadChild<'a>>),
}

#[derive(Debug, CloneIn)]
pub struct JSXNameSpacedName {
    pub start: u32,
    pub namespace: JSXIdentifier,
    pub name: JSXIdentifier,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum JSXAttributeName<'a> {
    Identifier(Box<'a, JSXIdentifier>),
    NamespacedName(Box<'a, JSXNameSpacedName>),
}

#[derive(Debug, CloneIn)]
pub struct JSXAttribute<'a> {
    pub start: u32,
    pub name: JSXAttributeName<'a>,
    pub value: Option<JSXAttributeValue<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct JSXSpreadAttribute<'a> {
    pub start: u32,
    pub argument:  Expression<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum Attribute<'a> {
    Attribute(Box<'a, JSXAttribute<'a>>),
    SpreadAttribute(Box<'a, JSXSpreadAttribute<'a>>),
}

#[derive(Debug, CloneIn)]
pub enum JSXTagName<'a> {
    Identifier(Box<'a, JSXIdentifier>),
    MemberExpression(Box<'a, JSXMemberExpression<'a>>),
    NamespacedName(Box<'a, JSXNameSpacedName>),
}

#[derive(Debug, CloneIn)]
pub struct JSXOpeningElement<'a> {
    pub start: u32,
    pub name: JSXTagName<'a>,
    pub self_closing: bool,
    pub attributes: ArenaVec<'a, Attribute<'a>>,
    pub type_parameter_arguments: Option<TsTypeParameterArguments<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct JSXText {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum JSXChild<'a> {
    Text(Box<'a, JSXText>),
    Element(Box<'a, JSXElement<'a>>),
    Fragment(Box<'a, JSXFragment<'a>>),
    ExpressionContainer(Box<'a, JSXExpressionContainer<'a>>),
    SpreadChild(Box<'a, JSXSpreadChild<'a>>),
}

#[derive(Debug, CloneIn)]
pub struct JSXClosingElement<'a> {
    pub start: u32,
    pub name: JSXTagName<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct JSXElement<'a> {
    pub start: u32,
    pub opening_element: JSXOpeningElement<'a>,
    pub children: ArenaVec<'a, JSXChild<'a>>,
    pub closing_element: Option<JSXClosingElement<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct JSXOpeningFragment {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct JSXClosingFragment {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct JSXFragment<'a> {
    pub start: u32,
    pub opening_element: JSXOpeningFragment,
    pub children: ArenaVec<'a, JSXChild<'a>>,
    pub closing_element: JSXClosingFragment,
    pub end: u32,
}
