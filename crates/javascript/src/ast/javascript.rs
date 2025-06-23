use std::ops::Range;

use super::typescript::{
    TsInstantiationExpression, TsInterfaceDeclaration, TsInterfaceHeritage, TsTypeAliasDeclaration,
    TsTypeAnnotation, TsTypeParameterDeclaration,
};
use crate::{
    allocator::CloneIn,
    ast::{
        jsx::{JSXElement, JSXFragment},
        typescript::{TsAsExpression, TsNonNullExpression, TsSatisfiesExpression, TsTypeAssertion},
    },
    kind::Kind,
};
use bumpalo::{boxed::Box, collections::Vec, Bump};
use macros::{CloneIn, Location};

pub trait Location {
    fn start(&self) -> u32;
    fn end(&self) -> u32;
}

#[derive(Debug, CloneIn)]
pub struct Identifier<'a> {
    pub start: u32,
    pub parenthesized: bool,
    pub type_annotation: Option<TsTypeAnnotation<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum ArrayPatternKind<'a> {
    Pattern(Box<'a, Pattern<'a>>),
    RestElement(Box<'a, RestElement<'a>>),
}

#[derive(Debug, CloneIn)]
pub struct ArrayPattern<'a> {
    pub start: u32,
    pub elements: Vec<'a, Option<ArrayPatternKind<'a>>>,
    pub type_annotation: Option<TsTypeAnnotation<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct ObjectPatternProperty<'a> {
    pub start: u32,
    pub key: Expression<'a>,
    pub value: Pattern<'a>,
    pub shorthand: bool,
    pub computed: bool,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct RestElement<'a> {
    pub start: u32,
    pub argument: Pattern<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum ObjectPatternPropertyKind<'a> {
    Property(Box<'a, ObjectPatternProperty<'a>>),
    RestElement(Box<'a, RestElement<'a>>),
}

#[derive(Debug, CloneIn)]
pub struct ObjectPattern<'a> {
    pub start: u32,
    pub properties: Vec<'a, ObjectPatternPropertyKind<'a>>,
    pub type_annotation: Option<TsTypeAnnotation<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct AssignmentPattern<'a> {
    pub start: u32,
    pub left: Pattern<'a>,
    pub right: Expression<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum Pattern<'a> {
    Identifier(Box<'a, Identifier<'a>>),
    ArrayPattern(Box<'a, ArrayPattern<'a>>),
    ObjectPattern(Box<'a, ObjectPattern<'a>>),
    AssignmentPattern(Box<'a, AssignmentPattern<'a>>),
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, CloneIn)]
pub enum VariableDeclarationKind {
    Const,
    Let,
    Var,
}

impl<'a> VariableDeclarationKind {
    pub fn as_str(&self) -> &'a str {
        match self {
            Self::Const => "const",
            Self::Let => "let",
            Self::Var => "var",
        }
    }
}

#[derive(Debug, CloneIn)]
pub struct VariableDeclarator<'a> {
    pub start: u32,
    pub id: Pattern<'a>,
    pub init: Option<Expression<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct VariableDeclaration<'a> {
    pub kind: VariableDeclarationKind,
    pub start: u32,
    pub end: u32,
    pub declarations: Vec<'a, VariableDeclarator<'a>>,
}

#[derive(Debug, CloneIn)]
pub struct NumericLiteral {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct StringLiteral {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct BooleanLiteral {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct NullLiteral {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum IdentifierOrLiteral<'a> {
    Identifier(Box<'a, Identifier<'a>>),
    Literal(Box<'a, StringLiteral>),
}

#[derive(Debug, CloneIn)]
pub struct Import {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum ArrayElement<'a> {
    Elision(Box<'a, Elision>),
    SpreadElement(Box<'a, SpreadElement<'a>>),
    Expression(Box<'a, Expression<'a>>),
}

#[derive(Debug, CloneIn)]
pub struct ArrayExpression<'a> {
    pub start: u32,
    pub elements: Vec<'a, ArrayElement<'a>>,
    pub end: u32,
    pub parenthesized: bool,
}

#[derive(Debug, CloneIn)]
pub struct ObjectExpressionProperty<'a> {
    pub start: u32,
    pub computed: bool,
    pub shorthand: bool,
    pub key: Expression<'a>,
    pub value: Expression<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum ObjectExpressionMethodKind {
    Method,
    Get,
    Set,
}

#[derive(Debug, CloneIn)]
pub struct ObjectExpressionMethod<'a> {
    pub start: u32,
    pub async_: bool,
    pub generator: bool,
    pub computed: bool,
    pub id: Expression<'a>,
    pub params: Vec<'a, FunctionParam<'a>>,
    pub body: Block<'a>,
    pub type_parameters: Option<TsTypeParameterDeclaration<'a>>,
    pub kind: ObjectExpressionMethodKind,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum ObjectExpressionPropertyKind<'a> {
    Property(Box<'a, ObjectExpressionProperty<'a>>),
    Method(Box<'a, ObjectExpressionMethod<'a>>),
    SpreadElement(Box<'a, SpreadElement<'a>>),
}

#[derive(Debug, CloneIn)]
pub struct ObjectExpression<'a> {
    pub start: u32,
    pub properties: Vec<'a, ObjectExpressionPropertyKind<'a>>,
    pub end: u32,
    pub parenthesized: bool,
}

#[derive(Debug, CloneIn, PartialEq)]
pub enum BinaryOperator {
    Equality,
    Inequality,
    StrictInequality,
    StrictEquality,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LeftShift,
    RightShift,
    UnsignedRightShift,
    Add,
    Subtract,
    Multiply,
    Exponentiation,
    Divide,
    Mod,
    BitwiseAND,
    BitwiseOR,
    BitwiseXOR,
    And,
    Or,
    Coalescing,
    In,
    Instanceof,
    As,
    Satisfies,
}

impl<'a> BinaryOperator {
    pub fn as_str(&self) -> &'a str {
        match self {
            Self::Equality => "==",
            Self::Inequality => "!=",
            Self::StrictEquality => "===",
            Self::StrictInequality => "!==",
            Self::LessThan => "<",
            Self::LessThanOrEqual => "<=",
            Self::GreaterThan => ">",
            Self::GreaterThanOrEqual => ">=",
            Self::LeftShift => "<<",
            Self::RightShift => ">>",
            Self::UnsignedRightShift => ">>>",
            Self::Add => "+",
            Self::Subtract => "-",
            Self::Multiply => "*",
            Self::Exponentiation => "**",
            Self::Divide => "/",
            Self::Mod => "%",
            Self::BitwiseAND => "&",
            Self::BitwiseOR => "|",
            Self::BitwiseXOR => "^",
            Self::Coalescing => "??",
            Self::In => "in",
            Self::Instanceof => "instanceof",
            Self::As => "as",
            Self::Satisfies => "satisfies",
            Self::Or => "||",
            Self::And => "&&",
        }
    }

    pub fn precedence(&self) -> u8 {
        match self {
            Self::Coalescing => 1,
            Self::Or => 2,
            Self::And => 3,
            Self::BitwiseOR => 4,
            Self::BitwiseXOR => 5,
            Self::BitwiseAND => 6,
            Self::Equality | Self::Inequality | Self::StrictEquality | Self::StrictInequality => 7,
            Self::LessThan
            | Self::LessThanOrEqual
            | Self::GreaterThan
            | Self::GreaterThanOrEqual
            | Self::Instanceof
            | Self::In
            | Self::As
            | Self::Satisfies => 8,
            Self::LeftShift | Self::RightShift | Self::UnsignedRightShift => 9,
            Self::Add | Self::Subtract => 10,
            Self::Multiply | Self::Divide | Self::Mod => 11,
            Self::Exponentiation => 12,
        }
    }

    pub fn from(kind: Kind) -> Option<Self> {
        Some(match kind {
            Kind::Question2 => BinaryOperator::Coalescing,
            Kind::Pipe2 => BinaryOperator::Or,
            Kind::Ampersand2 => BinaryOperator::And,
            Kind::Pipe => BinaryOperator::BitwiseOR,
            Kind::Caret => BinaryOperator::BitwiseXOR,
            Kind::Ampersand => BinaryOperator::BitwiseAND,
            Kind::Equal2 => BinaryOperator::Equality,
            Kind::NotEqual => BinaryOperator::Inequality,
            Kind::Equal3 => BinaryOperator::StrictEquality,
            Kind::NotEqual2 => BinaryOperator::StrictInequality,
            Kind::LessThan => BinaryOperator::LessThan,
            Kind::LessThanOrEqual => BinaryOperator::LessThanOrEqual,
            Kind::GreaterThan => BinaryOperator::GreaterThan,
            Kind::GreaterThanOrEqual => BinaryOperator::GreaterThanOrEqual,
            Kind::Instanceof => BinaryOperator::Instanceof,
            Kind::In => BinaryOperator::In,
            Kind::As => BinaryOperator::As,
            Kind::Satisfies => BinaryOperator::Satisfies,
            Kind::LeftShift => BinaryOperator::LeftShift,
            Kind::RightShift => BinaryOperator::RightShift,
            Kind::UnsignedRightShift => BinaryOperator::UnsignedRightShift,
            Kind::Plus => BinaryOperator::Add,
            Kind::Minus => BinaryOperator::Subtract,
            Kind::Star => BinaryOperator::Multiply,
            Kind::Slash => BinaryOperator::Divide,
            Kind::Mod => BinaryOperator::Mod,
            Kind::Star2 => BinaryOperator::Exponentiation,
            _ => return None,
        })
    }
}

#[derive(Debug, CloneIn)]
pub struct BinaryExpression<'a> {
    pub start: u32,
    pub left: Expression<'a>,
    pub operator: BinaryOperator,
    pub right: Expression<'a>,
    pub end: u32,
    pub parenthesized: bool,
}

#[derive(Debug, CloneIn)]
pub enum AssignmentExpressionLHS<'a> {
    Pattern(Box<'a, Pattern<'a>>),
    Expression(Box<'a, Expression<'a>>),
}

#[derive(Debug, CloneIn, PartialEq, Eq)]
pub enum AssignmentOperator {
    Equal,
    Plus,
    Minus,
    Multiply,
    Divide,
    Mod,
    Exponentiation,
    LeftShift,
    RightShift,
    UnsignedRightShift,
    BitwiseOR,
    BitwiseAND,
    BitwiseXOR,
}

impl<'a> AssignmentOperator {
    pub fn as_str(&self) -> &'a str {
        use AssignmentOperator::*;

        match self {
            Equal => "=",
            Plus => "+=",
            Minus => "-=",
            Multiply => "*=",
            Divide => "/=",
            Mod => "%=",
            Exponentiation => "**=",
            LeftShift => "<<=",
            RightShift => ">>=",
            UnsignedRightShift => ">>>=",
            BitwiseOR => "|=",
            BitwiseAND => "&=",
            BitwiseXOR => "^=",
        }
    }

    pub fn from(kind: Kind) -> Option<Self> {
        Some(match kind {
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
        })
    }
}

#[derive(Debug, CloneIn)]
pub struct AssignmentExpression<'a> {
    pub start: u32,
    pub left: AssignmentExpressionLHS<'a>,
    pub operator: AssignmentOperator,
    pub right: Expression<'a>,
    pub end: u32,
    pub parenthesized: bool,
}

#[derive(Debug, CloneIn)]
pub struct SpreadElement<'a> {
    pub start: u32,
    pub argument: Expression<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct AwaitExpression<'a> {
    pub start: u32,
    pub argument: Expression<'a>,
    pub end: u32,
    pub parenthesized: bool,
}

#[derive(Debug, CloneIn)]
pub struct Elision {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct ThisExpression {
    pub start: u32,
    pub end: u32,
    pub parenthesized: bool,
}

#[derive(Debug, CloneIn)]
pub struct NewExpression<'a> {
    pub start: u32,
    pub callee: Expression<'a>,
    pub arguments: Vec<'a, FunctionArgument<'a>>,
    pub end: u32,
    pub parenthesized: bool,
}

#[derive(Debug, CloneIn)]
pub enum UpdateOperator {
    Increment,
    Decrement,
}

impl<'a> UpdateOperator {
    pub fn as_str(&self) -> &'a str {
        match self {
            Self::Increment => "++",
            Self::Decrement => "--",
        }
    }
}

#[derive(Debug, CloneIn)]
pub struct UpdateExpression<'a> {
    pub start: u32,
    pub operator: UpdateOperator,
    pub argument: Expression<'a>,
    pub prefix: bool,
    pub end: u32,
    pub parenthesized: bool,
}

#[derive(Debug, CloneIn)]
pub enum UnaryOperator {
    Plus,
    Minus,
    Tilde,
    Bang,
    Delete,
    Typeof,
    Void,
}

impl<'a> UnaryOperator {
    pub fn as_str(&self) -> &'a str {
        use UnaryOperator::*;

        match self {
            Plus => "+",
            Minus => "-",
            Tilde => "~",
            Bang => "!",
            Delete => "delete",
            Typeof => "typeof",
            Void => "void",
        }
    }

    pub fn from(kind: Kind) -> Option<Self> {
        Some(match kind {
            Kind::Plus => UnaryOperator::Plus,
            Kind::Minus => UnaryOperator::Minus,
            Kind::Tilde => UnaryOperator::Tilde,
            Kind::Bang => UnaryOperator::Bang,
            Kind::Typeof => UnaryOperator::Typeof,
            Kind::Void => UnaryOperator::Void,
            Kind::Delete => UnaryOperator::Delete,
            _ => return None,
        })
    }
}

#[derive(Debug, CloneIn)]
pub struct UnaryExpression<'a> {
    pub start: u32,
    pub operator: UnaryOperator,
    pub argument: Expression<'a>,
    pub prefix: bool,
    pub end: u32,
    pub parenthesized: bool,
}

#[derive(Debug, CloneIn)]
pub struct ConditionalExpression<'a> {
    pub start: u32,
    pub test: Expression<'a>,
    pub alternate: Expression<'a>,
    pub consequent: Expression<'a>,
    pub end: u32,
    pub parenthesized: bool,
}

#[derive(Debug, CloneIn)]
pub struct SequenceExpression<'a> {
    pub start: u32,
    pub expressions: Vec<'a, Expression<'a>>,
    pub end: u32,
    pub parenthesized: bool,
}

#[derive(Debug, CloneIn)]
pub struct Regexp<'a> {
    pub start: u32,
    pub pattern: Range<u32>,
    pub flag: &'a str,
    pub end: u32,
    pub parenthesized: bool,
}

#[derive(Debug, CloneIn)]
pub struct TemplateElement {
    pub start: u32,
    pub tail: bool,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TemplateLiteral<'a> {
    pub start: u32,
    pub quasis: Vec<'a, TemplateElement>,
    pub expressions: Vec<'a, Expression<'a>>,
    pub end: u32,
    pub parenthesized: bool,
}

#[derive(Debug, CloneIn)]
pub struct TaggedTemplateLiteral<'a> {
    pub start: u32,
    pub tag: Expression<'a>,
    pub quasi: TemplateLiteral<'a>,
    pub end: u32,
}

#[derive(Debug, Location, CloneIn)]
pub enum Expression<'a> {
    Identifier(Box<'a, Identifier<'a>>),
    NumericLiteral(Box<'a, NumericLiteral>),
    StringLiteral(Box<'a, StringLiteral>),
    BooleanLiteral(Box<'a, BooleanLiteral>),
    NullLiteral(Box<'a, NullLiteral>),
    Import(Box<'a, Import>),
    ArrayExpression(Box<'a, ArrayExpression<'a>>),
    ObjectExpression(Box<'a, ObjectExpression<'a>>),
    BinaryExpression(Box<'a, BinaryExpression<'a>>),
    AssignmentExpression(Box<'a, AssignmentExpression<'a>>),
    UpdateExpression(Box<'a, UpdateExpression<'a>>),
    UnaryExpression(Box<'a, UnaryExpression<'a>>),
    ConditionalExpression(Box<'a, ConditionalExpression<'a>>),
    ThisExpression(Box<'a, ThisExpression>),
    NewExpression(Box<'a, NewExpression<'a>>),
    FunctionExpression(Box<'a, Function<'a>>),
    ArrowFunctionExpression(Box<'a, ArrowFunction<'a>>),
    ClassExpression(Box<'a, Class<'a>>),
    CallExpression(Box<'a, CallExpression<'a>>),
    MemberExpression(Box<'a, MemberExpression<'a>>),
    SequenceExpression(Box<'a, SequenceExpression<'a>>),
    RegularExpression(Box<'a, Regexp<'a>>),
    TemplateLiteral(Box<'a, TemplateLiteral<'a>>),
    TaggedTemplateLiteral(Box<'a, TaggedTemplateLiteral<'a>>),
    AwaitExpression(Box<'a, AwaitExpression<'a>>),
    MetaProperty(Box<'a, MetaProperty<'a>>),

    // Typescript specific expressions
    TsInstantiationExpression(Box<'a, TsInstantiationExpression<'a>>),
    TsAsExpression(Box<'a, TsAsExpression<'a>>),
    TsSatisfiesExpression(Box<'a, TsSatisfiesExpression<'a>>),
    TsNonNullExpression(Box<'a, TsNonNullExpression<'a>>),
    TsTypeAssertionExpression(Box<'a, TsTypeAssertion<'a>>),

    // JSX specific expressions
    JSXElement(Box<'a, JSXElement<'a>>),
    JSXFragment(Box<'a, JSXFragment<'a>>),
}

#[derive(Debug, CloneIn)]
pub struct ExpressionStatement<'a> {
    pub start: u32,
    pub end: u32,
    pub exp: Expression<'a>,
}

#[derive(Debug, CloneIn)]
pub struct Block<'a> {
    pub start: u32,
    pub body: Vec<'a, Statement<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum FunctionArgument<'a> {
    Expression(Box<'a, Expression<'a>>),
    SpreadElement(Box<'a, SpreadElement<'a>>),
}

#[derive(Debug, CloneIn)]
pub enum FunctionParam<'a> {
    Pattern(Box<'a, Pattern<'a>>),
    RestElement(Box<'a, RestElement<'a>>),
}

#[derive(Debug, CloneIn)]
pub struct Function<'a> {
    pub start: u32,
    pub async_: bool,
    pub generator: bool,
    pub id: Option<Identifier<'a>>,
    pub params: Vec<'a, FunctionParam<'a>>,
    pub body: Block<'a>,
    pub type_parameters: Option<TsTypeParameterDeclaration<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum ArrowFunctionBody<'a> {
    Expression(Box<'a, Expression<'a>>),
    Block(Box<'a, Block<'a>>),
}

#[derive(Debug, CloneIn)]
pub struct ArrowFunction<'a> {
    pub start: u32,
    pub async_: bool,
    pub params: Vec<'a, FunctionParam<'a>>,
    pub type_parameters: Option<TsTypeParameterDeclaration<'a>>,
    pub body: ArrowFunctionBody<'a>,
    pub expression: bool,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum MethodDefinitionKind {
    Constructor,
    Method,
    Get,
    Set,
}

#[derive(Debug, CloneIn)]
pub struct ClassMethod<'a> {
    pub start: u32,
    pub static_: bool,
    pub computed: bool,
    pub generator: bool,
    pub async_: bool,
    pub key: Expression<'a>,
    pub params: Vec<'a, FunctionParam<'a>>,
    pub body: Block<'a>,
    pub kind: MethodDefinitionKind,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct ClassProperty<'a> {
    pub start: u32,
    pub key: Expression<'a>,
    pub value: Option<Expression<'a>>,
    pub computed: bool,
    pub static_: bool,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum ClassBody<'a> {
    MethodDefinition(Box<'a, ClassMethod<'a>>),
    PropertyDefinition(Box<'a, ClassProperty<'a>>),
}

#[derive(Debug, CloneIn)]
pub struct Class<'a> {
    pub start: u32,
    pub id: Option<Identifier<'a>>,
    pub super_class: Option<Expression<'a>>,
    pub body: Vec<'a, ClassBody<'a>>,
    pub type_parameters: Option<TsTypeParameterDeclaration<'a>>,
    pub implements: Vec<'a, TsInterfaceHeritage<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct ImportSpecifier<'a> {
    pub start: u32,
    pub imported: IdentifierOrLiteral<'a>,
    pub local: Option<Identifier<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct ImportDefaultSpecifier<'a> {
    pub start: u32,
    pub local: Identifier<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct ImportNamespaceSpecifier<'a> {
    pub start: u32,
    pub local: Identifier<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum ImportSpecifierType<'a> {
    ImportSpecifier(Box<'a, ImportSpecifier<'a>>),
    ImportDefaultSpecifier(Box<'a, ImportDefaultSpecifier<'a>>),
    ImportNamespaceSpecifier(Box<'a, ImportNamespaceSpecifier<'a>>),
}

#[derive(Debug, CloneIn)]
pub struct ImportAttribute<'a> {
    pub start: u32,
    pub key: IdentifierOrLiteral<'a>,
    pub value: StringLiteral,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct ImportDeclaration<'a> {
    pub start: u32,
    pub specifiers: Vec<'a, ImportSpecifierType<'a>>,
    pub source: StringLiteral,
    pub assertions: Vec<'a, ImportAttribute<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum Declaration<'a> {
    Variable(Box<'a, VariableDeclaration<'a>>),
    Function(Box<'a, Function<'a>>),
    Class(Box<'a, Class<'a>>),
}

#[derive(Debug, CloneIn)]
pub struct ExportSpecifier<'a> {
    pub start: u32,
    pub local: IdentifierOrLiteral<'a>,
    pub exported: IdentifierOrLiteral<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct ExportNamedDeclaration<'a> {
    pub start: u32,
    pub declaration: Option<Declaration<'a>>,
    pub specifiers: Vec<'a, ExportSpecifier<'a>>,
    pub source: Option<StringLiteral>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct ExportDefaultDeclaration<'a> {
    pub start: u32,
    pub declaration: Declaration<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct ExportAllDeclaration<'a> {
    pub start: u32,
    pub source: StringLiteral,
    pub exported: Option<IdentifierOrLiteral<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct MetaProperty<'a> {
    pub start: u32,
    pub meta: Identifier<'a>,
    pub property: Identifier<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct CallExpression<'a> {
    pub start: u32,
    pub callee: Expression<'a>,
    pub arguments: Vec<'a, FunctionArgument<'a>>,
    pub optional: bool,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct MemberExpression<'a> {
    pub start: u32,
    pub object: Expression<'a>,
    pub property: Expression<'a>,
    pub computed: bool,
    pub optional: bool,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct EmptyStatement {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct BreakStatement<'a> {
    pub start: u32,
    pub label: Option<Identifier<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct ContinueStatement<'a> {
    pub start: u32,
    pub label: Option<Identifier<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct DebuggerStatement {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct DoWhileLoop<'a> {
    pub start: u32,
    pub test: Expression<'a>,
    pub body: Statement<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct WhileLoop<'a> {
    pub start: u32,
    pub test: Expression<'a>,
    pub body: Statement<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum ForLoopInit<'a> {
    VariableDeclaration(Box<'a, VariableDeclaration<'a>>),
    Expression(Box<'a, Expression<'a>>),
}

#[derive(Debug, CloneIn)]
pub struct ForLoop<'a> {
    pub start: u32,
    pub init: Option<ForLoopInit<'a>>,
    pub test: Option<Expression<'a>>,
    pub update: Option<Expression<'a>>,
    pub body: Statement<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum ForInLoopLeft<'a> {
    VariableDeclaration(Box<'a, VariableDeclaration<'a>>),
    Pattern(Box<'a, Pattern<'a>>),
}

#[derive(Debug, CloneIn)]
pub struct ForInLoop<'a> {
    pub start: u32,
    pub left: ForInLoopLeft<'a>,
    pub right: Expression<'a>,
    pub body: Statement<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct IfStatement<'a> {
    pub start: u32,
    pub test: Expression<'a>,
    pub consequent: Statement<'a>,
    pub alternate: Option<Statement<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct ReturnStatement<'a> {
    pub start: u32,
    pub argument: Option<Expression<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct SwitchCase<'a> {
    pub start: u32,
    pub test: Option<Expression<'a>>,
    pub consequent: Vec<'a, Statement<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct SwitchStatement<'a> {
    pub start: u32,
    pub discriminant: Expression<'a>,
    pub cases: Vec<'a, SwitchCase<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct ThrowStatement<'a> {
    pub start: u32,
    pub argument: Expression<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct CatchClause<'a> {
    pub start: u32,
    pub param: Pattern<'a>,
    pub body: Block<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct TryStatement<'a> {
    pub start: u32,
    pub block: Block<'a>,
    pub handler: Option<CatchClause<'a>>,
    pub finalizer: Option<Block<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct WithStatement<'a> {
    pub start: u32,
    pub object: Expression<'a>,
    pub body: Statement<'a>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct LabelledStatement<'a> {
    pub start: u32,
    pub label: Identifier<'a>,
    pub body: Statement<'a>,
    pub end: u32,
}

#[derive(Debug, Location, CloneIn)]
pub enum Statement<'a> {
    // Declaration
    VariableDeclaration(Box<'a, VariableDeclaration<'a>>),
    FunctionDeclaration(Box<'a, Function<'a>>),
    ClassDeclaration(Box<'a, Class<'a>>),
    ImportDeclaration(Box<'a, ImportDeclaration<'a>>),
    ExportNamedDeclaration(Box<'a, ExportNamedDeclaration<'a>>),
    ExportDefaultDeclaration(Box<'a, ExportDefaultDeclaration<'a>>),
    ExportAllDeclaration(Box<'a, ExportAllDeclaration<'a>>),
    TsTypeAliasDeclaration(Box<'a, TsTypeAliasDeclaration<'a>>),
    TsInterfaceDeclaration(Box<'a, TsInterfaceDeclaration<'a>>),

    // Statements
    ExpressionStatement(Box<'a, ExpressionStatement<'a>>),
    BlockStatement(Box<'a, Block<'a>>),
    EmptyStatement(Box<'a, EmptyStatement>),
    BreakStatement(Box<'a, BreakStatement<'a>>),
    ContinueStatement(Box<'a, ContinueStatement<'a>>),
    DebuggerStatement(Box<'a, DebuggerStatement>),
    DoWhileLoop(Box<'a, DoWhileLoop<'a>>),
    WhileLoop(Box<'a, WhileLoop<'a>>),
    ForLoop(Box<'a, ForLoop<'a>>),
    ForInLoop(Box<'a, ForInLoop<'a>>),
    IfStatement(Box<'a, IfStatement<'a>>),
    ReturnStatement(Box<'a, ReturnStatement<'a>>),
    SwitchStatement(Box<'a, SwitchStatement<'a>>),
    ThrowStatement(Box<'a, ThrowStatement<'a>>),
    TryStatement(Box<'a, TryStatement<'a>>),
    WithStatement(Box<'a, WithStatement<'a>>),
    LabelledStatement(Box<'a, LabelledStatement<'a>>),
}
