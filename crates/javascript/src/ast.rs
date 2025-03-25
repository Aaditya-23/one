use crate::allocator::CloneIn;
use bumpalo::{boxed::Box, collections::Vec, Bump};
use macros::{CloneIn, Location};

pub trait Location {
    fn start(&self) -> u32;
    fn end(&self) -> u32;
}

#[derive(Debug, CloneIn)]
pub struct Identifier {
    pub start: u32,
    pub end: u32,
    pub parenthesized: bool,
}

#[derive(Debug, CloneIn)]
pub struct ArrayPattern<'a> {
    pub start: u32,
    pub elements: Vec<'a, Pattern<'a>>,
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
    IdentifierPattern(Box<'a, Identifier>),
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
    pub end: u32,
    pub id: Pattern<'a>,
    pub init: Option<Expression<'a>>,
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
    Identifier(Box<'a, Identifier>),
    Literal(Box<'a, StringLiteral>),
}

#[derive(Debug, CloneIn)]
pub struct Import {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct ArrayExpression<'a> {
    pub start: u32,
    pub elements: Vec<'a, Expression<'a>>,
    pub end: u32,
    pub parenthesized: bool,
}

#[derive(Debug, CloneIn)]
pub enum PropertyKind {
    Init,
    Get,
    Set,
}

#[derive(Debug, CloneIn)]
pub struct ObjectExpressionProperty<'a> {
    pub start: u32,
    pub computed: bool,
    pub shorthand: bool,
    pub key: Expression<'a>,
    pub value: Expression<'a>,
    pub method: bool,
    pub kind: PropertyKind,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum ObjectExpressionPropertyKind<'a> {
    Property(Box<'a, ObjectExpressionProperty<'a>>),
    SpreadElement(Box<'a, SpreadElement<'a>>),
}

#[derive(Debug, CloneIn)]
pub struct ObjectExpression<'a> {
    pub start: u32,
    pub properties: Vec<'a, ObjectExpressionPropertyKind<'a>>,
    pub end: u32,
    pub parenthesized: bool,
}

#[derive(Debug, CloneIn)]
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
    Coalescing,
    In,
    Instanceof,
}

impl<'a> BinaryOperator {
    pub fn as_str(&self) -> &'a str {
        use BinaryOperator::*;

        match self {
            Equality => "==",
            Inequality => "!=",
            StrictEquality => "===",
            StrictInequality => "!==",
            LessThan => "<",
            LessThanOrEqual => "<=",
            GreaterThan => ">",
            GreaterThanOrEqual => ">=",
            LeftShift => "<<",
            RightShift => ">>",
            UnsignedRightShift => ">>>",
            Add => "+",
            Subtract => "-",
            Multiply => "*",
            Exponentiation => "**",
            Divide => "/",
            Mod => "%",
            BitwiseAND => "&",
            BitwiseOR => "|",
            BitwiseXOR => "^",
            Coalescing => "??",
            In => "in",
            Instanceof => "instanceof",
        }
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
pub enum LogicalOperator {
    And,
    Or,
}

impl LogicalOperator {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::And => "&&",
            Self::Or => "||",
        }
    }
}

#[derive(Debug, CloneIn)]
pub struct LogicalExpression<'a> {
    pub start: u32,
    pub left: Expression<'a>,
    pub operator: LogicalOperator,
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
    pub arguments: Vec<'a, Expression<'a>>,
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
    pub pattern: &'a str,
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

#[derive(Debug, Location, CloneIn)]
pub enum Expression<'a> {
    Identifier(Box<'a, Identifier>),
    NumericLiteral(Box<'a, NumericLiteral>),
    StringLiteral(Box<'a, StringLiteral>),
    BooleanLiteral(Box<'a, BooleanLiteral>),
    NullLiteral(Box<'a, NullLiteral>),
    Import(Box<'a, Import>),
    ArrayExpression(Box<'a, ArrayExpression<'a>>),
    ObjectExpression(Box<'a, ObjectExpression<'a>>),
    BinaryExpression(Box<'a, BinaryExpression<'a>>),
    LogicalExpression(Box<'a, LogicalExpression<'a>>),
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
    AwaitExpression(Box<'a, AwaitExpression<'a>>),
    SpreadElement(Box<'a, SpreadElement<'a>>),
    Elision(Box<'a, Elision>),
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
pub enum FunctionParams<'a> {
    Pattern(Box<'a, Pattern<'a>>),
    RestElement(Box<'a, RestElement<'a>>),
}

#[derive(Debug, CloneIn)]
pub struct Function<'a> {
    pub start: u32,
    pub async_: bool,
    pub generator: bool,
    pub id: Option<Identifier>,
    pub params: Vec<'a, FunctionParams<'a>>,
    pub body: Block<'a>,
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
    pub params: Vec<'a, FunctionParams<'a>>,
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
    pub params: Vec<'a, FunctionParams<'a>>,
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
    pub id: Option<Identifier>,
    pub super_class: Option<Expression<'a>>,
    pub body: Vec<'a, ClassBody<'a>>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct ImportSpecifier<'a> {
    pub start: u32,
    pub imported: IdentifierOrLiteral<'a>,
    pub local: Option<Identifier>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct ImportDefaultSpecifier {
    pub start: u32,
    pub local: Identifier,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct ImportNamespaceSpecifier {
    pub start: u32,
    pub local: Identifier,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub enum ImportSpecifierType<'a> {
    ImportSpecifier(Box<'a, ImportSpecifier<'a>>),
    ImportDefaultSpecifier(Box<'a, ImportDefaultSpecifier>),
    ImportNamespaceSpecifier(Box<'a, ImportNamespaceSpecifier>),
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
pub struct MetaProperty {
    pub start: u32,
    pub meta: Identifier,
    pub property: Identifier,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct CallExpression<'a> {
    pub start: u32,
    pub callee: Expression<'a>,
    pub arguments: Vec<'a, Expression<'a>>,
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
pub struct BreakStatement {
    pub start: u32,
    pub label: Option<Identifier>,
    pub end: u32,
}

#[derive(Debug, CloneIn)]
pub struct ContinueStatement {
    pub start: u32,
    pub label: Option<Identifier>,
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
    pub label: Identifier,
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
    // ExportDeclaration(Box<'a, ExportDeclaration<'a>>),

    // Statements
    ExpressionStatement(Box<'a, ExpressionStatement<'a>>),
    BlockStatement(Box<'a, Block<'a>>),
    EmptyStatement(Box<'a, EmptyStatement>),
    BreakStatement(Box<'a, BreakStatement>),
    ContinueStatement(Box<'a, ContinueStatement>),
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

pub struct AST<'a> {
    arena: &'a Bump,
    body: Vec<'a, Statement<'a>>,
}

impl<'a> AST<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        AST {
            arena,
            body: Vec::new_in(arena),
        }
    }
}
