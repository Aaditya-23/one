use std::collections::HashMap;

use crate::tokenizer::Regex;

pub trait DeclarationOperations {
    fn get_id(&self) -> Vec<Identifier>;
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
    pub type_annotation: Option<ts_nodes::TsTypeAnnotation>,
}

impl Identifier {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            type_annotation: None,
        }
    }

    pub fn set_type_annotation(&mut self, type_annotation: ts_nodes::TsTypeAnnotation) {
        self.type_annotation = Some(type_annotation);
    }
}

#[derive(Debug, Clone)]
pub struct PrivateIdentifier {
    pub name: String,
    pub type_annotation: Option<ts_nodes::TsTypeAnnotation>,
}

impl PrivateIdentifier {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            type_annotation: None,
        }
    }

    pub fn set_type_annotation(&mut self, type_annotation: ts_nodes::TsTypeAnnotation) {
        self.type_annotation = Some(type_annotation);
    }
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub value: Option<String>,
    pub regex: Option<Regex>,
}

impl Literal {
    pub fn new(value: &str) -> Self {
        Self {
            value: Some(value.to_string()),
            regex: None,
        }
    }

    pub fn new_regexp(regex: Regex) -> Self {
        Self {
            value: None,
            regex: Some(regex),
        }
    }
}

#[derive(Debug, Clone)]
pub enum IdentifierOrLiteral {
    Identifier(Identifier),
    Literal(Literal),
}

#[derive(Debug, Clone)]
pub struct Function {
    id: Option<Identifier>,
    expression: bool,
    generator: bool,
    async_: bool,
    params: Vec<patterns::Pattern>,
    body: statements::Block,
    return_type: Option<ts_nodes::TsTypeAnnotation>,
    type_parameters: Vec<ts_nodes::TsTypeParameter>,
}

impl Function {
    pub fn new(
        id: Option<Identifier>,
        params: Vec<patterns::Pattern>,
        body: Vec<Body>,
        is_async: bool,
        is_generator: bool,
    ) -> Self {
        Self {
            id,
            expression: false,
            generator: is_generator,
            async_: is_async,
            params,
            body: statements::Block::new(body),
            return_type: None,
            type_parameters: vec![],
        }
    }

    pub fn set_return_type(&mut self, type_annotation: ts_nodes::TsTypeAnnotation) {
        self.return_type = Some(type_annotation);
    }

    pub fn set_type_parameters(&mut self, type_parameter: Vec<ts_nodes::TsTypeParameter>) {
        self.type_parameters = type_parameter;
    }
}

#[derive(Debug, Clone)]
pub struct MethodDefinition {
    key: expressions::Expression,
    value: Function,
    kind: String,
    computed: bool,
    static_: bool,
}

impl MethodDefinition {
    pub fn new(key: expressions::Expression, value: Function, kind: &str) -> Self {
        Self {
            key,
            value,
            kind: kind.to_string(),
            computed: false,
            static_: false,
        }
    }

    pub fn is_computed(&mut self) {
        self.computed = true;
    }

    pub fn is_static(&mut self) {
        self.static_ = true;
    }
}

#[derive(Debug, Clone)]
pub struct PropertyDefinition {
    key: expressions::Expression,
    value: Option<expressions::Expression>,
    computed: bool,
    static_: bool,
}

impl PropertyDefinition {
    pub fn new(key: expressions::Expression, value: Option<expressions::Expression>) -> Self {
        Self {
            key,
            value,
            computed: false,
            static_: false,
        }
    }

    pub fn is_computed(&mut self) {
        self.computed = true;
    }

    pub fn is_static(&mut self) {
        self.static_ = true;
    }
}

#[derive(Debug, Clone)]
pub struct StaticBlock {
    body: Vec<Body>,
}

impl StaticBlock {
    pub fn new(body: Vec<Body>) -> Self {
        Self { body }
    }
}

#[derive(Debug, Clone)]
pub enum ClassElement {
    MethodDefinition(MethodDefinition),
    PropertyDefinition(PropertyDefinition),
    StaticBlock(StaticBlock),
}

#[derive(Debug, Clone)]
pub struct Class {
    id: Option<Identifier>,
    super_class: Box<Option<expressions::Expression>>,
    body: Vec<ClassElement>,
}

impl Class {
    pub fn new(
        id: Option<Identifier>,
        super_class: Option<expressions::Expression>,
        body: Vec<ClassElement>,
    ) -> Self {
        Self {
            id,
            super_class: Box::new(super_class),
            body,
        }
    }
}

pub mod ts_nodes {
    use super::{
        expressions::{self, Expression},
        patterns, Identifier,
    };

    #[derive(Debug, Clone)]
    pub struct TsTypeParameter {
        name: String,
        constraint: Option<TsTypeAnnotation>,
        default: Option<TsTypeAnnotation>,
    }

    impl TsTypeParameter {
        pub fn new(name: &str) -> Self {
            Self {
                name: name.to_string(),
                constraint: None,
                default: None,
            }
        }

        pub fn set_constraint(&mut self, constraint: TsTypeAnnotation) {
            self.constraint = Some(constraint);
        }

        pub fn set_default(&mut self, default: TsTypeAnnotation) {
            self.default = Some(default);
        }
    }

    #[derive(Debug, Clone)]
    pub struct TsArrayType {
        pub type_: Box<TsTypeAnnotation>,
    }

    #[derive(Debug, Clone)]
    pub enum TsTypeOperator {
        Keyof,
        Typeof,
    }

    #[derive(Debug, Clone)]
    pub enum TsLiteralType {
        String(String),
        Number(String),
        Boolean(String),
    }

    #[derive(Debug, Clone)]
    pub struct TsTypePropertySignature {
        key: Expression,
        computed: bool,
        optional: bool,
        type_annotation: TsTypeAnnotation,
    }

    impl TsTypePropertySignature {
        pub fn new(
            key: Expression,
            computed: bool,
            optional: bool,
            type_annotation: TsTypeAnnotation,
        ) -> Self {
            Self {
                key,
                computed,
                optional,
                type_annotation,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct TsTypeMethodSignature {
        key: Expression,
        parameters: Vec<patterns::Pattern>,
        computed: bool,
        optional: bool,
        type_annotation: TsTypeAnnotation,
    }

    impl TsTypeMethodSignature {
        pub fn new(
            key: expressions::Expression,
            params: Vec<patterns::Pattern>,
            computed: bool,
            optional: bool,
            type_annotation: TsTypeAnnotation,
        ) -> Self {
            Self {
                key,
                parameters: params,
                computed,
                optional,
                type_annotation,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct TsTypeCallSignature {
        params: Vec<patterns::Pattern>,
        type_annotation: TsTypeAnnotation,
    }

    impl TsTypeCallSignature {
        pub fn new(params: Vec<patterns::Pattern>, type_annotation: TsTypeAnnotation) -> Self {
            Self {
                params,
                type_annotation,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum TsTypeSignature {
        Property(TsTypePropertySignature),
        Method(TsTypeMethodSignature),
        Call(TsTypeCallSignature),
    }

    #[derive(Debug, Clone)]
    pub enum TsTypeAnnotation {
        Any,
        Number,
        String,
        Boolean,
        Null,
        Undefined,
        Array(TsArrayType),
        TypeReference {
            type_name: String,
            type_parameters: Vec<TsTypeParameter>,
        },
        TypeOperator(TsTypeOperator),
        TypeLiteral {
            members: Vec<TsTypeSignature>,
        },
        UnionType {
            types: Vec<TsTypeAnnotation>,
        },
        IntersectionType {
            types: Vec<TsTypeAnnotation>,
        },
        Literal(TsLiteralType),
        TupleType {
            element_types: Vec<TsTypeAnnotation>,
        },
    }

    #[derive(Debug, Clone)]
    pub struct TsTypeAliasDeclaration {
        pub id: Identifier,
        pub type_annotation: TsTypeAnnotation,
        pub type_parameters: Vec<TsTypeParameter>,
    }

    impl TsTypeAliasDeclaration {
        pub fn new(
            id: Identifier,
            type_annotation: TsTypeAnnotation,
            type_parameters: Vec<TsTypeParameter>,
        ) -> Self {
            Self {
                id,
                type_annotation,
                type_parameters,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct TsInterfaceDeclaration {
        pub id: Identifier,
        pub body: Vec<TsTypeSignature>,
    }

    impl TsInterfaceDeclaration {
        pub fn new(id: Identifier, body: Vec<TsTypeSignature>) -> Self {
            Self { id, body }
        }
    }
}

pub mod patterns {
    use super::{expressions, ts_nodes, DeclarationOperations, Identifier};

    #[derive(Debug, Clone)]
    pub struct Property {
        key: Identifier,
        shorthand: bool,
        value: Pattern,
    }

    impl Property {
        pub fn new(key: Identifier, value: Pattern) -> Self {
            Self {
                key,
                shorthand: false,
                value,
            }
        }

        pub fn is_shorthand(&mut self) {
            self.shorthand = true;
        }
    }

    #[derive(Debug, Clone)]
    pub enum ObjectProperty {
        Property(Property),
        RestElement(RestElement),
    }

    impl DeclarationOperations for ObjectProperty {
        fn get_id(&self) -> Vec<Identifier> {
            let mut ids = vec![];

            match self {
                ObjectProperty::Property(property) => {
                    if property.shorthand {
                        ids.push(property.key.clone())
                    } else {
                        ids.extend_from_slice(&property.value.get_id());
                    }
                }
                ObjectProperty::RestElement(rest_el) => {
                    ids.extend_from_slice(&rest_el.argument.get_id());
                }
            }

            ids
        }
    }

    #[derive(Debug, Clone)]
    pub struct Object {
        properties: Vec<ObjectProperty>,
        type_annotation: Option<ts_nodes::TsTypeAnnotation>,
    }

    impl Object {
        pub fn new() -> Self {
            Self {
                properties: vec![],
                type_annotation: None,
            }
        }

        pub fn add_property(&mut self, property: ObjectProperty) {
            self.properties.push(property)
        }

        pub fn set_type_annotation(&mut self, type_annotation: ts_nodes::TsTypeAnnotation) {
            self.type_annotation = Some(type_annotation);
        }
    }

    #[derive(Debug, Clone)]
    pub struct Array {
        elements: Vec<Option<Pattern>>,
        type_annotation: Option<ts_nodes::TsTypeAnnotation>,
    }

    impl Array {
        pub fn new() -> Self {
            Self {
                elements: vec![],
                type_annotation: None,
            }
        }

        pub fn add_element(&mut self, element: Option<Pattern>) {
            self.elements.push(element);
        }

        pub fn set_type_annotation(&mut self, type_annotation: ts_nodes::TsTypeAnnotation) {
            self.type_annotation = Some(type_annotation);
        }
    }

    #[derive(Debug, Clone)]
    pub struct RestElement {
        argument: Box<Pattern>,
    }

    impl RestElement {
        pub fn new(argument: Pattern) -> Self {
            Self {
                argument: Box::new(argument),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Assignment {
        left: Box<Pattern>,
        right: expressions::Expression,
    }

    impl Assignment {
        pub fn new(left: Pattern, right: expressions::Expression) -> Self {
            Self {
                left: Box::new(left),
                right,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum Pattern {
        Identifier(Identifier),
        Array(Array),
        Object(Object),
        RestElement(RestElement),
        Assignment(Assignment),
    }

    impl DeclarationOperations for Pattern {
        fn get_id(&self) -> Vec<Identifier> {
            let mut ids = vec![];

            match self {
                Pattern::Identifier(id) => ids.push(id.clone()),
                Pattern::Array(arr) => {
                    for element in arr.elements.iter() {
                        if let Some(pattern) = element {
                            ids.extend_from_slice(&pattern.get_id());
                        }
                    }
                }
                Pattern::Object(obj) => {
                    for property in obj.properties.iter() {
                        ids.extend_from_slice(&property.get_id());
                    }
                }
                Pattern::RestElement(rest_el) => {
                    ids.extend_from_slice(&rest_el.argument.get_id());
                }
                Pattern::Assignment(assignment) => {
                    ids.extend_from_slice(&assignment.left.get_id());
                }
            }

            ids
        }
    }
}

pub mod expressions {
    use crate::plugins::jsx;

    use super::{
        patterns::{self, Pattern},
        statements, ts_nodes, Body, Class, Function, Identifier, Literal, PrivateIdentifier,
    };

    #[derive(Debug, Clone)]
    pub struct Binary {
        operator: String,
        left: Box<Expression>,
        right: Box<Expression>,
    }

    impl Binary {
        pub fn new(operator: &str, left: Expression, right: Expression) -> Self {
            Self {
                operator: operator.to_string(),
                left: Box::new(left),
                right: Box::new(right),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Conditional {
        test: Box<Expression>,
        consequent: Box<Expression>,
        alternate: Box<Expression>,
    }

    impl Conditional {
        pub fn new(test: Expression, consequent: Expression, alternate: Expression) -> Self {
            Self {
                test: Box::new(test),
                consequent: Box::new(consequent),
                alternate: Box::new(alternate),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Array {
        pub elements: Vec<Option<Expression>>,
    }

    impl Array {
        pub fn new() -> Self {
            Self { elements: vec![] }
        }

        pub fn add_element(&mut self, element: Option<Expression>) {
            self.elements.push(element);
        }
    }

    #[derive(Debug, Clone)]
    pub struct SpreadElement {
        pub argument: Box<Expression>,
    }

    impl SpreadElement {
        pub fn new(argument: Expression) -> Self {
            Self {
                argument: Box::new(argument),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Property {
        pub key: Expression,
        pub value: Expression,
        pub computed: bool,
        pub method: bool,
        pub shorthand: bool,
    }

    impl Property {
        pub fn new(key: Expression, value: Expression, is_shortand: bool) -> Self {
            Self {
                key,
                method: false,
                computed: false,
                shorthand: is_shortand,
                value,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum ObjectProperty {
        Property(Property),
        SpreadElement(SpreadElement),
    }

    #[derive(Debug, Clone)]
    pub struct Object {
        pub properties: Vec<ObjectProperty>,
    }

    impl Object {
        pub fn new() -> Self {
            Self { properties: vec![] }
        }

        pub fn add_property(&mut self, property: ObjectProperty) {
            self.properties.push(property)
        }
    }

    #[derive(Debug, Clone)]
    pub struct Member {
        object: Box<Expression>,
        property: Box<Expression>,
        computed: bool,
        optional: bool,
    }

    impl Member {
        pub fn new(object: Expression, property: Expression) -> Self {
            Self {
                object: Box::new(object),
                property: Box::new(property),
                computed: false,
                optional: false,
            }
        }

        pub fn is_computed(&mut self) {
            self.computed = true;
        }

        pub fn is_optional(&mut self) {
            self.optional = true;
        }
    }

    #[derive(Debug, Clone)]
    pub enum AssignmentLHS {
        Pattern(Pattern),
        Expression(Expression),
    }

    #[derive(Debug, Clone)]
    pub struct Assignment {
        pub operator: String,
        pub left: Box<AssignmentLHS>,
        pub right: Box<Expression>,
    }

    impl Assignment {
        pub fn new(operator: &str, left: AssignmentLHS, right: Expression) -> Self {
            Self {
                operator: operator.to_string(),
                left: Box::new(left),
                right: Box::new(right),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Unary {
        operator: String,
        argument: Box<Expression>,
    }

    impl Unary {
        pub fn new(operator: String, argument: Expression) -> Self {
            Self {
                operator,
                argument: Box::new(argument),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Update {
        pub operator: String,
        pub prefix: bool,
        pub argument: Box<Expression>,
    }

    impl Update {
        pub fn new(operator: String, prefix: bool, argument: Expression) -> Self {
            Self {
                operator,
                prefix,
                argument: Box::new(argument),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct TemplateElement {
        pub value: String,
        pub tail: bool,
    }

    impl TemplateElement {
        pub fn new(value: &str, tail: bool) -> Self {
            Self {
                value: value.to_string(),
                tail,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct TemplateLiteral {
        expressions: Vec<Expression>,
        quasis: Vec<TemplateElement>,
    }

    impl TemplateLiteral {
        pub fn new(expressions: Vec<Expression>, quasis: Vec<TemplateElement>) -> Self {
            Self {
                expressions,
                quasis,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Chain {
        expression: Box<Expression>,
    }

    impl Chain {
        pub fn new(expression: Expression) -> Self {
            Self {
                expression: Box::new(expression),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Sequence {
       pub expressions: Vec<Expression>,
    }

    impl Sequence {
        pub fn new(expressions: Vec<Expression>) -> Self {
            Self { expressions }
        }
    }

    #[derive(Debug, Clone)]
    pub enum ArrowFunctionBody {
        Expression(Box<Expression>),
        Block(statements::Block),
    }
    
    #[derive(Debug, Clone)]
    pub struct ArrowFunction {
        expression: bool,
        generator: bool,
        async_: bool,
        params: Vec<patterns::Pattern>,
        body: ArrowFunctionBody,
    }

    impl ArrowFunction {
        pub fn new(params: Vec<patterns::Pattern>, body: ArrowFunctionBody, is_async: bool) -> Self {
            Self {
                expression: false,
                generator: false,
                async_: is_async,
                params,
                body,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Call {
        pub callee: Box<Expression>,
        pub arguments: Vec<Expression>,
        pub optional: bool,
    }

    impl Call {
        pub fn new(callee: Expression, arguments: Vec<Expression>, optional: bool) -> Self {
            Self {
                callee: Box::new(callee),
                arguments,
                optional,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Logical {
       pub operator: String,
       pub left: Box<Expression>,
       pub right: Box<Expression>,
    }

    impl Logical {
        pub fn new(operator: &str, left: Expression, right: Expression) -> Self {
            Self {
                operator: operator.to_string(),
                left: Box::new(left),
                right: Box::new(right),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct This();

    impl This {
        pub fn new() -> Self {
            Self()
        }
    }

    #[derive(Debug, Clone)]
    pub struct New {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    }

    impl New {
        pub fn new(callee: Expression, arguments: Vec<Expression>) -> Self {
            Self {
                callee: Box::new(callee),
                arguments,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Super();

    impl Super {
        pub fn new() -> Self {
            Self()
        }
    }

    #[derive(Debug, Clone)]
    pub struct Yield {
        delegate: bool,
        argument: Box<Option<Expression>>,
    }

    impl Yield {
        pub fn new(delegate: bool, argument: Option<Expression>) -> Self {
            Self {
                delegate,
                argument: Box::new(argument),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Await {
        argument: Box<Expression>,
    }

    impl Await {
        pub fn new(argument: Expression) -> Self {
            Self {
                argument: Box::new(argument),
            }
        }
    }


    #[derive(Debug, Clone)]
    pub struct MetaProperty {
        meta: Identifier,
        property: Identifier,
    }

    impl MetaProperty {
        pub fn new(meta: Identifier, property: Identifier) -> Self {
            Self { meta, property }
        }
    }

    #[derive(Debug, Clone)]
    pub struct TsAs {
        pub expression: Box<Expression>,
        pub type_annotation: ts_nodes::TsTypeAnnotation,
    }

    impl TsAs {
        pub fn new(expression: Expression, type_annotation: ts_nodes::TsTypeAnnotation) -> Self {
            Self {
                expression: Box::new(expression),
                type_annotation,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct TsNonNull {
        pub expression: Box<Expression>,
    }

    impl TsNonNull {
        pub fn new(expression: Expression) -> Self {
            Self {
                expression: Box::new(expression),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum Expression {
        Identifier(Identifier),
        PrivateIdentifier(PrivateIdentifier),
        Binary(Binary),
        Literal(Literal),
        Conditional(Conditional),
        Array(Array),
        Object(Object),
        Member(Member),
        Assignment(Assignment),
        Unary(Unary),
        Update(Update),
        TemplateLiteral(TemplateLiteral),
        SpreadElement(SpreadElement),
        Chain(Chain),
        Sequence(Sequence),
        Function(Function),
        ArrowFunction(ArrowFunction),
        Call(Call),
        Logical(Logical),
        This(This),
        New(New),
        Class(Class),
        Super(Super),
        Yield(Yield),
        Await(Await),
        Meta(MetaProperty),
        JSXElement(jsx::nodes::JSXElement),
        TsAs(TsAs),
        TsNonNull(TsNonNull),
    }
}

pub mod statements {
    use super::{declarations, expressions, patterns, Body, Identifier};

    #[derive(Debug, Clone)]
    pub struct Block {
        pub body: Vec<Body>,
    }

    impl Block {
        pub fn new(body: Vec<Body>) -> Self {
            Self { body }
        }
    }

    #[derive(Debug, Clone)]
    pub struct While {
        test: expressions::Expression,
        body: Box<Statement>,
    }

    impl While {
        pub fn new(test: expressions::Expression, body: Statement) -> Self {
            Self {
                test,
                body: Box::new(body),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct EmptyStatement();

    impl EmptyStatement {
        pub fn new() -> Self {
            Self()
        }
    }

    #[derive(Debug, Clone)]
    pub struct Break {
        label: Option<Identifier>,
    }

    impl Break {
        pub fn new(label: Option<Identifier>) -> Self {
            Self { label }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Continue {
        label: Option<Identifier>,
    }

    impl Continue {
        pub fn new(label: Option<Identifier>) -> Self {
            Self { label }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Debugger();

    impl Debugger {
        pub fn new() -> Self {
            Self()
        }
    }

    #[derive(Debug, Clone)]
    pub struct With {
        object: expressions::Expression,
        body: Box<Statement>,
    }

    impl With {
        pub fn new(object: expressions::Expression, body: Statement) -> Self {
            Self {
                object,
                body: Box::new(body),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Return {
        argument: Option<expressions::Expression>,
    }

    impl Return {
        pub fn new(argument: Option<expressions::Expression>) -> Self {
            Self { argument }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Labeled {
        label: Identifier,
        body: Box<Statement>,
    }

    impl Labeled {
        pub fn new(label: Identifier, body: Statement) -> Self {
            Self {
                label,
                body: Box::new(body),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct If {
        test: expressions::Expression,
        consequent: Box<Statement>,
        alternate: Box<Option<Statement>>,
    }

    impl If {
        pub fn new(
            test: expressions::Expression,
            consequent: Statement,
            alternate: Option<Statement>,
        ) -> Self {
            Self {
                test,
                consequent: Box::new(consequent),
                alternate: Box::new(alternate),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct SwitchCase {
        test: Option<expressions::Expression>,
        consequent: Vec<Body>,
    }

    impl SwitchCase {
        pub fn new(test: Option<expressions::Expression>, consequent: Vec<Body>) -> Self {
            Self { test, consequent }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Switch {
        discriminant: expressions::Expression,
        cases: Vec<SwitchCase>,
    }

    impl Switch {
        pub fn new(discriminant: expressions::Expression) -> Self {
            Self {
                discriminant,
                cases: vec![],
            }
        }

        pub fn add_case(&mut self, case: SwitchCase) {
            self.cases.push(case);
        }
    }

    #[derive(Debug, Clone)]
    pub struct Throw {
        argument: expressions::Expression,
    }

    impl Throw {
        pub fn new(argument: expressions::Expression) -> Self {
            Self { argument }
        }
    }

    #[derive(Debug, Clone)]
    pub struct CatchClause {
        param: patterns::Pattern,
        body: Block,
    }

    impl CatchClause {
        pub fn new(param: patterns::Pattern, body: Block) -> Self {
            Self { param, body }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Try {
        block: Block,
        handler: Option<CatchClause>,
        finalizer: Option<Block>,
    }

    impl Try {
        pub fn new(block: Block, handler: Option<CatchClause>, finalizer: Option<Block>) -> Self {
            Self {
                block,
                handler,
                finalizer,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct DoWhile {
        body: Box<Statement>,
        test: expressions::Expression,
    }

    impl DoWhile {
        pub fn new(body: Statement, test: expressions::Expression) -> Self {
            Self {
                body: Box::new(body),
                test,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum ForInit {
        VariableDeclaration(declarations::Variable),
        Expression(expressions::Expression),
    }

    #[derive(Debug, Clone)]
    pub struct For {
        init: Option<ForInit>,
        test: Option<expressions::Expression>,
        update: Option<expressions::Expression>,
        body: Box<Statement>,
    }

    impl For {
        pub fn new(
            init: Option<ForInit>,
            test: Option<expressions::Expression>,
            update: Option<expressions::Expression>,
            body: Statement,
        ) -> Self {
            Self {
                init,
                test,
                update,
                body: Box::new(body),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum ForInLeft {
        VariableDeclaration(declarations::Variable),
        Pattern(patterns::Pattern),
    }

    #[derive(Debug, Clone)]
    pub struct ForIn {
        left: ForInLeft,
        right: expressions::Expression,
        body: Box<Statement>,
    }

    impl ForIn {
        pub fn new(left: ForInLeft, right: expressions::Expression, body: Statement) -> Self {
            Self {
                left,
                right,
                body: Box::new(body),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum Statement {
        Expression(expressions::Expression),
        While(While),
        Block(Block),
        EmptyStatement(EmptyStatement),
        Break(Break),
        Continue(Continue),
        Debugger(Debugger),
        With(With),
        Return(Return),
        Labeled(Labeled),
        If(If),
        Switch(Switch),
        Throw(Throw),
        Try(Try),
        DoWhile(DoWhile),
        For(For),
        ForIn(ForIn),
    }
}

pub mod declarations {
    use super::{
        expressions, patterns, Class, DeclarationOperations, Function, Identifier,
        IdentifierOrLiteral, Literal,
    };

    #[derive(Debug, Clone)]
    pub struct VariableDeclarator {
        pub id: patterns::Pattern,
        pub init: Option<expressions::Expression>,
    }

    impl VariableDeclarator {
        pub fn new(id: patterns::Pattern, init: Option<expressions::Expression>) -> Self {
            Self { id, init }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Variable {
        pub declarations: Vec<VariableDeclarator>,
        pub kind: String,
    }

    impl Variable {
        pub fn new(kind: &str) -> Self {
            Self {
                declarations: vec![],
                kind: kind.to_string(),
            }
        }

        pub fn add_declarator(&mut self, declarator: VariableDeclarator) {
            self.declarations.push(declarator);
        }
    }

    #[derive(Debug, Clone)]
    pub struct ImportSpecifier {
        imported: Identifier,
        local: Identifier,
    }

    impl ImportSpecifier {
        pub fn new(imported: Identifier, local: Identifier) -> Self {
            Self { imported, local }
        }
    }

    #[derive(Debug, Clone)]
    pub struct ImportDefaultSpecifier {
        local: Identifier,
    }

    impl ImportDefaultSpecifier {
        pub fn new(local: Identifier) -> Self {
            Self { local }
        }
    }

    #[derive(Debug, Clone)]
    pub struct ImportNamespaceSpecifier {
        local: Identifier,
    }

    impl ImportNamespaceSpecifier {
        pub fn new(local: Identifier) -> Self {
            Self { local }
        }
    }

    #[derive(Debug, Clone)]
    pub enum ImportSpecifierType {
        ImportSpecifier(ImportSpecifier),
        ImportDefaultSpecifier(ImportDefaultSpecifier),
        ImportNamespaceSpecifier(ImportNamespaceSpecifier),
    }

    impl DeclarationOperations for ImportSpecifierType {
        fn get_id(&self) -> Vec<Identifier> {
            let mut ids = vec![];

            match self {
                _ => panic!("not implemented"),
            }

            ids
        }
    }

    #[derive(Debug, Clone)]
    pub struct Import {
        specifiers: Vec<ImportSpecifierType>,
        source: Literal,
    }

    impl Import {
        pub fn new(specifiers: Vec<ImportSpecifierType>, source: Literal) -> Self {
            Self { specifiers, source }
        }
    }

    #[derive(Debug, Clone)]
    pub struct ExportSpecifier {
        local: IdentifierOrLiteral,
        exported: IdentifierOrLiteral,
    }

    impl ExportSpecifier {
        pub fn new(local: IdentifierOrLiteral, exported: IdentifierOrLiteral) -> Self {
            Self { local, exported }
        }
    }

    #[derive(Debug, Clone)]
    pub struct ExportNamed {
        declaration: Box<Option<Declaration>>,
        specifiers: Vec<ExportSpecifier>,
        source: Option<Literal>,
    }

    impl ExportNamed {
        pub fn new(
            declaration: Option<Declaration>,
            specifiers: Vec<ExportSpecifier>,
            source: Option<Literal>,
        ) -> Self {
            Self {
                declaration: Box::new(declaration),
                specifiers,
                source,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum ExportDefaultDeclaration {
        Declaration(Box<Declaration>),
        Expression(expressions::Expression),
    }

    #[derive(Debug, Clone)]
    pub struct ExportDefault {
        declaration: ExportDefaultDeclaration,
    }

    impl ExportDefault {
        pub fn new(declaration: ExportDefaultDeclaration) -> Self {
            Self { declaration }
        }
    }

    #[derive(Debug, Clone)]
    pub struct ExportAll {
        exported: Option<IdentifierOrLiteral>,
        source: Literal,
    }

    impl ExportAll {
        pub fn new(exported: Option<IdentifierOrLiteral>, source: Literal) -> Self {
            Self { exported, source }
        }
    }

    #[derive(Debug, Clone)]
    pub enum Declaration {
        Variable(Variable),
        Function(Function),
        Import(Import),
        ExportNamed(ExportNamed),
        ExportDefault(ExportDefault),
        ExportAll(ExportAll),
        Class(Class),
    }

    impl DeclarationOperations for Declaration {
        fn get_id(&self) -> Vec<Identifier> {
            let mut ids = vec![];

            match self {
                Declaration::Variable(v) => {
                    for declaration in v.declarations.iter() {
                        ids.extend_from_slice(&declaration.id.get_id());
                    }
                }
                Declaration::Function(func) => {
                    if let Some(id) = &func.id {
                        ids.push(id.clone())
                    }
                }
                _ => panic!("not implemented"),
            }

            ids
        }
    }
}

#[derive(Debug, Clone)]
pub enum Body {
    Declaration(declarations::Declaration),
    Statement(statements::Statement),
    TypeDeclaration(ts_nodes::TsTypeAliasDeclaration),
    InterfaceDeclartion(ts_nodes::TsInterfaceDeclaration),
}

#[derive(Debug, Clone)]
pub struct AST {
    pub body: Vec<Body>,
    pub source_type: String,
}

impl AST {
    pub fn new_module() -> Self {
        Self {
            body: vec![],
            source_type: String::from("module"),
        }
    }
}

pub trait NodeVisitor {
    fn visit_declaration(&self) {}
}
