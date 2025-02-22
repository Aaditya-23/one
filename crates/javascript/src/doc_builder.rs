use crate::{
    nodes::{
        declarations::{self, Declaration},
        expressions::{self, Expression},
        patterns::{self, Pattern},
        statements::{self, Statement},
        Body, AST,
    },
    parser::Parser,
};
use doc::{DocNode, DocTree, Options};

struct DocBuilder {
    options: Options,
}

impl DocBuilder {
    fn handle_variable_id(&self, id: Pattern) -> DocNode {
        match id {
            Pattern::Identifier(identifier) => DocNode::String(identifier.name),
            _ => panic!(),
        }
    }

    fn handle_expression(&self, exp: Expression) -> DocNode {
        match exp {
            // Expression::Literal(literal) => DocNode::String(literal.value),
            _ => panic!(),
        }
    }

    fn build_pattern(&self, pattern: Pattern) -> DocNode {
        match pattern {
            Pattern::Identifier(identifier) => DocNode::String(identifier.name),
            _ => panic!(),
        }
    }

    fn build_expression(&self, exp: Expression) -> DocNode {
        match exp {
            // Expression::Literal(literal) => DocNode::String(literal.value),
            _ => panic!(),
        }
    }

    fn build_variable_assignment(&self, declaration: declarations::VariableDeclarator) -> DocNode {
        let left = self.build_pattern(declaration.id);
        let doc_node = if let Some(exp) = declaration.init {
            let right = self.build_expression(exp);

            vec![
                left,
                DocNode::Line,
                DocNode::String("=".to_string()),
                DocNode::Line,
                right,
            ]
        } else {
            vec![left]
        };

        DocNode::Group(doc_node)
    }

    fn build_variable_declaration(&self, vd: declarations::Variable) -> DocNode {
        let kind = DocNode::String(vd.kind);

        let mut declarations = vec![];
        for declaration in vd.declarations {
            declarations.push(self.build_variable_assignment(declaration));
        }

        let doc_node = vec![
            kind,
            DocNode::Line,
            DocNode::Join(
                vec![
                    DocNode::String(",".to_string()),
                    DocNode::Hardline,
                    DocNode::Indent
                ],
                declarations,
            ),
        ];
        DocNode::Group(doc_node)
    }

    fn build_body(&self, ast_node: Body) -> DocNode {
        match ast_node {
            Body::Declaration(declaration) => match declaration {
                Declaration::Variable(variable_declaration) => {
                    self.build_variable_declaration(variable_declaration)
                }
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    fn build_doc(&self, ast: AST) -> Vec<DocNode> {
        let mut doc = vec![];

        for node in ast.body {
            doc.push(self.build_body(node));
        }

        doc
    }
}

pub fn build(code: String, options: Options) -> DocTree {
    let ast = Parser::new(code).parse();
    let builder = DocBuilder { options };

    builder.build_doc(ast)
}
