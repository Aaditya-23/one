#[derive(Debug, Clone)]
pub enum DocNode {
    Group(Vec<DocNode>),
    Hardline,
    Line,
    Indent,
    Join(Vec<DocNode>, Vec<DocNode>),
    String(String),
}

pub type DocTree = Vec<DocNode>;

pub struct Options {
    pub single_quote: bool,
    pub tab_width: i32,
    pub semicolon: bool,
    pub bracket_spacing: bool,
}

impl Options {
    pub fn default() -> Self {
        Self {
            single_quote: false,
            tab_width: 2,
            semicolon: true,
            bracket_spacing: true,
        }
    }
}

pub fn print(doc: DocTree) -> String {
    let doc_nodes = doc;
    let mut code = String::new();

    for node in doc_nodes {
        code.push_str(&print_node(node));
    }

    code
}

fn print_node(node: DocNode) -> String {
    match node {
        DocNode::Group(nodes) => {
            let mut code = String::new();

            for node in nodes {
                code.push_str(&print_node(node));
            }

            code
        }
        DocNode::Join(separator, nodes) => {
            let mut code = String::new();
            let nodes_len = nodes.len();
            
            for (index, node) in nodes.into_iter().enumerate() {
                code.push_str(&print_node(node));
                if index < nodes_len-1 {
                    code.push_str(&print(separator.clone()));
                }
            }

            code
            
        }

        DocNode::Indent => {
             String::from('\t')
        }

        DocNode::Hardline => String::from('\n'),
        
        DocNode::Line => String::from(' '),

        DocNode::String(s) => s,

        _ => panic!("unimplemented"),
    }
}
