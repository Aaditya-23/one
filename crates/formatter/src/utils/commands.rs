use bumpalo::{boxed::Box, collections::Vec, Bump};

#[derive(Debug)]
pub struct GroupOptions {
    pub should_break: bool,
    pub(in crate::utils) _break_all_internal: bool,
}

impl Default for GroupOptions {
    fn default() -> Self {
        Self {
            should_break: false,
            _break_all_internal: false,
        }
    }
}

#[derive(Debug)]
pub struct ArrayOptions {
    pub(in crate::utils) _break_all_internal: bool,
}

impl Default for ArrayOptions {
    fn default() -> Self {
        Self {
            _break_all_internal: false,
        }
    }
}

#[derive(Debug)]
pub struct IndentOptions {
    pub(in crate::utils) _break_all_internal: bool,
}

impl Default for IndentOptions {
    fn default() -> Self {
        Self {
            _break_all_internal: false,
        }
    }
}

#[derive(Debug)]
pub struct DedentOptions {
    pub(in crate::utils) _break_all_internal: bool,
}

impl Default for DedentOptions {
    fn default() -> Self {
        Self {
            _break_all_internal: false,
        }
    }
}

#[derive(Debug)]
pub struct IfBreakOptions {
    pub group_id: Option<String>,
    pub(in crate::utils) _break_all_internal: bool,
}

impl Default for IfBreakOptions {
    fn default() -> Self {
        Self {
            group_id: None,
            _break_all_internal: false,
        }
    }
}

#[derive(Debug)]
pub struct LineOptions {
    pub(in crate::utils) _insert_line_internal: bool,
    pub(in crate::utils) _width_internal: usize,
}

impl Default for LineOptions {
    fn default() -> Self {
        Self {
           _insert_line_internal: false,
            _width_internal: 0,
        }
    }
}

#[derive(Debug)]
pub struct SoftlineOptions {
    pub(in crate::utils) _insert_line_internal: bool,
    pub(in crate::utils) _width_internal: usize,
}

impl Default for SoftlineOptions {
    fn default() -> Self {
        Self {
            _insert_line_internal: false,
            _width_internal: 0,
        }
    }
}

pub enum Command<'a> {
    Group(Vec<'a, Command<'a>>, GroupOptions),
    ConditionalGroup(Vec<'a, Command<'a>>, GroupOptions),
    IfBreak(Vec<'a, Command<'a>>, Vec<'a, Command<'a>>, IfBreakOptions),
    Array(Vec<'a, Command<'a>>, ArrayOptions),
    Indent(Vec<'a, Command<'a>>, IndentOptions),
    Dedent(Vec<'a, Command<'a>>, DedentOptions),
    Join(Box<'a, Command<'a>>, Vec<'a, Command<'a>>),
    Text(&'a str),
    Line(LineOptions),
    Softline(SoftlineOptions),
    Hardline,
    BreakParent,
}

// impl<'a> Command<'a> {
//     pub fn clone_in(&self, arena: &Bump) -> Self {
//         match self {
//             Self::Group(cmds,opts ) => {
//                 cmds.clone();
//                 todo!()
//             }
//             _ => todo!()
//         }
//     }
// } 

#[macro_export]
macro_rules! group {
    ($cmds:tt) => {{
        Command::Group($cmds, GroupOptions::default())
    }};

    ($arena:expr, $cmds:tt) => {{
        let mut vec: Vec<Command<'a>> = Vec::new_in($arena);
        vec.extend($cmds);

        Command::Group(vec, GroupOptions::default())
    }};

    ($arena:expr, $cmds:tt, $should_break:expr) => {{
        let mut vec: Vec<Command<'a>> = Vec::new_in($arena);
        vec.extend($cmds);

        let mut g_opts = GroupOptions::default();
        g_opts.should_break = $should_break;

        Command::Group(vec, g_opts)
    }};
}

#[macro_export]
macro_rules! array {
    ($cmds:tt) => {{
        Command::Array($cmds, ArrayOptions::default())
    }};

    ($arena:expr, $cmds:tt) => {{
        let mut vec: Vec<Command<'a>> = Vec::new_in($arena);
        vec.extend($cmds);

        Command::Array(vec, ArrayOptions::default())
    }};
}

#[macro_export]
macro_rules! indent {
    ($cmds:tt) => {{
        Command::Indent($cmds, IndentOptions::default())
    }};
    ($arena:expr ,$cmds:tt) => {{
        let mut vec = Vec::new_in($arena);
        vec.extend($cmds);

        Command::Indent(vec, IndentOptions::default())
    }};
}

#[macro_export]
macro_rules! text {
    ($x:expr) => {
        Command::Text($x)
    };
}

#[macro_export]
macro_rules! line {
    () => {
        Command::Line(LineOptions::default())
    };
}

#[macro_export]
macro_rules! softline {
    () => {
        Command::Softline(SoftlineOptions::default())
    };
}

#[macro_export]
macro_rules! hardline {
    () => {
        Command::Hardline
    };
}

#[macro_export]
macro_rules! break_parent {
    () => {
        Command::BreakParent()
    };
}
