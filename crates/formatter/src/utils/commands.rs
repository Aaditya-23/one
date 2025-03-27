#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct LineOptions {
    pub(in crate::utils) _insert_line_internal: bool,
}

impl Default for LineOptions {
    fn default() -> Self {
        Self {
            _insert_line_internal: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SoftlineOptions {
    pub(in crate::utils) _insert_line_internal: bool,
}

impl Default for SoftlineOptions {
    fn default() -> Self {
        Self {
            _insert_line_internal: false,
        }
    }
}

#[derive(Debug)]
pub enum Command<'a> {
    Group(Vec<Command<'a>>, GroupOptions),
    ConditionalGroup(Vec<Command<'a>>, GroupOptions),
    IfBreak(Vec<Command<'a>>, Vec<Command<'a>>, IfBreakOptions),
    Array(Vec<Command<'a>>, ArrayOptions),
    Indent(Vec<Command<'a>>, IndentOptions),
    Dedent(Vec<Command<'a>>, DedentOptions),
    Join(Box<Command<'a>>, Vec<Command<'a>>),
    Text(&'a str),
    Line(LineOptions),
    Softline(SoftlineOptions),
    Hardline,
    BreakParent,
}

impl Clone for Command<'_> {
    fn clone(&self) -> Self {

        match self {
            Command::Group(cmds,opts ) => {
                Command::Group(cmds.clone(), opts.clone())
            }
            Command::ConditionalGroup(cmds, opts ) => {
                Command::ConditionalGroup(cmds.clone(), opts.clone())
            }
            Command::Array(cmds,opts )=> {
                Command::Array(cmds.clone(),  opts.clone())
            }
            Command::IfBreak(break_cmd,flat_cmd ,opts ) => {
                Command::IfBreak(break_cmd.clone(), flat_cmd.clone(), opts.clone())
            }
            Command::Indent(cmds,opts ) => {
                Command::Indent(cmds.clone(), opts.clone())
            }
            Command::Dedent(cmds,opts ) => {
                Command::Dedent(cmds.clone(), opts.clone())
            }
            Command::Join(sep, cmds ) => {
                Command::Join(sep.clone(), cmds.clone())
            }
            Command::Text(text)=> {
                Command::Text(text)
            }
            Command::Softline(opts) => {
                Command::Softline(opts.clone())
            }
            Command::Line(opts) => {
                Command::Line(opts.clone())
            }
            Command::Hardline => {
                Command::Hardline
            }
            Command::BreakParent => {
                Command::BreakParent
            }
        }
    }
}

#[macro_export]
macro_rules! group {
    ($cmds:expr) => {{
        Command::Group($cmds, GroupOptions::default())
    }};

    ($cmds:expr, $should_break:expr) => {{
        let mut g_opts = GroupOptions::default();
        g_opts.should_break = $should_break;

        Command::Group($cmds, g_opts)
    }};
}

#[macro_export]
macro_rules! conditional_group {
    ($cmds:expr) => {{
        Command::ConditionalGroup($cmds, GroupOptions::default())
    }};

    ($cmds:expr, $should_break:expr) => {{
        let mut g_opts = GroupOptions::default();
        g_opts.should_break = $should_break;

        Command::ConditionalGroup($cmds, g_opts)
    }};
}

#[macro_export]
macro_rules! array {
    ($cmds:expr) => {{
        Command::Array($cmds, ArrayOptions::default())
    }};
}

#[macro_export]
macro_rules! join {
    ($sep:expr, $cmds:expr) => {
        Command::Join(Box::new($sep), $cmds)
    };
}

#[macro_export]
macro_rules! indent {
    ($cmds:expr) => {{
        Command::Indent($cmds, IndentOptions::default())
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
