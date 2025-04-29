#[derive(Debug, Clone)]
pub struct GroupOptions {
    pub should_break: bool,
    pub id: Option<String>,
}

impl Default for GroupOptions {
    fn default() -> Self {
        Self {
            should_break: false,
            id: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct IfBreakOptions {
    pub group_id: Option<String>,
}
#[derive(Debug)]
pub enum Command<'a> {
    Group(Vec<Command<'a>>, GroupOptions),
    IfBreak(Vec<Command<'a>>, Vec<Command<'a>>, IfBreakOptions),
    Array(Vec<Command<'a>>),
    Indent(Vec<Command<'a>>),
    Dedent(Vec<Command<'a>>),
    Text(&'a str),
    Line,
    Softline,
    Hardline,
    BreakParent,
}

impl Clone for Command<'_> {
    fn clone(&self) -> Self {
        match self {
            Command::Group(cmds, opts) => Command::Group(cmds.clone(), opts.clone()),
            Command::Array(cmds) => Command::Array(cmds.clone()),
            Command::IfBreak(break_cmd, flat_cmd, opts) => {
                Command::IfBreak(break_cmd.clone(), flat_cmd.clone(), opts.clone())
            }
            Command::Indent(cmds) => Command::Indent(cmds.clone()),
            Command::Dedent(cmds) => Command::Dedent(cmds.clone()),
            Command::Text(text) => Command::Text(text),
            Command::Softline => Command::Softline,
            Command::Line => Command::Line,
            Command::Hardline => Command::Hardline,
            Command::BreakParent => Command::BreakParent,
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

    ($cmds:expr, $should_break:expr, $id:expr) => {{
        let mut g_opts = GroupOptions::default();
        g_opts.should_break = $should_break;
        g_opts.id = Some($id.to_string());

        Command::Group($cmds, g_opts)
    }};
}

#[macro_export]
macro_rules! array {
    ($cmds:expr) => {{
        Command::Array($cmds)
    }};
}

#[macro_export]
macro_rules! join {
    ($sep:expr, $cmds:expr) => {{
        let mut array_cmd = vec![];

        let mut it = $cmds.into_iter();

        if let Some(cmd) = it.next(){
            array_cmd.push(cmd);
        }

        it.for_each(|cmd| {
            array_cmd.push($sep.clone());
            array_cmd.push(cmd);
        });

        Command::Array(array_cmd)
    }};
}

#[macro_export]
macro_rules! indent {
    ($cmds:expr) => {{
        Command::Indent($cmds)
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
        Command::Line
    };
}

#[macro_export]
macro_rules! softline {
    () => {
        Command::Softline
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
