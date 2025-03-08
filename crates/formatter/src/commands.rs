use bumpalo::{boxed::Box, collections::Vec};

#[derive(Debug, Default)]
pub struct GroupOptions {
    pub should_break: bool,
}

pub enum Command<'a> {
    Group(Vec<'a, Command<'a>>, GroupOptions),
    Array(Vec<'a, Command<'a>>),
    Text(&'a str),
    Indent(Vec<'a, Command<'a>>),
    Dedent,
    Line,
    Softline,
    Hardline,
}

#[macro_export]
macro_rules! group {
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
    ($arena:expr, $cmds:tt) => {
        {
            let mut vec: Vec<Command<'a>> = Vec::new_in($arena);
            vec.extend($cmds);

            Command::Array(vec)
        }
    }
}

#[macro_export]
macro_rules! indent {
    ($arena:expr ,$cmds:tt) => {
        {
            let mut vec: Vec<Command<'a>> = Vec::new_in($arena);
            vec.extend($cmds);
            
            Command::Indent(vec)
        }
    };
}

#[macro_export]
macro_rules! text {
    ($x:expr) => {
        Command::Text($x)
    };
}
