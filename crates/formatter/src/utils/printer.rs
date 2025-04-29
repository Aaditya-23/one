use std::collections::VecDeque;

use super::commands::{Command::*, *};

pub struct FormatterOptions {
    width: usize,
    tabs: bool,
    indent_size: usize,
    semicolons: bool,
    single_quotes: bool,
}

impl Default for FormatterOptions {
    fn default() -> Self {
        Self {
            width: 80,
            tabs: false,
            indent_size: 2,
            semicolons: true,
            single_quotes: false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Mode {
    Break,
    Flat,
}

pub struct Printer {
    opts: FormatterOptions,
}

impl<'a> Printer {
    fn gen_indentation(&self, level: usize) -> String {
        let ind = if self.opts.tabs {
            "\t".repeat(self.opts.indent_size * level)
        } else {
            " ".repeat(self.opts.indent_size * level)
        };

        ind
    }

    fn propagate_breaks(&self, doc: &mut Vec<Command>) {
        let mut group_stack: Vec<&mut GroupOptions> = Vec::new();
        let mut stack = Vec::new();

        stack.extend(doc.iter_mut().rev().map(|cmd| (cmd, false)));

        while !stack.is_empty() {
            let (cmd, is_group_exit) = unsafe { stack.pop().unwrap_unchecked() };

            match cmd {
                Group(cmds, opts) => {
                    if is_group_exit {
                        let opts = group_stack.pop();

                        if let Some(opts) = opts {
                            if opts.should_break && group_stack.len() > 0 {
                                let last = unsafe { group_stack.last_mut().unwrap_unchecked() };
                                last.should_break = true;
                            }
                        }
                    }

                    group_stack.push(opts);

                    let mut index = 0;

                    stack.extend(cmds.iter_mut().rev().map(|cmd| {
                        if index == 0 {
                            index += 1;
                            (cmd, true)
                        } else {
                            (cmd, false)
                        }
                    }));
                }
                Array(cmds) => stack.extend(cmds.iter_mut().rev().map(|cmd| (cmd, false))),
                Indent(cmds) => stack.extend(cmds.iter_mut().rev().map(|cmd| (cmd, false))),
                IfBreak(break_cnt, flat_cnt, opts) => {
                    stack.extend(break_cnt.iter_mut().rev().map(|cmd| (cmd, false)));
                    stack.extend(flat_cnt.iter_mut().rev().map(|cmd| (cmd, false)));
                }
                BreakParent | Hardline => {
                    if group_stack.len() > 0 {
                        let last = unsafe { group_stack.last_mut().unwrap_unchecked() };
                        last.should_break = true;
                    }
                }
                _ => {}
            }
        }
    }

    fn fits(
        &self,
        mut stack: Vec<(&'a Command<'a>, Mode, usize)>,
        rest_cmds: &Vec<(&'a Command<'a>, Mode, usize)>,
        mut width: isize,
        must_be_flat: bool,
    ) -> bool {
        let mut rest_index = rest_cmds.len();

        while width >= 0 {
            if stack.len() == 0 {
                if rest_index == 0 {
                    return true;
                } else {
                    rest_index -= 1;
                    stack.push(rest_cmds[rest_index]);
                }
            }

            let (cmd, mode, _ind) = unsafe { stack.pop().unwrap_unchecked() };

            match cmd {
                Group(cmds, opts) => {
                    if must_be_flat && opts.should_break {
                        return false;
                    }

                    let group_mode = if opts.should_break { Mode::Break } else { mode };

                    stack.extend(cmds.iter().rev().map(|cmd| (cmd, group_mode, _ind)));
                }
                IfBreak(break_cmds, flat_cmds, opts) => {}
                Array(cmds) => {
                    stack.extend(cmds.iter().rev().map(|cmd| (cmd, mode, _ind)));
                }
                Indent(cmds) | Dedent(cmds) => {
                    stack.extend(cmds.iter().rev().map(|cmd| (cmd, mode, _ind)));
                }
                Text(text) => {
                    width -= text.len() as isize;
                }
                Line => {
                    if mode == Mode::Break {
                        return true;
                    }

                    width -= 1;
                }
                Softline => {
                    if mode == Mode::Break {
                        return true;
                    }
                }
                Hardline => {
                    return true;
                }
                BreakParent => {}
            }
        }

        false
    }

    pub fn print(&self, doc: &mut Vec<Command>) -> String {
        let mut output = String::new();
        let mut curr_width = 0;
        let mut remeasure = false;

        self.propagate_breaks(doc);
        let mut stack = Vec::new();
        stack.extend(doc.iter().rev().map(|cmd| (cmd, Mode::Break, 0 as usize)));

        while !stack.is_empty() {
            let (cmd, mode, ind) = unsafe { stack.pop().unwrap_unchecked() };

            match cmd {
                Group(cmds, opts) => {
                    if mode == Mode::Flat && !remeasure {
                        let mode = if opts.should_break {
                            Mode::Break
                        } else {
                            Mode::Flat
                        };

                        stack.extend(cmds.iter().rev().map(|cmd| (cmd, mode, ind)));
                    } else {
                        remeasure = false;

                        let mut temp_stack = Vec::new();
                        temp_stack.extend(cmds.iter().rev().map(|cmd| (cmd, Mode::Flat, ind)));

                        let fits =
                            self.fits(temp_stack, &stack, (self.opts.width - curr_width) as isize, false);

                        if !opts.should_break && fits {
                            stack.extend(cmds.iter().rev().map(|cmd| (cmd, Mode::Flat, ind)));
                        } else {
                            stack.extend(cmds.iter().rev().map(|cmd| (cmd, Mode::Break, ind)));
                        }
                    }
                }
                IfBreak(break_cmds, flat_cmds, opts) => {}
                Array(cmds) => {
                    stack.extend(cmds.iter().rev().map(|cmd| (cmd, mode, ind)));
                }
                Indent(cmds) => {
                    stack.extend(cmds.iter().rev().map(|cmd| (cmd, mode, ind + 1)));
                }
                Dedent(cmds) => {
                    stack.extend(
                        cmds.iter()
                            .rev()
                            .map(|cmd| (cmd, mode, ind.checked_sub(1).unwrap_or(0))),
                    );
                }
                Text(text) => {
                    output.push_str(text);
                    curr_width += text.len();
                }
                Line => match mode {
                    Mode::Break => {
                        let ind_content = self.gen_indentation(ind);
                        output.push('\n');
                        output.push_str(&ind_content);

                        curr_width = ind_content.len();
                    }
                    Mode::Flat => {
                        output.push(' ');
                        curr_width += 1;
                    }
                },
                Softline => {
                    if mode == Mode::Break {
                        let ind_content = self.gen_indentation(ind);
                        output.push('\n');
                        output.push_str(&ind_content);

                        curr_width = ind_content.len();
                    }
                }
                Hardline => {
                    if mode == Mode::Flat {
                        remeasure = true;
                    }

                    let ind_content = self.gen_indentation(ind);
                    output.push('\n');
                    output.push_str(&ind_content);

                    curr_width = ind_content.len();
                }
                BreakParent => {}
            }
        }

        output
    }

    pub fn new(fmt_opts: FormatterOptions) -> Self {
        Printer { opts: fmt_opts }
    }
}
