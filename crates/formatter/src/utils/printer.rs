use std::ops::DerefMut;

use bumpalo::{collections::Vec as BumpVec, Bump};

use crate::array;

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

pub struct Printer<'a> {
    arena: &'a Bump,
    opts: FormatterOptions,
}

impl<'a, 'b, 'c> Printer<'a> {
    fn expand_join_cmd_inner(&self, doc: &mut BumpVec<'a, Command<'a>>) {
        for cmd in doc.iter_mut() {
            match cmd {
                Group(cmds, _) => {
                    self.expand_join_cmd_inner(cmds);
                }
                ConditionalGroup(cmds, _) => {
                    self.expand_join_cmd_inner(cmds);
                }
                IfBreak(break_cmd, flat_cmd, _) => {
                    self.expand_join_cmd_inner(break_cmd);
                    self.expand_join_cmd_inner(flat_cmd);
                }
                Array(cmds, _) => {
                    self.expand_join_cmd_inner(cmds);
                }
                Indent(cmds, _) => {
                    self.expand_join_cmd_inner(cmds);
                }
                Dedent(cmds, _) => {
                    self.expand_join_cmd_inner(cmds);
                }
                Join(sep, cmds) => {
                    let mut expanded_form = BumpVec::new_in(self.arena);

                    let it = cmds.iter().take(cmds.len().max(1) - 1);

                    for cmd in cmds.into_iter() {
                        // let a = **sep;
                        // expanded_form.push(*cmd);
                        // expanded_form.push(*sep);
                    }

                    // if let Some(cmd) = cmds.last() {
                    //     expanded_form.push(cmd);
                    // }

                    let a = array!(expanded_form);
                }

                _ => {}
            }
        }
    }

    fn remove_join_cmd(&self, doc: &mut BumpVec<'a, Command<'a>>) {}

    fn expand_join_cmd(&self, doc: &mut BumpVec<'a, Command<'a>>) {
        todo!()
        // self.expand_join_cmd_inner(doc);
        // self.remove_join_cmd(doc);
    }

    fn propagate_line_breaks_inner(
        &self,
        doc: &mut BumpVec<'_, Command>,
        propagate_hard_line: bool,
        propagate_break_parent: bool,
    ) -> bool {
        let mut break_all = false;

        for cmd in doc.iter_mut() {
            match cmd {
                Group(cmds, opts) => {
                    let propagate_line_break = self.propagate_line_breaks_inner(cmds, true, true);

                    opts._break_all_internal = propagate_line_break;

                    if propagate_line_break {
                        break_all = true;
                    }
                }
                Array(cmds, opts) => {
                    let propagate_line_break = self.propagate_line_breaks_inner(
                        cmds,
                        propagate_hard_line,
                        propagate_break_parent,
                    );

                    opts._break_all_internal = propagate_line_break;

                    if propagate_line_break {
                        break_all = true;
                    }
                }
                Indent(cmds, opts) => {
                    let propagate_line_break = self.propagate_line_breaks_inner(
                        cmds,
                        propagate_hard_line,
                        propagate_break_parent,
                    );

                    opts._break_all_internal = propagate_line_break;

                    if propagate_line_break {
                        break_all = true;
                    }
                }
                Dedent(cmds, opts) => {
                    let propagate_line_break = self.propagate_line_breaks_inner(
                        cmds,
                        propagate_hard_line,
                        propagate_break_parent,
                    );

                    opts._break_all_internal = propagate_line_break;

                    if propagate_line_break {
                        break_all = true;
                    }
                }
                Hardline => {
                    if propagate_hard_line {
                        break_all = true;
                    }
                }
                BreakParent => {
                    if propagate_break_parent {
                        break_all = true;
                    }
                }
                _ => {}
            }
        }

        break_all
    }

    fn propagate_line_breaks(&self, doc: &mut BumpVec<'_, Command>) {
        self.propagate_line_breaks_inner(doc, false, true);
    }

    fn force_group_break(&mut self, cmds: &mut BumpVec<'_, Command>, width: &mut usize) {
        let mut stack = Vec::new();
        stack.extend(cmds.iter_mut().rev());

        while !stack.is_empty() {
            let cmd = unsafe { stack.pop().unwrap_unchecked() };

            match cmd {
                Group(cmds, _) => {
                    stack.extend(cmds.iter_mut().rev());
                }
                Array(cmds, _) => {
                    stack.extend(cmds.iter_mut().rev());
                }
                Text(text) => {
                    *width += text.len();
                }
                Softline(opts) => {
                    opts._insert_line_internal = true;
                    *width = 0;
                }
                Line(opts) => {
                    opts._insert_line_internal = true;
                    *width = 0;
                }
                Hardline => {
                    *width = 0;
                }

                _ => {}
            }
        }
    }

    fn adjust_group_width(&mut self, cmd: &mut Command, width: &mut usize) {
        let mut parent_group_cmd;
        let mut parent_group_width;
        let mut stack = Vec::new();

        let break_line = match cmd {
            Group(cmds, opts) => {
                if opts._break_all_internal {
                    return self.force_group_break(cmds, width);
                } else {
                    opts.should_break
                }
            }
            _ => unsafe { core::hint::unreachable_unchecked() },
        };

        stack.push((cmd, break_line));

        while !stack.is_empty() {
            let (mut cmd, break_line) = unsafe { stack.pop().unwrap_unchecked() };
            parent_group_cmd = Some(&mut cmd);
            parent_group_width = *width;

            match cmd {
                Group(cmds, opts) => {
                    if opts._break_all_internal {
                        self.force_group_break(cmds, width);
                    } else {
                        if opts.should_break {
                            parent_group_cmd = None;
                            parent_group_width = 0;
                        }

                        stack.extend(cmds.iter_mut().rev().map(|cmd| (cmd, opts.should_break)));
                    }
                }
                Array(cmds, opts) => {
                    parent_group_cmd = None;
                    parent_group_width = 0;

                    stack.extend(
                        cmds.iter_mut()
                            .rev()
                            .map(|cmd| (cmd, opts._break_all_internal)),
                    );
                }
                Indent(cmds, opts) => {
                    parent_group_cmd = None;
                    parent_group_width = 0;

                    stack.extend(
                        cmds.iter_mut()
                            .rev()
                            .map(|cmd| (cmd, opts._break_all_internal)),
                    );
                }
                Dedent(cmds, opts) => {
                    parent_group_cmd = None;
                    parent_group_width = 0;

                    stack.extend(
                        cmds.iter_mut()
                            .rev()
                            .map(|cmd| (cmd, opts._break_all_internal)),
                    );
                }
                Softline(opts) => {
                    parent_group_cmd = None;
                    parent_group_width = 0;

                    if break_line {
                        opts._insert_line_internal = true;
                        *width = 0;
                    } else if *width > self.opts.width {
                        if let Some(parent_group) = parent_group_cmd {
                            match parent_group {
                                Group(_, opts) => {
                                    opts.should_break = true;
                                    *width = parent_group_width;
                                    return self
                                        .adjust_group_width(parent_group.deref_mut(), width);
                                }
                                _ => unsafe { core::hint::unreachable_unchecked() },
                            }
                        } else {
                            opts._insert_line_internal = true;
                            *width = 0;
                        }
                    }
                }
                Line(opts) => {
                    parent_group_cmd = None;
                    parent_group_width = 0;

                    if break_line {
                        opts._insert_line_internal = true;
                        *width = 0;
                    } else if *width > self.opts.width {
                        if let Some(parent_group) = parent_group_cmd {
                            match parent_group {
                                Group(_, opts) => {
                                    opts.should_break = true;
                                    *width = parent_group_width;
                                    return self
                                        .adjust_group_width(parent_group.deref_mut(), width);
                                }
                                _ => unsafe { core::hint::unreachable_unchecked() },
                            }
                        } else {
                            opts._insert_line_internal = true;
                            *width = 0;
                        }
                    }
                }
                Hardline => {
                    parent_group_cmd = None;
                    parent_group_width = 0;

                    *width = 0;
                }
                _ => {}
            }
        }
    }

    fn adjust_doc_width(&mut self, doc: &mut BumpVec<'_, Command>) {
        let mut stack = Vec::new();

        stack.extend(doc.iter_mut().rev().map(|cmd| (cmd, false)));

        let mut width: usize = 0;

        while !stack.is_empty() {
            let (cmd, break_line) = unsafe { stack.pop().unwrap_unchecked() };

            match cmd {
                Group(_, _) => self.adjust_group_width(cmd, &mut width),
                Array(cmds, opts) => {
                    stack.extend(
                        cmds.iter_mut()
                            .rev()
                            .map(|cmd| (cmd, opts._break_all_internal)),
                    );
                }
                Indent(cmds, opts) => {
                    stack.extend(
                        cmds.iter_mut()
                            .rev()
                            .map(|cmd| (cmd, opts._break_all_internal)),
                    );
                }
                Dedent(cmds, opts) => {
                    stack.extend(
                        cmds.iter_mut()
                            .rev()
                            .map(|cmd| (cmd, opts._break_all_internal)),
                    );
                }
                Text(text) => {
                    width += text.len();
                }
                Line(opts) => {
                    if break_line || width > self.opts.width {
                        opts._insert_line_internal = true;
                        width = 0;
                    } else {
                        width += 1;
                    }
                }
                Softline(opts) => {
                    if break_line || width > self.opts.width {
                        opts._insert_line_internal = true;
                        width = 0;
                    }
                }
                Hardline => {
                    width = 0;
                }

                _ => (),
            }
        }
    }

    fn print_doc(&mut self, doc: &BumpVec<'_, Command>) -> String {
        let mut result = String::new();

        let mut stack = Vec::new();
        stack.extend(doc.iter().rev().map(|cmd| (cmd, 0)));

        let gen_indent = |indentation| {
            let indentation = indentation * self.opts.indent_size;

            if self.opts.tabs {
                "\t".repeat(indentation)
            } else {
                " ".repeat(indentation)
            }
        };

        while !stack.is_empty() {
            let (cmd, indent) = unsafe { stack.pop().unwrap_unchecked() };

            match cmd {
                Group(cmds, _) => {
                    stack.extend(cmds.iter().rev().map(|cmd| (cmd, indent)));
                }
                Array(cmds, _) => {
                    stack.extend(cmds.iter().rev().map(|cmd| (cmd, indent)));
                }
                Indent(cmds, _) => {
                    stack.extend(cmds.iter().rev().map(|cmd| (cmd, indent + 1)));
                }
                Dedent(cmds, _) => {
                    stack.extend(cmds.iter().rev().map(|cmd| (cmd, (indent - 1).max(0))));
                }
                Text(text) => {
                    result.push_str(text);
                }
                Softline(opts) => {
                    if opts._insert_line_internal {
                        result.push('\n');
                        let indentation = gen_indent(indent);
                        result.push_str(indentation.as_str());
                    }
                }
                Line(opts) => {
                    if opts._insert_line_internal {
                        result.push('\n');
                        let indentation = gen_indent(indent);
                        result.push_str(indentation.as_str());
                    } else {
                        result.push(' ');
                    }
                }
                Hardline => {
                    result.push('\n');
                    let indentation = gen_indent(indent);
                    result.push_str(indentation.as_str());
                }
                _ => {}
            }
        }

        result
    }

    pub fn print(&mut self, doc: &'b mut BumpVec<'_, Command<'c>>) -> String {
        self.propagate_line_breaks(doc);
        self.adjust_doc_width(doc);

        self.print_doc(doc)
    }

    pub fn new(arena: &'a Bump, fmt_opts: FormatterOptions) -> Self {
        Printer {
            arena,
            opts: fmt_opts,
        }
    }
}
