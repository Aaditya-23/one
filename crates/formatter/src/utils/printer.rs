use crate::array;

use super::commands::{Command::*, *};

enum PrintInstruction {
    GroupForceBreak,
    GroupBreak,
    LineBreak,
    BreakCommand,
    None,
}

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
            width: 50,
            tabs: false,
            indent_size: 2,
            semicolons: true,
            single_quotes: false,
        }
    }
}

pub struct Printer {
    opts: FormatterOptions,
}

impl<'a> Printer {
    fn expand_join_cmd(&self, doc: &mut Vec<Command>) {
        let mut stack = Vec::new();
        stack.extend(doc.iter_mut().rev());

        while !stack.is_empty() {
            let cmd = unsafe { stack.pop().unwrap_unchecked() };

            match cmd {
                Group(cmds, _) => {
                    stack.extend(cmds.iter_mut().rev());
                }
                IfBreak(break_cmd, flat_cmd, _) => {
                    stack.extend(flat_cmd.iter_mut().rev());
                    stack.extend(break_cmd.iter_mut().rev());
                }
                Array(cmds) => {
                    stack.extend(cmds.iter_mut().rev());
                }
                Indent(cmds) => {
                    stack.extend(cmds.iter_mut().rev());
                }
                Dedent(cmds) => {
                    stack.extend(cmds.iter_mut().rev());
                }
                Join(separator, cmds) => {
                    let mut array_cmd = vec![];

                    let mut it = cmds.iter();

                    if let Some(cmd_inner) = it.next() {
                        array_cmd.push(cmd_inner.clone());
                    }

                    for cmd_inner in it {
                        array_cmd.push(*separator.clone());
                        array_cmd.push(cmd_inner.clone());
                    }

                    *cmd = array!(array_cmd);
                    stack.push(cmd);
                }
                _ => {}
            }
        }
    }

    fn gen_print_instructions(&self, doc: &Vec<Command>) -> Vec<PrintInstruction> {
        let mut print_instructions = vec![];
        let mut stack = Vec::new();
        stack.extend(doc.iter().rev());

        while !stack.is_empty() {
            let cmd = unsafe { stack.pop().unwrap_unchecked() };
            print_instructions.push(PrintInstruction::None);

            match cmd {
                Group(cmds, opts) => {
                    stack.extend(cmds.iter().rev());
                }
                // IfBreak(break_cmd, flat_cmd, opts) => {}
                Array(cmds) => {
                    stack.extend(cmds.iter().rev());
                }
                Indent(cmds) => {
                    stack.extend(cmds.iter().rev());
                }
                Dedent(cmds) => {
                    stack.extend(cmds.iter().rev());
                }
                _ => {}
            }
        }

        print_instructions
    }

    fn propagate_group_line_breaks(
        &self,
        cmds: &Vec<Command>,
        print_instructions: &mut Vec<PrintInstruction>,
        instructions_index: &mut usize,
    ) -> bool {
        let mut force_group_break = false;

        let mut stack = Vec::new();
        stack.extend(cmds.iter().rev());

        while !stack.is_empty() {
            let cmd = unsafe { stack.pop().unwrap_unchecked() };
            *instructions_index += 1;

            match cmd {
                Group(cmds, opts) => {
                    let temp = *instructions_index;
                    if self.propagate_group_line_breaks(
                        cmds,
                        print_instructions,
                        instructions_index,
                    ) {
                        print_instructions[temp] = PrintInstruction::GroupForceBreak;
                        force_group_break = true;
                    } else if opts.should_break {
                        print_instructions[temp] = PrintInstruction::GroupBreak;
                    }
                }
                Array(cmds) => {
                    cmds.iter().for_each(|cmd| {
                        if let BreakParent = cmd {
                            print_instructions[*instructions_index] =
                                PrintInstruction::BreakCommand;
                        }
                    });

                    stack.extend(cmds.iter().rev())
                }
                Indent(cmds) => {
                    cmds.iter().for_each(|cmd| {
                        if let BreakParent = cmd {
                            print_instructions[*instructions_index] =
                                PrintInstruction::BreakCommand;
                        }
                    });

                    stack.extend(cmds.iter().rev())
                }
                Dedent(cmds) => {
                    cmds.iter().for_each(|cmd| {
                        if let BreakParent = cmd {
                            print_instructions[*instructions_index] =
                                PrintInstruction::BreakCommand;
                        }
                    });

                    stack.extend(cmds.iter().rev())
                }
                Hardline => {
                    force_group_break = true;
                }
                BreakParent => {
                    force_group_break = true;
                }

                _ => {}
            }
        }

        force_group_break
    }

    fn propagate_line_breaks(
        &self,
        doc: &Vec<Command>,
        print_instructions: &mut Vec<PrintInstruction>,
    ) {
        let mut stack = Vec::new();
        let mut instructions_index = 0;

        stack.extend(doc.iter().rev());

        while !stack.is_empty() {
            let cmd = unsafe { stack.pop().unwrap_unchecked() };

            match cmd {
                Group(cmds, _) => {
                    let temp = instructions_index;
                    if self.propagate_group_line_breaks(
                        cmds,
                        print_instructions,
                        &mut instructions_index,
                    ) {
                        print_instructions[temp] = PrintInstruction::GroupForceBreak
                    }
                }
                Array(cmds) => {
                    cmds.iter().for_each(|cmd| {
                        if let BreakParent = cmd {
                            print_instructions[instructions_index] = PrintInstruction::BreakCommand;
                        }
                    });

                    stack.extend(cmds.iter().rev());
                }
                Indent(cmds) => {
                    cmds.iter().for_each(|cmd| {
                        if let BreakParent = cmd {
                            print_instructions[instructions_index] = PrintInstruction::BreakCommand;
                        }
                    });

                    stack.extend(cmds.iter().rev());
                }
                Dedent(cmds) => {
                    cmds.iter().for_each(|cmd| {
                        if let BreakParent = cmd {
                            print_instructions[instructions_index] = PrintInstruction::BreakCommand;
                        }
                    });

                    stack.extend(cmds.iter().rev());
                }

                _ => {}
            }

            instructions_index += 1;
        }
    }

    fn break_group(
        &self,
        cmds: &Vec<Command>,
        print_instructions: &mut Vec<PrintInstruction>,
        instructions_index: &mut usize,
        width: &mut usize,
        break_all: bool,
    ) {
        let mut stack = Vec::new();
        stack.extend(cmds.iter().rev());

        while !stack.is_empty() {
            let cmd = unsafe { stack.pop().unwrap_unchecked() };
            *instructions_index += 1;

            match cmd {
                Group(cmds, _) => {
                    if break_all {
                        stack.extend(cmds.iter().rev());
                    } else {
                        // self.adjust_group_width(group_cmd, width);
                    }
                }
                Array(cmds) => {
                    stack.extend(cmds.iter().rev());
                }
                Indent(cmds) => {
                    stack.extend(cmds.iter().rev());
                }
                Dedent(cmds) => {
                    stack.extend(cmds.iter().rev());
                }
                Softline | Line => {
                    *width = 0;
                    print_instructions[*instructions_index] = PrintInstruction::LineBreak;
                }
                Hardline => {
                    *width = 0;
                }

                _ => {}
            }
        }
    }

    fn adjust_group_width(
        &self,
        group_cmd: &Command,
        print_instructions: &mut Vec<PrintInstruction>,
        instructions_index: &mut usize,
        width: &mut usize,
    ) {
        let mut stack = Vec::new();
        let (temp_instructions_index, temp_width) = (*instructions_index, *width);

        match group_cmd {
            Group(cmds, _) => {
                stack.extend(cmds.iter().rev());
            }
            _ => unsafe { core::hint::unreachable_unchecked() },
        }

        while !stack.is_empty() {
            let cmd = unsafe { stack.pop().unwrap_unchecked() };
            *instructions_index += 1;

            match cmd {
                Group(cmds, _) => {
                    match print_instructions[*instructions_index] {
                        PrintInstruction::GroupForceBreak => {
                            self.break_group(cmds, print_instructions, instructions_index, width, true);
                        }
                        PrintInstruction::GroupBreak => {
                            self.break_group(cmds, print_instructions, instructions_index, width, false);
                        }
                        _ => stack.extend(cmds.iter().rev()),
                    }
                    
                }
                Array(cmds) => {
                    stack.extend(cmds.iter().rev());
                }
                Indent(cmds) => {
                    stack.extend(cmds.iter().rev());
                }
                Dedent(cmds) => {
                    stack.extend(cmds.iter().rev());
                }
                Text(txt) => {
                    *width += txt.len();
                }
                Softline | Line => {
                    if *width > self.opts.width {
                        *width  = temp_width;
                        *instructions_index = temp_instructions_index;

                        let cmds = match group_cmd {
                            Group(cmds, _) => cmds,
                            _ => unsafe { core::hint::unreachable_unchecked() },
                        };
                        
                        return self.break_group(cmds, print_instructions, instructions_index, width, false);
                    }
                }
                _ => {}
            }
            
        }
        
        
    }

    fn adjust_doc_width(&self, doc: &Vec<Command>, print_instructions: &mut Vec<PrintInstruction>) {
        let mut stack = Vec::new();
        let (mut width, mut instructions_index) = (0, 0);

        stack.extend(doc.iter().rev().map(|cmd| (cmd, false)));

        while !stack.is_empty() {
            let (cmd, break_line) = unsafe { stack.pop().unwrap_unchecked() };

            match cmd {
                Group(cmds, _) => match print_instructions[instructions_index] {
                    PrintInstruction::GroupForceBreak => {
                        self.break_group(
                            cmds,
                            print_instructions,
                            &mut instructions_index,
                            &mut width,
                            true,
                        );
                    }
                    PrintInstruction::GroupBreak => {
                        self.break_group(
                            cmds,
                            print_instructions,
                            &mut instructions_index,
                            &mut width,
                            false,
                        );
                    }
                    _ => {
                        self.adjust_group_width(cmd, print_instructions, &mut instructions_index, &mut width);
                    }
                },
                Array(cmds) => {
                    let break_cmd;

                    if let PrintInstruction::BreakCommand = print_instructions[instructions_index] {
                        break_cmd = true;
                    } else {
                        break_cmd = false;
                    }

                    stack.extend(cmds.iter().rev().map(|cmd| (cmd, break_cmd)));
                }
                Indent(cmds) => {
                    let break_cmd;

                    if let PrintInstruction::BreakCommand = print_instructions[instructions_index] {
                        break_cmd = true;
                    } else {
                        break_cmd = false;
                    }

                    stack.extend(cmds.iter().rev().map(|cmd| (cmd, break_cmd)));
                }
                Dedent(cmds) => {
                    let break_cmd;

                    if let PrintInstruction::BreakCommand = print_instructions[instructions_index] {
                        break_cmd = true;
                    } else {
                        break_cmd = false;
                    }

                    stack.extend(cmds.iter().rev().map(|cmd| (cmd, break_cmd)));
                }
                Text(txt) => {
                    width += txt.len();
                }
                Softline | Line => {
                    if break_line {
                        width = 0;
                        print_instructions[instructions_index] = PrintInstruction::LineBreak
                    } else if width > self.opts.width {
                        width = 0;
                        print_instructions[instructions_index] = PrintInstruction::LineBreak
                    }
                }
                Hardline => {
                    width = 0;
                }

                _ => {}
            }

            instructions_index += 1;
        }
    }

    fn print_doc(&self, doc: &Vec<Command>, print_instructions: &Vec<PrintInstruction>) -> String {
        let mut result = String::new();

        let mut stack = Vec::new();
        let mut instructions_index = 0;
        
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
                Array(cmds) => {
                    stack.extend(cmds.iter().rev().map(|cmd| (cmd, indent)));
                }
                Indent(cmds) => {
                    stack.extend(cmds.iter().rev().map(|cmd| (cmd, indent + 1)));
                }
                Dedent(cmds) => {
                    stack.extend(cmds.iter().rev().map(|cmd| (cmd, (indent - 1).max(0))));
                }
                Text(text) => {
                    result.push_str(text);
                }
                Softline => {
                    if let PrintInstruction::LineBreak = print_instructions[instructions_index] {
                        result.push('\n');
                        let indentation = gen_indent(indent);
                        result.push_str(indentation.as_str());
                    }
                }
                Line => {
                    if let PrintInstruction::LineBreak = print_instructions[instructions_index] {
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
        
            instructions_index += 1;
        }

        result
    }

    pub fn print(&self, doc: &mut Vec<Command>) -> String {
        self.expand_join_cmd(doc);

        let mut print_instructions = self.gen_print_instructions(doc);

        self.propagate_line_breaks(doc, &mut print_instructions);
        self.adjust_doc_width(doc, &mut print_instructions);
        self.print_doc(doc, &print_instructions)
    }

    pub fn new(fmt_opts: FormatterOptions) -> Self {
        Printer { opts: fmt_opts }
    }
}
