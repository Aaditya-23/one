
   
    
    fn build_from_function_params(&self, params: &BumpVec<'_, FunctionParams<'a>>) -> Command<'a> {
        let mut indent_cmd = vec![];
        indent_cmd.push(softline!());

        let it = params
            .iter()
            .take((params.len() as isize - 1).max(0) as usize);

        let from_array_pattern = |param: &FunctionParams<'a>| -> Command<'a> {
            match param {
                FunctionParams::Pattern(p) => self.build_from_pattern(p),
                FunctionParams::RestElement(rest_el) => {
                    array!(vec![
                        text!("..."),
                        self.build_from_pattern(&rest_el.argument)
                    ])
                }
            }
        };

        for param in it {
            indent_cmd.push(from_array_pattern(param));
            indent_cmd.push(text!(","));
            indent_cmd.push(line!());
        }

        if let Some(last_param) = params.last() {
            indent_cmd.push(from_array_pattern(last_param));
        }

        array!(vec![
            text!("("),
            indent!(indent_cmd),
            softline!(),
            text!(")")
        ])
    }

    fn build_from_function(&self, exp: &Function<'a>) -> Command<'a> {
        let mut array_cmd = vec![];

        if exp.async_ {
            array_cmd.push(text!("async "));
        }

        array_cmd.push(text!("function"));

        if exp.generator {
            array_cmd.push(text!("*"));
        }

        array_cmd.push(text!(" "));

        if let Some(id) = &exp.id {
            array_cmd.push(self.build_from_identifier(id));
        }

        let params = self.build_from_function_params(&exp.params);
        array_cmd.push(params);

        array_cmd.push(text!(" {"));

        let mut indent_cmd = vec![hardline!()];
        let mut join_cmds = vec![];

        for statement in exp.body.body.iter() {
            join_cmds.push(self.build_from_statement(statement));
        }

        indent_cmd.push(join!(hardline!(), join_cmds));
        array_cmd.push(indent!(indent_cmd));

        array_cmd.push(hardline!());
        array_cmd.push(text!("}"));

        array!(array_cmd)
    }

    fn build_from_arrow_function(&self, exp: &ArrowFunction<'a>) -> Command<'a> {
        let mut array_cmd = vec![];

        if exp.async_ {
            array_cmd.push(text!("async "));
        }

        let params = self.build_from_function_params(&exp.params);
        array_cmd.push(params);

        array_cmd.push(text!(" => "));

        array_cmd.push(text!("{"));
        array_cmd.push(hardline!());

        match &exp.body {
            ArrowFunctionBody::Expression(exp) => {
                array_cmd.push(self.build_from_expression(exp));
                array_cmd.push(hardline!());
            }
            ArrowFunctionBody::Block(exp) => {
                for statement in exp.body.iter() {
                    array_cmd.push(self.build_from_statement(statement));
                    array_cmd.push(hardline!());
                }
            }
        }

        array_cmd.push(text!("}"));

        array!(array_cmd)
    }
