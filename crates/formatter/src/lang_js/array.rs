  fn build_from_array_exp(&self, exp: &ArrayExpression<'a>) -> Command<'a> {
        let mut array_cmd = vec![];

        let it = exp
            .elements
            .iter()
            .take((exp.elements.len() as isize - 1).max(0) as usize);

        for el in it {
            array_cmd.push(self.build_from_expression(el));
            array_cmd.push(text!(","));
            array_cmd.push(line!());
        }

        if let Some(last_el) = exp.elements.last() {
            array_cmd.push(self.build_from_expression(last_el));
        }

        group!(vec![
            text!("["),
            indent!(vec![softline!(), array!(array_cmd)]),
            softline!(),
            text!("]")
        ])
    }
