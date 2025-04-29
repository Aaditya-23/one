 fn build_from_binary_exp(&self, exp: &BinaryExpression<'a>) -> Command<'a> {
        let left = self.build_from_expression(&exp.left);
        let right = self.build_from_expression(&exp.right);

        group!(vec![
            left,
            softline!(),
            text!(exp.operator.as_str()),
            line!(),
            right
        ])
    }

    fn build_from_logical_exp(&self, exp: &LogicalExpression<'a>) -> Command<'a> {
        let left = self.build_from_expression(&exp.left);
        let right = self.build_from_expression(&exp.right);

        group!(vec![
            left,
            softline!(),
            text!(exp.operator.as_str()),
            line!(),
            right
        ])
    }
