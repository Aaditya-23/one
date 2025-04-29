
    pub fn build_from_obj_exp_property(
        &self,
        property_kind: &ObjectExpressionPropertyKind<'a>,
    ) -> Command<'a> {
        match property_kind {
            ObjectExpressionPropertyKind::Property(p) => {
                if p.computed {
                    array!(vec![
                        text!("["),
                        self.build_from_expression(&p.key),
                        text!(": "),
                        self.build_from_expression(&p.value),
                        text!("]")
                    ])
                } else {
                    let mut array_cmd = vec![self.build_from_expression(&p.key)];

                    if !p.shorthand {
                        array_cmd.push(text!(": "));
                        array_cmd.push(self.build_from_expression(&p.value));
                    }

                    array!(array_cmd)
                }
            }
            ObjectExpressionPropertyKind::SpreadElement(spread_el) => array!(vec![
                text!("..."),
                self.build_from_expression(&spread_el.argument)
            ]),
        }
    }

    fn build_from_object_exp(&self, exp: &ObjectExpression<'a>) -> Command<'a> {
        let mut array_cmd = vec![];

        let it = exp
            .properties
            .iter()
            .take((exp.properties.len() as isize - 1).max(0) as usize);

        for property_kind in it {
            let cmd = self.build_from_obj_exp_property(property_kind);

            array_cmd.push(cmd);
            array_cmd.push(text!(","));
            array_cmd.push(line!());
        }

        if let Some(property_kind) = exp.properties.last() {
            array_cmd.push(self.build_from_obj_exp_property(property_kind));
        }

        group!(vec![
            text!("{"),
            indent!(vec![softline!(), array!(array_cmd)]),
            softline!(),
            text!("}")
        ])
    }
