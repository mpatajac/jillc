use crate::common::ast;

use super::{
    context::{ModuleContext, ProgramContext},
    vm,
};

mod literal {
    use super::{ast, vm, ModuleContext, ProgramContext};

    pub fn construct(
        literal: &ast::JillLiteral,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) -> vm::VMInstructionBlock {
        match literal {
            ast::JillLiteral::Integer(i) => construct_integer(i),
            ast::JillLiteral::String(s) => construct_string(s),
            ast::JillLiteral::Bool(b) => construct_bool(b),
        }
    }

    pub(super) fn construct_integer(i: &isize) -> vm::VMInstructionBlock {
        let mut instructions = vec![vm::push(vm::Segment::Constant, i.unsigned_abs())];

        if *i < 0 {
            instructions.push(vm::command(vm::VMCommand::Neg));
        }

        instructions.into()
    }

    pub(super) fn construct_string(s: &str) -> vm::VMInstructionBlock {
        let string_init = vec![
            vm::push(vm::Segment::Constant, s.len()),
            vm::call("String.new", 1),
        ];

        let string_population = s
            .chars()
            .flat_map(|c| {
                vec![
                    vm::push(vm::Segment::Constant, to_ascii(c)),
                    vm::call("String.appendChar", 2),
                ]
            })
            .collect();

        [string_init, string_population].concat().into()
    }

    pub(super) fn construct_bool(b: &bool) -> vm::VMInstructionBlock {
        if *b {
            vec![
                vm::push(vm::Segment::Constant, 1),
                vm::command(vm::VMCommand::Neg),
            ]
        } else {
            vec![vm::push(vm::Segment::Constant, 0)]
        }
        .into()
    }

    fn to_ascii(c: char) -> usize {
        (c as u8).into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_positive_integer_construction() {
        let i = 17;

        let expected = "push constant 17";

        assert_eq!(literal::construct_integer(&i).compile(), expected);
    }

    #[test]
    fn test_negative_integer_construction() {
        let i = -28;

        let expected = ["push constant 28", "neg"].join("\n");

        assert_eq!(literal::construct_integer(&i).compile(), expected);
    }

    #[test]
    fn test_string_construction() {
        let s = String::from("fin");

        let expected = [
            "push constant 3",
            "call String.new 1",
            "push constant 102",
            "call String.appendChar 2",
            "push constant 105",
            "call String.appendChar 2",
            "push constant 110",
            "call String.appendChar 2",
        ]
        .join("\n");

        assert_eq!(literal::construct_string(&s).compile(), expected);
    }
}
