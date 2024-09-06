use crate::{
    codegen::{
        context::{ModuleContext, ProgramContext},
        vm,
    },
    common::ast,
};

pub fn construct(
    literal: &ast::JillLiteral,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> Vec<vm::VMInstruction> {
    match literal {
        ast::JillLiteral::Integer(i) => construct_integer(i),
        ast::JillLiteral::String(s) => construct_string(s),
        ast::JillLiteral::Bool(b) => construct_bool(b),
        ast::JillLiteral::List(l) => construct_list(l, module_context, program_context),
    }
}

fn construct_integer(i: &isize) -> Vec<vm::VMInstruction> {
    let mut instructions = vec![vm::push(vm::Segment::Constant, i.unsigned_abs())];

    if *i < 0 {
        instructions.push(vm::command(vm::VMCommand::Neg));
    }

    instructions
}

fn construct_string(s: &str) -> Vec<vm::VMInstruction> {
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

    [string_init, string_population].concat()
}

fn construct_bool(b: &bool) -> Vec<vm::VMInstruction> {
    if *b {
        vec![
            vm::push(vm::Segment::Constant, 1),
            vm::command(vm::VMCommand::Neg),
        ]
    } else {
        vec![vm::push(vm::Segment::Constant, 0)]
    }
}

fn construct_list(
    l: &Vec<ast::JillExpression>,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> Vec<vm::VMInstruction> {
    // start with an empty list
    let empty_list = vec![vm::call("List.Empty", 0)];

    if l.is_empty() {
        // no elements, just return the empty list
        return empty_list;
    };

    let instructions = l
        .iter()
        // need to add elements in reverse order
        .rev()
        .flat_map(|elem| {
            [
                // move last result to temporary storage
                vec![vm::pop(vm::Segment::Temp, 0)],
                // evaluate next elem
                super::expression::construct(elem, module_context, program_context),
                vec![
                    // re-push previous result (to maintain proper element order)
                    vm::push(vm::Segment::Temp, 0),
                    // construct new list "head"
                    vm::call("List.List", 2),
                ],
            ]
            .concat()
        })
        .collect();

    [empty_list, instructions].concat()
}

fn to_ascii(c: char) -> usize {
    (c as u8).into()
}

#[cfg(test)]
mod tests {
    use crate::codegen::vm;

    #[test]
    fn test_positive_integer_construction() {
        let i = 17;

        let expected = "push constant 17";

        assert_eq!(
            vm::VMInstructionBlock::from(super::construct_integer(&i)).compile(),
            expected
        );
    }

    #[test]
    fn test_negative_integer_construction() {
        let i = -28;

        let expected = ["push constant 28", "neg"].join("\n");

        assert_eq!(
            vm::VMInstructionBlock::from(super::construct_integer(&i)).compile(),
            expected
        );
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

        assert_eq!(
            vm::VMInstructionBlock::from(super::construct_string(&s)).compile(),
            expected
        );
    }

    #[test]
    fn test_list_construction() {
        // TODO!: when expression is implemented
    }
}
