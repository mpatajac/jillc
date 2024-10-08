use crate::{
    codegen::{
        context::{module::VariableContext, ModuleContext, ProgramContext},
        error::FallableInstructions,
        vm,
    },
    common::ast,
};

pub fn construct(
    literal: &ast::JillLiteral,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableInstructions {
    let instructions = match literal {
        ast::JillLiteral::Integer(i) => construct_integer(i),
        ast::JillLiteral::String(s) => construct_string(s),
        ast::JillLiteral::Bool(b) => construct_bool(b),
        ast::JillLiteral::List(l) => construct_list(l, module_context, program_context)?,
    };

    Ok(instructions)
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
        vm::call(vm::VMFunctionName::from_literal("String.new"), 1),
    ];

    let string_population = s
        .chars()
        .flat_map(|c| {
            vec![
                vm::push(vm::Segment::Constant, to_ascii(c)),
                vm::call(vm::VMFunctionName::from_literal("String.appendChar"), 2),
            ]
        })
        .collect();

    [string_init, string_population].concat()
}

fn construct_bool(b: &bool) -> Vec<vm::VMInstruction> {
    if *b {
        vm::r#true()
    } else {
        vec![vm::r#false()]
    }
}

fn construct_list(
    list: &Vec<ast::JillExpression>,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableInstructions {
    note_std_list_constructor_usage("Empty", program_context);

    // start with an empty list
    let empty_list = vec![vm::call(vm::VMFunctionName::from_literal("List.Empty"), 0)];

    if list.is_empty() {
        // no elements, just return the empty list
        return Ok(empty_list);
    };

    note_std_list_constructor_usage("List", program_context);

    let temp_index = program_context.temp_segment_index.request();
    let temp_storage = VariableContext {
        segment: vm::Segment::Temp,
        index: temp_index,
    };
    let instructions = list
        .iter()
        // need to add elements in reverse order
        .rev()
        .map(|elem| {
            Ok([
                // move last result to temporary storage
                vec![temp_storage.pop()],
                // evaluate next elem
                super::expression::construct(elem, module_context, program_context)?,
                // re-push previous result (to maintain proper element order)
                temp_storage.push(),
                // construct new list "head"
                vec![vm::call(vm::VMFunctionName::from_literal("List.List"), 2)],
            ]
            .concat())
        })
        .collect::<Result<Vec<Vec<_>>, _>>()?
        .concat();

    program_context.temp_segment_index.release();

    Ok([empty_list, instructions].concat())
}

fn note_std_list_constructor_usage(constructor: &str, program_context: &mut ProgramContext) {
    let function_reference = ast::JillFunctionReference {
        modules_path: vec![ast::JillIdentifier(String::from("List"))],
        associated_type: None,
        function_name: ast::JillIdentifier(constructor.to_owned()),
    };

    program_context
        .std_usage_tracker
        .note_usage(&function_reference);
}

fn to_ascii(c: char) -> usize {
    (c as u8).into()
}

#[cfg(test)]
mod tests {
    use crate::{
        codegen::{
            context::{ModuleContext, ProgramContext},
            vm,
        },
        common::ast,
    };

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
    fn test_basic_list_construction() {
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new("Test".to_owned());

        let number = |i| ast::JillExpression::Literal(ast::JillLiteral::Integer(i));

        let list = vec![number(5), number(2), number(7), number(3)];

        let expected = [
            "call List.Empty 0",
            "pop temp 0",
            "push constant 3",
            "push temp 0",
            "call List.List 2",
            "pop temp 0",
            "push constant 7",
            "push temp 0",
            "call List.List 2",
            "pop temp 0",
            "push constant 2",
            "push temp 0",
            "call List.List 2",
            "pop temp 0",
            "push constant 5",
            "push temp 0",
            "call List.List 2",
        ]
        .join("\n");

        assert!(
            super::construct_list(&list, &mut module_context, &mut program_context).is_ok_and(
                |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
            )
        );
    }

    #[test]
    fn test_nested_list_construction() {
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new("Test".to_owned());

        let number = |i| ast::JillExpression::Literal(ast::JillLiteral::Integer(i));
        let list = |l| ast::JillExpression::Literal(ast::JillLiteral::List(l));

        // [[5, 2], [7, 3]]
        let lists = vec![
            list(vec![number(5), number(2)]),
            list(vec![number(7), number(3)]),
        ];

        let inner_list_1 = vec![
            "call List.Empty 0",
            "pop temp 1",
            "push constant 2",
            "push temp 1",
            "call List.List 2",
            "pop temp 1",
            "push constant 5",
            "push temp 1",
            "call List.List 2",
        ];

        let inner_list_2 = vec![
            "call List.Empty 0",
            "pop temp 1",
            "push constant 3",
            "push temp 1",
            "call List.List 2",
            "pop temp 1",
            "push constant 7",
            "push temp 1",
            "call List.List 2",
        ];

        let expected = [
            vec!["call List.Empty 0", "pop temp 0"],
            inner_list_2,
            vec!["push temp 0", "call List.List 2", "pop temp 0"],
            inner_list_1,
            vec!["push temp 0", "call List.List 2"],
        ]
        .concat()
        .join("\n");

        assert!(
            super::construct_list(&lists, &mut module_context, &mut program_context).is_ok_and(
                |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
            )
        );
    }
}
