use crate::{
    codegen::{context::ProgramContext, vm, GLOBALS_INIT_FN_NAME},
    fileio::output::OutputFile,
};

pub fn construct(program_context: &mut ProgramContext) -> Option<OutputFile> {
    if program_context.globals.is_empty() {
        // no globals in any module - no need to generate anything
        return None;
    }

    let globals_init_call_instructions = program_context
        .globals
        .iter()
        .map(|module_name| {
            let vm_function_name =
                vm::VMFunctionName::construct(module_name, "", GLOBALS_INIT_FN_NAME);

            vec![vm::call(vm_function_name, 0), vm::pop(vm::Segment::Temp, 0)]
        })
        .collect::<Vec<_>>()
        .concat();

    let instruction_block: vm::VMInstructionBlock = [
        vec![vm::function(
            vm::VMFunctionName::from_literal("Globals.init"),
            0,
        )],
        globals_init_call_instructions,
        vec![vm::push(vm::Segment::Constant, 0), vm::vm_return()],
    ]
    .concat()
    .into();

    Some(OutputFile::new(
        String::from("Globals"),
        instruction_block.compile(),
    ))
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;

    #[test]
    fn test_globals_init_construction() {
        let mut program_context = ProgramContext::new();

        program_context.globals.insert(String::from("Foo"));
        program_context.globals.insert(String::from("Mod_One"));
        program_context.globals.insert(String::from("Mod_Two"));
        program_context.globals.insert(String::from("Mod"));

        let expected = [
            "function Globals.init 0",
            &format!("call Foo.{GLOBALS_INIT_FN_NAME} 0"),
            "pop temp 0",
            &format!("call Mod_One.{GLOBALS_INIT_FN_NAME} 0"),
            "pop temp 0",
            &format!("call Mod_Two.{GLOBALS_INIT_FN_NAME} 0"),
            "pop temp 0",
            &format!("call Mod.{GLOBALS_INIT_FN_NAME} 0"),
            "pop temp 0",
            "push constant 0",
            "return",
        ];

        let output = construct(&mut program_context).expect("should be Some(_)");

        // since the output is built from hashset elements, we can't
        // directly compare to expected output (order is not guaranteed)

        let output_items: Vec<_> = output.content().split('\n').collect();

        // same amount of lines
        assert_eq!(output_items.len(), expected.len());

        let expected_items = HashSet::from(expected);

        let mut matches = HashSet::with_capacity(expected_items.len());

        // no line that isn't expected
        for item in output_items {
            assert!(expected_items.contains(&item));

            matches.insert(item);
        }

        // ALL expected lines (no duplicates)
        assert_eq!(matches.len(), expected_items.len());
    }

    #[test]
    fn test_no_globals() {
        let mut program_context = ProgramContext::new();

        assert_eq!(
            construct(&mut program_context)
                .as_ref()
                .map(OutputFile::content),
            None
        );
    }
}
