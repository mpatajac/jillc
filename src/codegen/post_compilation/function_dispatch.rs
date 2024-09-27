use crate::{
    codegen::{
        context::{program::FunctionReferenceIndex, ProgramContext},
        vm,
    },
    fileio::output::OutputFile,
};

pub fn construct(program_context: &mut ProgramContext) -> Option<OutputFile> {
    let functions_to_dispatch = program_context.function_dispatch.collect();

    if functions_to_dispatch.is_empty() {
        // no functions to dispatch - no need to generate anything
        return None;
    }

    let instruction_block: vm::VMInstructionBlock = [
        construct_new(),
        construct_call(functions_to_dispatch, program_context),
    ]
    .concat()
    .into();

    Some(OutputFile::new(
        String::from("Fn"),
        instruction_block.compile(),
    ))
}

fn construct_new() -> Vec<vm::VMInstruction> {
    todo!()
}

fn construct_call(
    functions_to_dispatch: Vec<(vm::VMFunctionName, FunctionReferenceIndex)>,
    program_context: &mut ProgramContext,
) -> Vec<vm::VMInstruction> {
    // TOOD: test
    todo!()
}
