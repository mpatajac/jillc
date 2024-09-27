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
    vec![
        vm::function(vm::VMFunctionName::from_literal("Fn._new"), 0),
        // we store two fields in closure object
        vm::push(vm::Segment::Constant, 2),
        vm::call(vm::VMFunctionName::from_literal("Memory.alloc"), 1),
        vm::pop(vm::Segment::Pointer, 0),
        // fid
        vm::push(vm::Segment::Argument, 0),
        vm::pop(vm::Segment::This, 0),
        // captures array
        vm::push(vm::Segment::Argument, 1),
        vm::pop(vm::Segment::This, 1),
        // return constructed object
        vm::push(vm::Segment::Pointer, 0),
        vm::vm_return(),
    ]
}

fn construct_call(
    functions_to_dispatch: Vec<(vm::VMFunctionName, FunctionReferenceIndex)>,
    program_context: &mut ProgramContext,
) -> Vec<vm::VMInstruction> {
    // TOOD: test
    todo!()
}
