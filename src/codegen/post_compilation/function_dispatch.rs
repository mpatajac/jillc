use crate::{
    codegen::{
        context::{module::VariableContext, program::FunctionReferenceIndex, ProgramContext},
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

    // NOTE: since this function is completely unrelated the rest of the program,
    // we will use TEMP 7 (last one) to prevent any possible overwrites (just in case)
    let arg_counter_storage = VariableContext {
        segment: vm::Segment::Temp,
        index: 7,
    };

    let vm_function_name = vm::VMFunctionName::from_literal("Fn._call");

    let closure_as_this = vec![
        vm::push(vm::Segment::Argument, 0),
        vm::pop(vm::Segment::Pointer, 0),
    ];

    let arguments_construction = vec![
        vm::push(vm::Segment::Constant, 0),
        arg_counter_storage.pop(),
        vm::label(vm::LabelAction::Label, "ARGS_INIT_START"),
        // section: condition check
        // counter
        arg_counter_storage.push()[0].clone(),
        // arity
        vm::push(vm::Segment::Argument, 1),
        vm::command(vm::VMCommand::Eq),
        vm::label(vm::LabelAction::IfGoto, "ARGS_INIT_END"),
        // section: loop body
        // counter
        arg_counter_storage.push()[0].clone(),
        // arguments
        vm::push(vm::Segment::Argument, 2),
        vm::command(vm::VMCommand::Add),
        // use as array (THAT segment)
        vm::pop(vm::Segment::Pointer, 1),
        vm::push(vm::Segment::That, 0),
        // increase counter
        arg_counter_storage.push()[0].clone(),
        vm::push(vm::Segment::Constant, 1),
        vm::command(vm::VMCommand::Add),
        arg_counter_storage.pop(),
        // repeat loop
        vm::label(vm::LabelAction::Goto, "ARGS_INIT_START"),
        vm::label(vm::LabelAction::Label, "ARGS_INIT_END"),
    ];

    let enumerated_functions = functions_to_dispatch.iter().enumerate();

    let dispatch_jumps = enumerated_functions
        .clone()
        .map(|(i, (_, fid))| {
            vec![
                // closure's fid
                vm::push(vm::Segment::This, 0),
                // currently-checked fid
                vm::push(vm::Segment::Constant, *fid),
                vm::command(vm::VMCommand::Eq),
                // jump to associated call
                vm::label(vm::LabelAction::IfGoto, format!("FN_{i}")),
            ]
        })
        .collect::<Vec<_>>()
        .concat();

    let dispatch_calls = enumerated_functions
        .map(|(i, (vm_function_name, _))| {
            let function_arity = program_context
                .program_metadata
                .get_function_arity(vm_function_name)
                .expect("arities of all used functions should have been logged");

            vec![
                // jump label
                vm::label(vm::LabelAction::Label, format!("FN_{i}")),
                // directly call associated function
                // NOTE: call with `arity` + 1 arguments (for captures array)
                vm::call(vm_function_name.clone(), function_arity + 1),
                vm::vm_return(),
            ]
        })
        .collect::<Vec<_>>()
        .concat();

    [
        vec![vm::function(vm_function_name, 0)],
        closure_as_this,
        arguments_construction,
        // captures
        vec![vm::push(vm::Segment::This, 1)],
        dispatch_jumps,
        // "default" (no fid-s matched)
        vec![vm::push(vm::Segment::Constant, 0), vm::vm_return()],
        dispatch_calls,
    ]
    .concat()
}
