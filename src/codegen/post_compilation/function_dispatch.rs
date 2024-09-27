use crate::{
    codegen::{
        common::helpers,
        context::{module::VariableContext, program::FunctionReferenceIndex, ProgramContext},
        error::{Error, FallableInstructions},
        vm,
    },
    fileio::output::OutputFile,
};

pub fn construct(program_context: &mut ProgramContext) -> Result<Option<OutputFile>, Error> {
    let functions_to_dispatch = program_context.function_dispatch.collect();

    if functions_to_dispatch.is_empty() {
        // no functions to dispatch - no need to generate anything
        return Ok(None);
    }

    let instruction_block: vm::VMInstructionBlock = [
        construct_new(),
        construct_call(functions_to_dispatch, program_context)?,
    ]
    .concat()
    .into();

    Ok(Some(OutputFile::new(
        String::from("Fn"),
        instruction_block.compile(),
    )))
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
) -> FallableInstructions {
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
            let Some(function_arity) = get_function_arity(vm_function_name, program_context) else {
                return Err(Error::InvalidFunctionReference(
                    vm_function_name.to_string(),
                ));
            };

            Ok(vec![
                // jump label
                vm::label(vm::LabelAction::Label, format!("FN_{i}")),
                // directly call associated function
                // NOTE: call with `arity` + 1 arguments (for captures array)
                vm::call(vm_function_name.clone(), function_arity + 1),
                vm::vm_return(),
            ])
        })
        .collect::<Result<Vec<_>, _>>()?
        .concat();

    Ok([
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
    .concat())
}

fn get_function_arity(
    vm_function_name: &vm::VMFunctionName,
    program_context: &mut ProgramContext,
) -> Option<usize> {
    program_context
        .program_metadata
        .get_function_arity(vm_function_name)
        .or_else(|| helpers::jack_api::function_arity(vm_function_name))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[allow(clippy::similar_names, clippy::redundant_clone)]
    #[test]
    fn test_dispatch_call_construction() {
        let mut program_context = ProgramContext::new();

        // region: setup

        let foo = vm::VMFunctionName::from_literal("Test.foo");
        let bar = vm::VMFunctionName::from_literal("Test.bar");
        let baz = vm::VMFunctionName::from_literal("Test.baz");
        let biz = vm::VMFunctionName::from_literal("Test.biz");

        // arities
        assert!(program_context
            .program_metadata
            .log_function_arity(foo.clone(), 2)
            .is_ok());

        assert!(program_context
            .program_metadata
            .log_function_arity(bar.clone(), 1)
            .is_ok());

        assert!(program_context
            .program_metadata
            .log_function_arity(baz.clone(), 5)
            .is_ok());

        assert!(program_context
            .program_metadata
            .log_function_arity(biz.clone(), 2)
            .is_ok());

        // dispatch
        program_context.function_dispatch.encounter(foo.clone());
        program_context.function_dispatch.encounter(bar.clone());
        program_context.function_dispatch.encounter(foo.clone());
        program_context.function_dispatch.encounter(baz.clone());
        program_context.function_dispatch.encounter(biz.clone());
        program_context.function_dispatch.encounter(bar.clone());
        program_context.function_dispatch.encounter(foo.clone());
        program_context.function_dispatch.encounter(baz.clone());
        program_context.function_dispatch.encounter(foo.clone());
        program_context.function_dispatch.encounter(baz.clone());

        // endregion

        let expected = [
            "function Fn._call 0",
            // resolve `this`
            "push argument 0",
            "pop pointer 0",
            // SECTION: push args on stack
            "push constant 0",
            "pop temp 7",
            "label ARGS_INIT_START",
            // condition check
            "push temp 7",
            "push argument 1",
            "eq",
            "if-goto ARGS_INIT_END",
            // loop body
            "push temp 7",
            "push argument 2",
            "add",
            "pop pointer 1",
            "push that 0",
            // i = i + 1
            "push temp 7",
            "push constant 1",
            "add",
            "pop temp 7",
            "goto ARGS_INIT_START",
            "label ARGS_INIT_END",
            // end SECTION: push args on stack
            // captures
            "push this 1",
            // SECTION: dispatch jumps
            // foo
            "push this 0",
            "push constant 0",
            "eq",
            "if-goto FN_0",
            // baz
            "push this 0",
            "push constant 2",
            "eq",
            "if-goto FN_1",
            // bar
            "push this 0",
            "push constant 1",
            "eq",
            "if-goto FN_2",
            // biz
            "push this 0",
            "push constant 3",
            "eq",
            "if-goto FN_3",
            // "default"
            "push constant 0",
            "return",
            // end SECTION: dispatch jumps
            // SECTION: dispatch calls
            // foo
            "label FN_0",
            "call Test.foo 3",
            "return",
            // baz
            "label FN_1",
            "call Test.baz 6",
            "return",
            // bar
            "label FN_2",
            "call Test.bar 2",
            "return",
            // biz
            "label FN_3",
            "call Test.biz 3",
            "return",
            // end SECTION: dispatch calls
        ]
        .join("\n");

        assert!(construct_call(
            program_context.function_dispatch.collect(),
            &mut program_context,
        )
        .is_ok_and(
            |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
        ));
    }
}
