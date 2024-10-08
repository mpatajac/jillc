use crate::{
    codegen::{
        common::helpers::{self, function::JillFunctionReferenceExtensions},
        context::{module::FunctionContext, ModuleContext, ProgramContext},
        error::{Error, FallableInstructions},
        post_compilation::jillstd,
        vm,
    },
    common::ast,
};

pub fn construct(
    function_reference: &ast::JillFunctionReference,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableInstructions {
    let function_context = module_context
        .scope
        .search_function(&function_reference.type_associated_function_name());

    if !is_valid_function_reference(function_reference, &function_context) {
        return invalid_function_reference(function_reference);
    }

    // track (potential) `JillStd` function occurences
    let std_function_usage_note_outcome = program_context
        .std_usage_tracker
        .note_usage(function_reference);
    if std_function_usage_note_outcome
        == jillstd::JillStdFunctionUsageNoteOutcome::FunctionNotPresentInModule
    {
        return invalid_function_reference(function_reference);
    }

    let function_prefix = function_context
        .as_ref()
        .map(|ctx| ctx.prefix.clone())
        .unwrap_or_default();
    let vm_function_name = function_reference
        .to_fully_qualified_hack_name(&module_context.module_name, function_prefix);

    // we handle function references by pushing their
    // associated ID onto the stack as a constant
    let function_id = program_context
        .function_dispatch
        // have to use fully qualified (HACK) names to avoid misrepresentation
        .encounter(vm_function_name);

    // collect function captures (if any are present)
    let function_captures = function_context.map_or(Vec::new(), |ctx| ctx.captures);
    let captures_instructions = helpers::capture::construct_captures_array(
        &function_captures,
        module_context,
        program_context,
    )?;

    let instructions = [
        vec![vm::push(vm::Segment::Constant, function_id)],
        captures_instructions,
        vec![vm::call(vm::VMFunctionName::from_literal("Fn._new"), 2)],
    ]
    .concat();

    Ok(instructions)
}

/// Function reference is valid if it is a reference to another module
/// or an existing (found module-local) function
fn is_valid_function_reference(
    function_reference: &ast::JillFunctionReference,
    function_context: &Option<FunctionContext>,
) -> bool {
    function_reference.is_fully_qualified() || function_context.is_some()
}

fn invalid_function_reference(
    function_reference: &ast::JillFunctionReference,
) -> FallableInstructions {
    Err(Error::InvalidFunctionReference(
        function_reference.reconstruct_source_name(),
    ))
}

#[cfg(test)]
mod tests {
    use crate::codegen::context::module::{FunctionContextArguments, VariableContextArguments};

    use super::*;

    #[test]
    fn test_successful_function_reference_construction() {
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new("Test".to_owned());

        assert!(module_context
            .scope
            .enter_function("foo".to_string(), FunctionContextArguments::new(0))
            .is_ok());

        module_context.scope.leave_function();

        assert!(module_context
            .scope
            .enter_function("bar".to_string(), FunctionContextArguments::new(0))
            .is_ok());

        let function_reference = ast::JillFunctionReference {
            modules_path: vec![],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("foo")),
        };

        let expected_instructions = [
            "push constant 0",
            // null
            "push constant 0",
            "call Fn._new 2",
        ]
        .join("\n");

        assert!(construct(
            &function_reference,
            &mut module_context,
            &mut program_context
        )
        .is_ok_and(
            |instructions| vm::VMInstructionBlock::from(instructions).compile()
                == expected_instructions
        ));
    }

    #[test]
    fn test_unsuccessful_function_reference_construction() {
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new("Test".to_owned());

        assert!(module_context
            .scope
            .enter_function("foo".to_string(), FunctionContextArguments::new(0))
            .is_ok());

        module_context.scope.leave_function();

        let function_reference = ast::JillFunctionReference {
            modules_path: vec![],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("bar")),
        };

        assert!(construct(
            &function_reference,
            &mut module_context,
            &mut program_context
        )
        .is_err_and(|error| matches!(error, Error::InvalidFunctionReference(_))));
    }

    #[test]
    fn test_construction_of_function_reference_with_captures() {
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new("Test".to_owned());

        // `fn foo a b c` (top level fn)
        assert!(module_context
            .scope
            .enter_function("foo".to_string(), FunctionContextArguments::new(3))
            .is_ok());

        // argument `a`
        assert!(module_context
            .scope
            .add_variable(
                "a".to_string(),
                VariableContextArguments::new(vm::Segment::Argument)
            )
            .is_ok());

        // argument `b`
        assert!(module_context
            .scope
            .add_variable(
                "b".to_string(),
                VariableContextArguments::new(vm::Segment::Argument)
            )
            .is_ok());

        // argument `c`
        assert!(module_context
            .scope
            .add_variable(
                "c".to_string(),
                VariableContextArguments::new(vm::Segment::Argument)
            )
            .is_ok());

        // `fn bar x [a c] = _.` (nested fn)
        assert!(module_context
            .scope
            .enter_function(
                "bar".to_string(),
                FunctionContextArguments::new(1)
                    .with_captures(vec!["a".to_string(), "c".to_string()])
            )
            .is_ok());

        // argument `x`
        assert!(module_context
            .scope
            .add_variable(
                "x".to_string(),
                VariableContextArguments::new(vm::Segment::Argument)
            )
            .is_ok());

        module_context.scope.leave_function();

        // reference to the nested function (i.e. returning a function from a function)
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("bar")),
        };

        let expected_instructions = [
            // fid
            "push constant 0",
            // captures
            // create array
            "push constant 2",
            "call Array.new 1",
            "pop temp 1",
            // add `a`
            "push constant 0",
            "push temp 1",
            "add",
            "push argument 0",
            "pop temp 0",
            "pop pointer 1",
            "push temp 0",
            "pop that 0",
            // add `c`
            "push constant 1",
            "push temp 1",
            "add",
            "push argument 2",
            "pop temp 0",
            "pop pointer 1",
            "push temp 0",
            "pop that 0",
            // push to stack
            "push temp 1",
            // build closure
            "call Fn._new 2",
        ]
        .join("\n");

        assert!(construct(
            &function_reference,
            &mut module_context,
            &mut program_context
        )
        .is_ok_and(
            |instructions| vm::VMInstructionBlock::from(instructions).compile()
                == expected_instructions
        ));
    }
}
