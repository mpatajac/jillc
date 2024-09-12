use crate::{
    codegen::{
        common::helpers::function::JillFunctionReferenceExtensions,
        context::{module::FunctionContext, ModuleContext, ProgramContext},
        error::Error,
        jillstd, vm,
    },
    common::ast,
};

pub fn construct(
    function_reference: &ast::JillFunctionReference,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> Result<Vec<vm::VMInstruction>, Error> {
    let function_context = module_context
        .scope
        .search_function(&function_reference.function_name.0);

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

    let function_prefix = function_context.map(|ctx| ctx.prefix).unwrap_or_default();
    let fully_qualified_function_reference_name = function_reference
        .to_fully_qualified_hack_name(&module_context.module_name, function_prefix);

    // we handle function references by pushing their
    // associated ID onto the stack as a constant
    let function_id = program_context
        .function_dispatch
        // have to use fully qualified names to avoid misrepresentation
        .encounter(fully_qualified_function_reference_name);
    let instructions = vec![vm::push(vm::Segment::Constant, function_id)];

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
) -> Result<Vec<vm::VMInstruction>, Error> {
    Err(Error::InvalidFunctionReference(
        function_reference.reconstruct_source_name(),
    ))
}

#[cfg(test)]
mod tests {
    use crate::codegen::context::module::FunctionContextArguments;

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

        let expected_instructions = "push constant 0";
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
}
