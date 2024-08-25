use crate::{
    codegen::{
        context::{module::ScopeSearchOutcome, ModuleContext, ProgramContext},
        error::Error,
        vm,
    },
    common::ast,
};

pub fn construct(
    variable: &ast::JillIdentifier,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> Result<vm::VMInstructionBlock, Error> {
    let variable_name = &variable.0;

    let ScopeSearchOutcome::Variable(variable_context) = module_context.scope.search(variable_name)
    else {
        return Err(Error::VariableNotInScope(variable_name.clone()));
    };

    let instructions = vec![vm::push(variable_context.segment, variable_context.index)].into();

    Ok(instructions)
}

#[cfg(test)]
mod tests {
    use crate::codegen::context::module::{FunctionContext, VariableContext};

    use super::*;

    #[test]
    fn test_successful_variable_name_construction() {
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new("Test".to_owned());

        assert!(module_context
            .scope
            .add_function(
                "foo".to_string(),
                FunctionContext {
                    number_of_arguments: 1,
                },
            )
            .is_ok());

        module_context.scope.add_frame();

        assert!(module_context
            .scope
            .add_variable(
                "a".to_string(),
                VariableContext {
                    segment: vm::Segment::Argument,
                    index: 0
                },
            )
            .is_ok());

        let expected_instructions = "push argument 0";

        assert!(construct(
            &ast::JillIdentifier("a".to_string()),
            &mut module_context,
            &mut program_context
        )
        .is_ok_and(|instructions| instructions.compile() == expected_instructions));
    }

    #[test]
    fn test_unsuccessful_variable_name_construction() {
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new("Test".to_owned());

        assert!(construct(
            &ast::JillIdentifier("a".to_string()),
            &mut module_context,
            &mut program_context
        )
        .is_err_and(|err| matches!(err, Error::VariableNotInScope(_))));
    }
}
