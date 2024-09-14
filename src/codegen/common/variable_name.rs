use crate::{
    codegen::{
        context::{ModuleContext, ProgramContext},
        error::{Error, FallableInstructions},
    },
    common::ast,
};

pub fn construct(
    variable: &ast::JillIdentifier,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableInstructions {
    let variable_name = &variable.0;

    let Some(variable_context) = module_context.scope.search_variable(variable_name) else {
        return Err(Error::VariableNotInScope(variable_name.clone()));
    };

    let instructions = vec![variable_context.push()];

    Ok(instructions)
}

#[cfg(test)]
mod tests {
    use crate::codegen::{
        context::module::{FunctionContextArguments, VariableContextArguments},
        vm,
    };

    use super::*;

    #[test]
    fn test_successful_variable_name_construction() {
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new("Test".to_owned());

        assert!(module_context
            .scope
            .enter_function("foo".to_string(), FunctionContextArguments::new(1))
            .is_ok());

        assert!(module_context
            .scope
            .add_variable(
                "a".to_string(),
                VariableContextArguments::new(vm::Segment::Argument),
            )
            .is_ok());

        let expected_instructions = "push argument 0";

        assert!(construct(
            &ast::JillIdentifier("a".to_string()),
            &mut module_context,
            &mut program_context
        )
        .is_ok_and(
            |instructions| vm::VMInstructionBlock::from(instructions).compile()
                == expected_instructions
        ));
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
