use crate::{
    codegen::{
        context::{module::VariableContextArguments, ModuleContext, ProgramContext},
        error::Error,
        vm,
    },
    common::ast,
};

use super::expression;

pub fn construct(
    variable: &ast::JillVariable,
    // pass segment as argument so we can share
    // this logic between global and local variables
    segment: vm::Segment,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> Result<Vec<vm::VMInstruction>, Error> {
    // (try to) evaluate assigned expression
    let expression_instructions =
        expression::construct(&variable.value, module_context, program_context)?;

    // (try to) register variable
    let variable_context = module_context.scope.add_variable(
        variable.name.0.clone(),
        VariableContextArguments::new(segment),
    )?;

    let instruction_components = [expression_instructions, vec![variable_context.push()]];

    Ok(instruction_components.concat())
}

#[cfg(test)]
mod tests {
    use crate::codegen::context::module::FunctionContextArguments;

    use super::*;

    #[test]
    fn test_assignment_construction() {
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new("Test".to_owned());

        assert!(module_context
            .scope
            .enter_function(String::from("f"), FunctionContextArguments::new(0))
            .is_ok());

        let name = ast::JillIdentifier(String::from("foo"));
        let expression = ast::JillExpression::Literal(ast::JillLiteral::Integer(5));
        let variable = ast::JillVariable {
            name: name.clone(),
            value: expression,
        };

        let segment = vm::Segment::Local;

        let expected = ["push constant 5", "push local 0"].join("\n");

        assert!(construct(
            &variable,
            segment,
            &mut module_context,
            &mut program_context
        )
        .is_ok_and(
            |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
        ));

        assert!(module_context.scope.search_variable(&name.0).is_some());
    }
}
