use crate::{
    codegen::{
        context::{ModuleContext, ProgramContext},
        error::FallableInstructions,
    },
    common::ast,
};

use super::{function_reference, literal, variable_name};

pub fn construct(
    expression: &ast::JillExpression,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableInstructions {
    match expression {
        ast::JillExpression::Literal(literal) => {
            literal::construct(literal, module_context, program_context)
        }
        ast::JillExpression::VariableName(variable_name) => {
            variable_name::construct(variable_name, module_context, program_context)
        }
        ast::JillExpression::FunctionReference(function_reference) => {
            function_reference::construct(function_reference, module_context, program_context)
        }
        ast::JillExpression::FunctionCall(_) => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::vm;

    use super::*;

    #[test]
    fn test_expression_construction() {
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new("Test".to_owned());

        let expression = ast::JillExpression::Literal(ast::JillLiteral::Integer(5));

        let expected = "push constant 5";

        assert!(
            construct(&expression, &mut module_context, &mut program_context).is_ok_and(
                |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
            )
        );
    }
}
