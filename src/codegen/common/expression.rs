use crate::{
    codegen::{
        context::{ModuleContext, ProgramContext},
        error::Error,
        vm,
    },
    common::ast,
};

use super::{function_reference, literal, variable_name};

pub fn construct(
    expression: &ast::JillExpression,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> Result<Vec<vm::VMInstruction>, Error> {
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
