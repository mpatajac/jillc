use crate::{
    codegen::{
        context::{ModuleContext, ProgramContext},
        vm,
    },
    common::ast,
};

pub fn construct(
    expression: &ast::JillExpression,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> Vec<vm::VMInstruction> {
    todo!()
}
