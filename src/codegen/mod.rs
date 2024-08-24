//! Logic for converting parsed `Jill` code (_AST_)
//! to Hack VM instructions.

use context::{ModuleContext, ProgramContext};

use crate::common::ast;

mod common;
pub mod context;
mod types;
mod vm;

pub fn construct_module(module: ast::JillModule, program_context: &mut ProgramContext) -> String {
    let mut module_context = ModuleContext::new(module.name);

    types::construct(module.content.types, &mut module_context, program_context);

    // TODO: lets (globals)
    // TODO: fns

    module_context.output.compile()
}
