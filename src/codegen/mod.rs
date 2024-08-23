//! Logic for converting parsed `Jill` code (_AST_)
//! to Hack VM instructions.

use context::{ModuleContext, ProgramContext};

use crate::common::ast;

pub mod context;

pub fn construct_module(module: ast::JillModule, program_context: &mut ProgramContext) -> String {
    let mut module_context = ModuleContext::new(module.name);

    todo!()
}
