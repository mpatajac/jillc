//! Logic for converting parsed `Jill` code (_AST_)
//! to Hack VM instructions.

use context::{ModuleContext, ProgramContext};

use crate::{common::ast, fileio::output::OutputFile};

mod common;
pub mod context;
pub mod error;
mod functions;
mod globals;
mod jillstd;
mod types;
mod vm;

const GLOBALS_INIT_FN_NAME: &str = "_init__globals";

pub fn construct_module(
    module: ast::JillModule,
    program_context: &mut ProgramContext,
) -> Result<OutputFile, error::Error> {
    let mut module_context = ModuleContext::new(module.name);

    types::construct(module.content.types, &mut module_context, program_context)?;

    globals::construct(
        module.content.variables,
        &mut module_context,
        program_context,
    )?;

    functions::construct(
        module.content.functions,
        &mut module_context,
        program_context,
    )?;

    Ok(OutputFile::new(
        module_context.module_name,
        module_context.output.compile(),
    ))
}
