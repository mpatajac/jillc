//! Logic for converting parsed `Jill` code (_AST_)
//! to Hack VM instructions.

use context::{ModuleContext, ProgramContext};
use error::FallableOutputFile;

use crate::{common::ast, fileio::output::OutputFile};

pub mod context;
pub mod error;
pub mod post_compilation;

mod common;
mod functions;
mod globals;
mod types;
mod vm;

const GLOBALS_INIT_FN_NAME: &str = "_init__globals";

pub fn construct_module(
    module: ast::JillModule,
    program_context: &mut ProgramContext,
) -> FallableOutputFile {
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
