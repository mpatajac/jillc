use crate::fileio;

use super::vm;

/// List of all errors that could possibly occur during code generation.
#[derive(Debug)]
pub enum Error {
    VariableAlreadyInScope(String),
    FunctionAlreadyInScope(String),
    VariableNotInScope(String),
    CaptureNotInScope(String),
    InvalidFunctionReference(String),
    MultipleFunctionDefinitions(vm::VMFunctionName),
    InvalidFunctionCall(String),
    InvalidCompilerInternalFunctionCall(String),
    CaptureInTopLevelFunction(String),
    DiscardInGlobal,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO!: implement
        write!(f, "ERROR")
    }
}

impl std::error::Error for Error {}

pub type FallableAction = Result<(), Error>;
pub type FallableInstructions = Result<Vec<vm::VMInstruction>, Error>;
pub type FallableOutputFile = Result<fileio::output::OutputFile, Error>;
