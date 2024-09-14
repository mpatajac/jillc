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
}

pub type FallableAction = Result<(), Error>;
pub type FallableInstructions = Result<Vec<vm::VMInstruction>, Error>;
