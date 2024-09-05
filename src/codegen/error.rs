/// List of all errors that could possibly occur during code generation.
#[derive(Debug)]
pub enum Error {
    VariableAlreadyInScope(String),
    FunctionAlreadyInScope(String),
    VariableNotInScope(String),
    FunctionNotInScope(String),
}

pub type FallableAction = Result<(), Error>;
