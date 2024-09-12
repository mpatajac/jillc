// region: VMModule

#[derive(Debug)]
pub struct VMModule {
    blocks: Vec<VMInstructionBlock>,
}

impl VMModule {
    pub const fn new() -> Self {
        Self { blocks: Vec::new() }
    }

    pub fn add_block(&mut self, block: VMInstructionBlock) {
        self.blocks.push(block);
    }

    pub fn compile(self) -> String {
        self.to_string()
    }
}

impl std::fmt::Display for VMModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.blocks
                .iter()
                .map(|block| format!("{block}"))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

// endregion

// region: VMInstructionBlock

#[derive(Debug)]
pub struct VMInstructionBlock {
    instructions: Vec<VMInstruction>,
}

impl std::fmt::Display for VMInstructionBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.instructions
                .iter()
                .map(|instruction| format!("{instruction}"))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

impl From<Vec<VMInstruction>> for VMInstructionBlock {
    fn from(instructions: Vec<VMInstruction>) -> Self {
        Self { instructions }
    }
}

impl From<&[VMInstruction]> for VMInstructionBlock {
    fn from(instructions: &[VMInstruction]) -> Self {
        Self {
            instructions: instructions.to_vec(),
        }
    }
}

impl VMInstructionBlock {
    pub fn compile(self) -> String {
        self.to_string()
    }
}

// endregion

// region: VMInstruction

/// Wrapper type which prevents accidentaly passing non-HACK-formatted
/// function name where one is required.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VMFunctionName(String);

impl VMFunctionName {
    /// Create function name from a string literal.
    ///
    /// Useful in cases when you know exactly which function
    /// you want to call (e.g. a built-in).
    pub fn from_literal(name: &str) -> Self {
        Self(name.to_owned())
    }

    /// Construct function name from (processed) components.
    ///
    /// Note: this function assumes that module paths are properly formatted
    /// and type name is empty if it was not originally present.
    pub fn construct(module_path: &str, type_name: &str, function_name: &str) -> Self {
        Self(format!("{module_path}.{type_name}{function_name}"))
    }
}

impl std::ops::Deref for VMFunctionName {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// region: VMInstruction utility functions

/// Utility function for the `push` VM instruction.
pub fn push(segment: Segment, i: usize) -> VMInstruction {
    VMInstruction::Push(segment, i)
}

/// Utility function for the `pop` VM instruction.
pub fn pop(segment: Segment, i: usize) -> VMInstruction {
    VMInstruction::Pop(segment, i)
}

/// Utility function for the `command` VM instruction.
pub fn command(command: VMCommand) -> VMInstruction {
    VMInstruction::Command(command)
}

/// Utility function for the `return` VM instruction.
pub fn vm_return() -> VMInstruction {
    VMInstruction::Command(VMCommand::Return)
}

/// Utility function for the `label` VM instruction.
pub fn label<S: Into<String>>(label_action: LabelAction, label: S) -> VMInstruction {
    VMInstruction::Label(label_action, label.into())
}

/// Utility function for the `function` VM instruction.
pub fn function(function_name: VMFunctionName, variable_count: usize) -> VMInstruction {
    VMInstruction::Function(function_name, variable_count)
}

/// Utility function for the `call` VM instruction.
pub fn call(function_name: VMFunctionName, argument_count: usize) -> VMInstruction {
    VMInstruction::Call(function_name, argument_count)
}

// endregion

type Index = usize;
type Label = String;
type Count = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VMInstruction {
    Push(Segment, Index),
    Pop(Segment, Index),
    Command(VMCommand),
    Label(LabelAction, Label),
    Function(VMFunctionName, Count),
    Call(VMFunctionName, Count),
}

impl VMInstruction {
    fn as_instruction(&self) -> String {
        match self {
            Self::Push(segment, i) => format!("push {segment} {i}"),
            Self::Pop(segment, i) => format!("pop {segment} {i}"),
            Self::Command(command) => command.to_string(),
            Self::Label(label_action, label) => format!("{label_action} {label}"),
            Self::Function(vm_function_name, variable_count) => {
                let function_name = &vm_function_name.0;
                format!("function {function_name} {variable_count}")
            }
            Self::Call(vm_function_name, argument_count) => {
                let function_name = &vm_function_name.0;
                format!("call {function_name} {argument_count}")
            }
        }
    }
}

impl std::fmt::Display for VMInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_instruction())
    }
}

#[derive(Debug, strum::Display, Clone, PartialEq, Eq)]
#[strum(serialize_all = "kebab-case")]
pub enum VMCommand {
    Add,
    Sub,
    Neg,
    Eq,
    Gt,
    Lt,
    And,
    Or,
    Not,
    Return,
}

#[derive(Debug, strum::Display, Clone, PartialEq, Eq)]
#[strum(serialize_all = "kebab-case")]
pub enum LabelAction {
    Label,
    Goto,
    IfGoto,
}

#[derive(Debug, strum::Display, Clone, Copy, PartialEq, Eq, Hash, strum::VariantArray)]
#[strum(serialize_all = "kebab-case")]
pub enum Segment {
    Local,
    Argument,
    Static,
    Constant,
    This,
    That,
    Pointer,
    Temp,
}

// endregion
