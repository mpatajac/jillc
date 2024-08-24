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

// endregion

// region: VMInstruction

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
pub fn function<S: Into<String>>(function_name: S, variable_count: usize) -> VMInstruction {
    VMInstruction::Function(function_name.into(), variable_count)
}

/// Utility function for the `call` VM instruction.
pub fn call<S: Into<String>>(function_name: S, argument_count: usize) -> VMInstruction {
    VMInstruction::Call(function_name.into(), argument_count)
}

// endregion

type Index = usize;
type Label = String;
type Count = usize;
type FunctionName = String;

#[derive(Debug, Clone)]
pub enum VMInstruction {
    Push(Segment, Index),
    Pop(Segment, Index),
    Command(VMCommand),
    Label(LabelAction, Label),
    Function(FunctionName, Count),
    Call(FunctionName, Count),
}

impl VMInstruction {
    fn as_instruction(&self) -> String {
        match self {
            Self::Push(segment, i) => format!("push {segment} {i}"),
            Self::Pop(segment, i) => format!("pop {segment} {i}"),
            Self::Command(command) => command.to_string(),
            Self::Label(label_action, label) => format!("{label_action} {label}"),
            Self::Function(function_name, variable_count) => {
                format!("function {function_name} {variable_count}")
            }
            Self::Call(function_name, argument_count) => {
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

#[derive(Debug, strum::Display, Clone)]
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

#[derive(Debug, strum::Display, Clone)]
#[strum(serialize_all = "kebab-case")]
pub enum LabelAction {
    Label,
    Goto,
    IfGoto,
}

#[derive(Debug, strum::Display, Clone)]
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
