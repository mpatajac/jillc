//! Types representing the structure of a Jill program.
//!
//! Note: all types are prefixed with `Jill` to avoid potential
//! name collision with existing keywords/phrases (e.g. Type).

#[derive(Debug)]
pub struct JillProgram {
    pub modules: Vec<JillModule>,
}

#[derive(Debug)]
pub struct JillModule {
    pub name: String,
    pub content: JillModuleContent,
}

#[derive(Debug)]
pub struct JillModuleContent {
    // `let`
    pub variables: Vec<JillVariable>,
    // `fn`
    // pub functions: Vec<JillFunction>,

    // `type`
    // pub types: Vec<JillType>,
}

#[derive(Debug)]
pub struct JillVariable {
    pub name: JillIdentifier,
    pub value: JillExpression,
}

#[derive(Debug)]
pub enum JillExpression {
    Literal(JillLiteral),
    // FunctionCall(??),
    // VariableName(??)
}

#[derive(Debug)]
pub enum JillLiteral {
    Integer(isize),
    String(String),
}

#[derive(Debug)]
pub struct JillIdentifier {
    pub value: String,
}
