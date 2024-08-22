//! Types representing the structure of a Jill program.
//!
//! Note: all types are prefixed with `Jill` to avoid potential
//! name collision with existing keywords/phrases (e.g. Type).

#[derive(Debug)]
pub struct JillProgram {
    pub modules: Vec<JillModule>,
}

// region: non-terminals

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
    pub functions: Vec<JillFunction>,
    // `type`
    // pub types: Vec<JillType>,
}

#[derive(Debug)]
pub struct JillVariable {
    pub name: JillIdentifier,
    pub value: JillExpression,
}

#[derive(Debug)]
pub struct JillFunction {
    pub name: JillIdentifier,
    pub arguments: Vec<JillIdentifier>,
    pub body: JillFunctionBody,
}

#[derive(Debug)]
pub enum JillExpression {
    Literal(JillLiteral),
    // TODO?: differentiate function call types (full, type, local, variable)
    FunctionCall(JillFunctionCall),
    FunctionReference(JillFunctionReference),
    VariableName(JillIdentifier),
}

#[derive(Debug)]
pub struct JillFunctionBody {
    pub local_functions: Vec<JillFunction>,
    pub local_variables: Vec<JillVariable>,
    pub return_expression: JillExpression,
}

#[derive(Debug)]
pub struct JillFunctionReference {
    pub modules_path: Vec<JillIdentifier>,
    pub associated_type: Option<JillIdentifier>,
    pub function_name: JillIdentifier,
}

#[derive(Debug)]
pub struct JillFunctionCall {
    pub reference: JillFunctionReference,
    pub arguments: Vec<JillExpression>,
}

// endregion

// region: terminals

#[derive(Debug)]
pub enum JillLiteral {
    Integer(isize),
    String(String),
}

#[derive(Debug, Clone)]
pub struct JillIdentifier(pub String);

// endregion
