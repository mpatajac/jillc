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
    // `type`
    pub types: Vec<JillType>,

    // `let`
    pub variables: Vec<JillVariable>,

    // `fn`
    pub functions: Vec<JillFunction>,
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
pub struct JillType {
    pub name: JillIdentifier,
    pub variants: Vec<JillTypeVariant>,
}

#[derive(Debug)]
pub struct JillTypeVariant {
    pub name: JillIdentifier,
    pub fields: Vec<JillIdentifier>,
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
    Bool(bool),
    List(Vec<JillExpression>),
}

#[derive(Debug, Clone)]
pub struct JillIdentifier(pub String);

impl std::fmt::Display for JillIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

// endregion
