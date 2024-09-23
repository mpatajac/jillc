//! Types representing the structure of a Jill program.
//!
//! Note: all types are prefixed with `Jill` to avoid potential
//! name collision with existing keywords/phrases (e.g. Type).

/// Associate each AST element with its corresponding
/// source code span (for better error reporting).
pub type Span = std::ops::Range<usize>;

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
    pub _span: Span,
}

impl JillVariable {
    /// Construct an internally used Jill variable
    /// (as opposed to a parsed one).
    ///
    /// Most commonly used in tests.
    pub fn internal(name: JillIdentifier, value: JillExpression) -> Self {
        Self {
            name,
            value,
            // did not come from a source file, so does not have a related span
            _span: Span::default(),
        }
    }
}

#[derive(Debug)]
pub struct JillFunction {
    pub name: JillIdentifier,
    pub arguments: Vec<JillIdentifier>,
    pub captures: Vec<JillIdentifier>,
    pub body: JillFunctionBody,
    pub _span: Span,
}

impl JillFunction {
    /// Construct an internally used Jill function
    /// (as opposed to a parsed one).
    ///
    /// Most commonly used in tests.
    pub fn internal(
        name: JillIdentifier,
        arguments: Vec<JillIdentifier>,
        captures: Vec<JillIdentifier>,
        body: JillFunctionBody,
    ) -> Self {
        Self {
            name,
            arguments,
            captures,
            body,
            // did not come from a source file, so does not have a related span
            _span: Span::default(),
        }
    }
}

#[derive(Debug)]
pub struct JillType {
    pub name: JillIdentifier,
    pub variants: Vec<JillTypeVariant>,
    pub _span: Span,
}

impl JillType {
    /// Construct an internally used Jill type
    /// (as opposed to a parsed one).
    ///
    /// Most commonly used in tests.
    pub fn internal(name: JillIdentifier, variants: Vec<JillTypeVariant>) -> Self {
        Self {
            name,
            variants,
            // did not come from a source file, so does not have a related span
            _span: Span::default(),
        }
    }
}

#[derive(Debug)]
pub struct JillTypeVariant {
    pub name: JillIdentifier,
    pub fields: Vec<JillIdentifier>,
    pub _span: Span,
}

impl JillTypeVariant {
    /// Construct an internally used Jill type variant
    /// (as opposed to a parsed one).
    ///
    /// Most commonly used in tests.
    pub fn internal(name: JillIdentifier, fields: Vec<JillIdentifier>) -> Self {
        Self {
            name,
            fields,
            // did not come from a source file, so does not have a related span
            _span: Span::default(),
        }
    }
}

#[derive(Debug)]
pub enum JillExpression {
    Literal(JillLiteral),
    FunctionCall(JillFunctionCall),
    FunctionReference(JillFunctionReference),
    VariableName(JillIdentifier),
}

#[derive(Debug)]
pub struct JillFunctionBody {
    pub local_functions: Vec<JillFunction>,
    pub local_variables: Vec<JillVariable>,
    pub return_expression: JillExpression,
    pub _span: Span,
}

impl JillFunctionBody {
    /// Construct an internally used Jill function body
    /// (as opposed to a parsed one).
    ///
    /// Most commonly used in tests.
    pub fn internal(
        local_functions: Vec<JillFunction>,
        local_variables: Vec<JillVariable>,
        return_expression: JillExpression,
    ) -> Self {
        Self {
            local_functions,
            local_variables,
            return_expression,
            // did not come from a source file, so does not have a related span
            _span: Span::default(),
        }
    }
}

#[derive(Debug)]
pub struct JillFunctionReference {
    pub modules_path: Vec<JillIdentifier>,
    pub associated_type: Option<JillIdentifier>,
    pub function_name: JillIdentifier,
    pub _span: Span,
}

impl JillFunctionReference {
    /// Construct an internally used Jill function reference
    /// (as opposed to a parsed one).
    ///
    /// Most commonly used in tests.
    pub fn internal(
        modules_path: Vec<JillIdentifier>,
        associated_type: Option<JillIdentifier>,
        function_name: JillIdentifier,
    ) -> Self {
        Self {
            modules_path,
            associated_type,
            function_name,
            // did not come from a source file, so does not have a related span
            _span: Span::default(),
        }
    }
}

#[derive(Debug)]
pub struct JillFunctionCall {
    pub reference: JillFunctionReference,
    pub arguments: Vec<JillExpression>,
    pub _span: Span,
}

impl JillFunctionCall {
    /// Construct an internally used Jill function call
    /// (as opposed to a parsed one).
    ///
    /// Most commonly used in tests.
    pub fn internal(reference: JillFunctionReference, arguments: Vec<JillExpression>) -> Self {
        Self {
            reference,
            arguments,
            // did not come from a source file, so does not have a related span
            _span: Span::default(),
        }
    }
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
pub struct JillIdentifier(pub String, pub Span);

impl JillIdentifier {
    /// Construct an internally used Jill identifier
    /// (as opposed to a parsed one).
    ///
    /// Most commonly used in tests.
    pub fn internal(name: String) -> Self {
        // did not come from a source file, so does not have a related span
        Self(name, Span::default())
    }
}

impl std::fmt::Display for JillIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

// endregion
