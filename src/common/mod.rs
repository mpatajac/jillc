//! Common data & logic shared across the compiler
//! (e.g. AST).
pub mod ast;

#[derive(Debug, PartialEq, Eq, Clone, Copy, strum::VariantNames, strum::EnumString)]
#[strum(serialize_all = "camelCase")]
pub enum CompilerInternalFunction {
    If,
    IfElse,
    Do,
    Match,
    Todo,
    Free,
}
