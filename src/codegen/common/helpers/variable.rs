use crate::common::ast;

pub trait JillVariableExtensions {
    fn is_discard(&self) -> bool;
}

impl JillVariableExtensions for ast::JillVariable {
    fn is_discard(&self) -> bool {
        self.name.0.is_empty()
    }
}
