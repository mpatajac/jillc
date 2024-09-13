use crate::{
    codegen::{
        context::{ModuleContext, ProgramContext},
        error::Error,
        vm,
    },
    common::ast,
};

pub fn construct(
    globals: Vec<ast::JillVariable>,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> Result<Vec<vm::VMInstruction>, Error> {
    globals
        .iter()
        .map(|variable| {
            super::common::variable::construct(
                variable,
                vm::Segment::Static,
                module_context,
                program_context,
            )
        })
        .collect::<Result<Vec<_>, _>>()
        .map(|blocks| blocks.concat())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_globals_construction() {
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new("Test".to_owned());

        let variables = vec![
            ast::JillVariable {
                name: ast::JillIdentifier(String::from("foo")),
                value: ast::JillExpression::Literal(ast::JillLiteral::Integer(5)),
            },
            ast::JillVariable {
                name: ast::JillIdentifier(String::from("bar")),
                value: ast::JillExpression::Literal(ast::JillLiteral::Bool(true)),
            },
        ];

        let expected = [
            // `foo`
            "push constant 5",
            "pop static 0",
            // `bar`
            "push constant 1",
            "neg",
            "pop static 1",
        ]
        .join("\n");

        assert!(module_context
            .scope
            .search_variable(&"foo".to_string())
            .is_none());

        assert!(
            construct(variables, &mut module_context, &mut program_context).is_ok_and(
                |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
            )
        );

        assert!(module_context
            .scope
            .search_variable(&"foo".to_string())
            .is_some());
    }
}
