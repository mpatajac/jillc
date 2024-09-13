use crate::{
    codegen::{
        context::{ModuleContext, ProgramContext},
        error::Error,
        vm,
    },
    common::ast,
};

use super::{error::FallableAction, GLOBALS_INIT_FN_NAME};

pub fn construct(
    globals: Vec<ast::JillVariable>,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableAction {
    let globals_assignment_instructions =
        construct_globals(&globals, module_context, program_context)?;

    if !globals_assignment_instructions.is_empty() {
        // log this module as one of those which will be called in `Globals.init`
        program_context
            .globals
            .insert(module_context.module_name.clone());

        let function_name =
            vm::VMFunctionName::construct(&module_context.module_name, "", GLOBALS_INIT_FN_NAME);

        let instructions = [
            vec![vm::function(function_name, 0)],
            globals_assignment_instructions,
            vec![vm::push(vm::Segment::Constant, 0), vm::vm_return()],
        ];

        module_context
            .output
            .add_block(instructions.concat().into());
    }

    Ok(())
}

fn construct_globals(
    globals: &Vec<ast::JillVariable>,
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
        let module_name = "Test";
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new(module_name.to_owned());

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
            "function Test._init__globals 0",
            // `foo`
            "push constant 5",
            "pop static 0",
            // `bar`
            "push constant 1",
            "neg",
            "pop static 1",
            // return
            "push constant 0",
            "return",
        ]
        .join("\n");

        // `foo` not previously present in scope
        assert!(module_context
            .scope
            .search_variable(&"foo".to_string())
            .is_none());

        // construction successful and correct
        assert!(construct(variables, &mut module_context, &mut program_context).is_ok());
        assert_eq!(module_context.output.compile(), expected);

        // `foo` now added to scope
        assert!(module_context
            .scope
            .search_variable(&"foo".to_string())
            .is_some());

        // module added to "globals to generate" list
        assert!(program_context.globals.contains(module_name));
    }

    #[test]
    fn test_no_globals() {
        let module_name = "Test";
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new(module_name.to_owned());

        // no globals in module
        let variables = vec![];

        // result should be empty
        assert!(construct(variables, &mut module_context, &mut program_context).is_ok());
        assert_eq!(module_context.output.compile(), "");

        // module should not be added to "globals to generate" list
        assert!(!program_context.globals.contains(module_name));
    }
}
