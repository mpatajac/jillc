use crate::{
    codegen::{
        common::helpers::function::JillFunctionReferenceExtensions,
        context::{
            module::{FunctionContextArguments, VariableContextArguments},
            ModuleContext, ProgramContext,
        },
        error::FallableInstructions,
        vm,
    },
    common::ast,
};

pub fn construct(
    function: &ast::JillFunction,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableInstructions {
    let arity = function.arguments.len();

    let capture_names = function
        .captures
        .iter()
        .map(|capture| capture.0.clone())
        .collect();

    // register function in scope
    let function_context = module_context.scope.enter_function(
        // TODO: figure out naming
        function.name.0.clone(),
        FunctionContextArguments::new(arity).with_captures(capture_names),
    )?;

    // register arguments in scope
    for argument in &function.arguments {
        module_context.scope.add_variable(
            argument.0.clone(),
            VariableContextArguments::new(vm::Segment::Argument),
        )?;
    }

    let function_reference =
        ast::JillFunctionReference::from_function_definition(&module_context.module_name, function);

    let vm_function_name = function_reference
        .to_fully_qualified_hack_name(&module_context.module_name, function_context.prefix);

    // add arity (for dispatch)
    program_context
        .program_metadata
        .log_function_arity(vm_function_name.clone(), arity)?;

    // construct nested functions
    let nested_functions_instructions = function
        .body
        .local_functions
        .iter()
        .map(|f| construct(f, module_context, program_context))
        .collect::<Result<Vec<_>, _>>()?
        .concat();

    let number_of_local_variables = function.body.local_variables.len();

    let function_body_instructions =
        construct_body(&function.body, module_context, program_context)?;

    module_context.scope.leave_function();

    Ok([
        nested_functions_instructions,
        vec![vm::function(vm_function_name, number_of_local_variables)],
        function_body_instructions,
    ]
    .concat())
}

fn construct_body(
    function_body: &ast::JillFunctionBody,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableInstructions {
    // TODO: check is tail-recursive
    todo!()
}

mod tail_recursion {
    use crate::{
        codegen::{
            common::function_call::{self, FunctionCallKind},
            context::ModuleContext,
        },
        common::{ast, CompilerInternalFunction},
    };

    // give type alias to make OR's in match arms nicer
    type Cif = CompilerInternalFunction;

    pub(super) fn is_tail_recursive(
        original_function_name: &str,
        return_expression: &ast::JillExpression,
        module_context: &mut ModuleContext,
    ) -> bool {
        // has to be a function call
        let ast::JillExpression::FunctionCall(function_call) = return_expression else {
            return false;
        };

        // direct recursive call
        if function_call.reference.function_name.0 == original_function_name {
            return true;
        }

        // if not a direct recursive call, has to be a recursive call
        // nested in a compiler internal function call
        let FunctionCallKind::CompilerInternal(compiler_internal_function) =
            function_call::determine_function_call_kind(&function_call.reference, module_context)
        else {
            return false;
        };

        match compiler_internal_function {
            // check last argument for a nested recursive call
            Cif::If | Cif::IfElse | Cif::Do => {
                // all of these functions expect (more than) one argument
                let Some(last_argument) = function_call.arguments.last() else {
                    return false;
                };

                is_tail_recursive(original_function_name, last_argument, module_context)
            }

            // check that any variant expression is a nested recursive call
            Cif::Match => function_call
                .arguments
                .iter()
                .skip(1)
                .any(|expr| is_tail_recursive(original_function_name, expr, module_context)),

            // cannot have a nested recursive call
            Cif::Todo => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tail_recursion_detection() {
        let mut module_context = ModuleContext::new(String::from("Test"));

        let original_function_name = "foo";

        let tail_recursion_tests = [
            (
                // non-function call
                ast::JillExpression::Literal(ast::JillLiteral::Integer(5)),
                false,
            ),
            (
                // direct recursive call
                ast::JillExpression::FunctionCall(ast::JillFunctionCall {
                    reference: ast::JillFunctionReference {
                        modules_path: vec![],
                        associated_type: None,
                        function_name: ast::JillIdentifier(String::from("foo")),
                    },
                    arguments: vec![],
                }),
                true,
            ),
            (
                // call to other function
                ast::JillExpression::FunctionCall(ast::JillFunctionCall {
                    reference: ast::JillFunctionReference {
                        modules_path: vec![],
                        associated_type: None,
                        function_name: ast::JillIdentifier(String::from("bar")),
                    },
                    arguments: vec![],
                }),
                false,
            ),
            (
                // recursive call nested in compiler internal (last argument)
                ast::JillExpression::FunctionCall(ast::JillFunctionCall {
                    reference: ast::JillFunctionReference {
                        modules_path: vec![],
                        associated_type: None,
                        function_name: ast::JillIdentifier(String::from("if")),
                    },
                    arguments: vec![
                        ast::JillExpression::Literal(ast::JillLiteral::Bool(true)),
                        ast::JillExpression::FunctionCall(ast::JillFunctionCall {
                            reference: ast::JillFunctionReference {
                                modules_path: vec![],
                                associated_type: None,
                                function_name: ast::JillIdentifier(String::from("foo")),
                            },
                            arguments: vec![],
                        }),
                    ],
                }),
                true,
            ),
            (
                // recursive call nested in compiler internal (any argument)
                ast::JillExpression::FunctionCall(ast::JillFunctionCall {
                    reference: ast::JillFunctionReference {
                        modules_path: vec![ast::JillIdentifier(String::from("Mod"))],
                        associated_type: Some(ast::JillIdentifier(String::from("Type"))),
                        function_name: ast::JillIdentifier(String::from("match")),
                    },
                    arguments: vec![
                        ast::JillExpression::VariableName(ast::JillIdentifier(String::from("o"))),
                        ast::JillExpression::Literal(ast::JillLiteral::Integer(-1)),
                        ast::JillExpression::FunctionCall(ast::JillFunctionCall {
                            reference: ast::JillFunctionReference {
                                modules_path: vec![],
                                associated_type: None,
                                function_name: ast::JillIdentifier(String::from("foo")),
                            },
                            arguments: vec![],
                        }),
                        ast::JillExpression::Literal(ast::JillLiteral::Integer(0)),
                    ],
                }),
                true,
            ),
            (
                // recursive call nested in invalid compiler internal
                ast::JillExpression::FunctionCall(ast::JillFunctionCall {
                    reference: ast::JillFunctionReference {
                        modules_path: vec![],
                        associated_type: None,
                        function_name: ast::JillIdentifier(String::from("todo")),
                    },
                    arguments: vec![ast::JillExpression::FunctionCall(ast::JillFunctionCall {
                        reference: ast::JillFunctionReference {
                            modules_path: vec![],
                            associated_type: None,
                            function_name: ast::JillIdentifier(String::from("foo")),
                        },
                        arguments: vec![],
                    })],
                }),
                false,
            ),
        ];

        let mut run_test = |expr| {
            tail_recursion::is_tail_recursive(original_function_name, expr, &mut module_context)
        };

        assert!(tail_recursion_tests
            .iter()
            .all(|(expr, expected)| run_test(expr) == *expected));
    }
}
