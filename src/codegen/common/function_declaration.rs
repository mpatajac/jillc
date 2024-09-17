use crate::{
    codegen::{
        common::{expression, helpers::function::JillFunctionReferenceExtensions, variable},
        context::{
            module::{FunctionContextArguments, VariableContextArguments},
            ModuleContext, ProgramContext,
        },
        error::{Error, FallableInstructions},
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
        .collect::<Vec<_>>();

    // check that all captures are existing variables in scope
    // NOTE: since we search for variables in the parent function's scope,
    // we need to perform this check before we add the function to scope.
    if let Some(capture) = capture_names
        .iter()
        .find(|capture| module_context.scope.search_variable(capture).is_none())
    {
        return Err(Error::CaptureNotInScope(capture.to_string()));
    }

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

    let function_reference = ast::JillFunctionReference::from_function_definition(function);

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

    let function_body_instructions = construct_body(
        &function.body,
        &function.name.0,
        module_context,
        program_context,
    )?;

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
    function_name: &str,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableInstructions {
    let is_tail_recursive = tail_recursion::is_tail_recursive(
        function_name,
        &function_body.return_expression,
        module_context,
    );

    let function_start_label = if is_tail_recursive {
        vec![vm::label(
            vm::LabelAction::Label,
            module_context.scope.create_label("REC_CALL"),
        )]
    } else {
        Vec::new()
    };

    let local_variables_instructions = function_body
        .local_variables
        .iter()
        .map(|var| variable::construct(var, vm::Segment::Local, module_context, program_context))
        .collect::<Result<Vec<_>, _>>()?
        .concat();

    let return_instructions = if is_tail_recursive {
        // in case of a tail-recursive call, simulate function call by
        // resetting the function arguments and jumping to the start of the function
        tail_recursion::construct_tail_recursive_call(
            function_name,
            &function_body.return_expression,
            module_context,
        )
    } else {
        // otherwise just evaluate the return expression
        expression::construct(
            &function_body.return_expression,
            module_context,
            program_context,
        )
    }?;

    Ok([
        function_start_label,
        local_variables_instructions,
        return_instructions,
        vec![vm::vm_return()],
    ]
    .concat())
}

mod tail_recursion {
    use crate::{
        codegen::{
            common::function_call::{self, FunctionCallKind},
            context::ModuleContext,
            error::FallableInstructions,
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

    pub(super) fn construct_tail_recursive_call(
        original_function_name: &str,
        return_expression: &ast::JillExpression,
        module_context: &mut ModuleContext,
    ) -> FallableInstructions {
        todo!()
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

    #[test]
    fn test_simple_function() {
        use ast::*;

        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new("Test".to_owned());

        // fn g x = Math::add(x, 2).
        let function = JillFunction {
            name: JillIdentifier("g".to_owned()),
            arguments: vec![JillIdentifier("x".to_string())],
            captures: vec![],
            body: JillFunctionBody {
                local_functions: vec![],
                local_variables: vec![],
                return_expression: JillExpression::FunctionCall(JillFunctionCall {
                    reference: JillFunctionReference {
                        modules_path: vec![JillIdentifier("Math".to_string())],
                        associated_type: None,
                        function_name: JillIdentifier("add".to_owned()),
                    },
                    arguments: vec![
                        JillExpression::VariableName(JillIdentifier("x".to_string())),
                        JillExpression::Literal(JillLiteral::Integer(2)),
                    ],
                }),
            },
        };

        let expected = [
            "function Test.g 0",
            "push argument 0",
            "push constant 2",
            "call Math.add 2",
            "return",
        ]
        .join("\n");

        assert!(
            construct(&function, &mut module_context, &mut program_context).is_ok_and(
                |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
            )
        );
    }

    #[allow(clippy::too_many_lines)]
    #[test]
    fn test_compound_function() {
        use ast::*;

        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new("Test".to_owned());

        /*
           fn foo a b =
               fn bar x = Bool::gt(x, 2).

               let c = Math::mult(a, b),
               let d = Math::sub(c, 6),

               ifElse(bar(d), 1, -1).
        */
        let function = JillFunction {
            name: JillIdentifier("foo".to_owned()),
            arguments: vec![
                JillIdentifier("a".to_owned()),
                JillIdentifier("b".to_owned()),
            ],
            captures: vec![],
            body: JillFunctionBody {
                local_functions: vec![JillFunction {
                    name: JillIdentifier("bar".to_owned()),
                    arguments: vec![JillIdentifier("x".to_owned())],
                    captures: vec![],
                    body: JillFunctionBody {
                        local_functions: vec![],
                        local_variables: vec![],
                        return_expression: JillExpression::FunctionCall(JillFunctionCall {
                            reference: JillFunctionReference {
                                modules_path: vec![JillIdentifier("Bool".to_owned())],
                                associated_type: None,
                                function_name: JillIdentifier("gt".to_owned()),
                            },
                            arguments: vec![
                                JillExpression::VariableName(JillIdentifier("x".to_owned())),
                                JillExpression::Literal(JillLiteral::Integer(2)),
                            ],
                        }),
                    },
                }],
                local_variables: vec![
                    JillVariable {
                        name: JillIdentifier("c".to_owned()),
                        value: JillExpression::FunctionCall(JillFunctionCall {
                            reference: JillFunctionReference {
                                modules_path: vec![JillIdentifier("Math".to_owned())],
                                associated_type: None,
                                function_name: JillIdentifier("mult".to_owned()),
                            },
                            arguments: vec![
                                JillExpression::VariableName(JillIdentifier("a".to_owned())),
                                JillExpression::VariableName(JillIdentifier("b".to_owned())),
                            ],
                        }),
                    },
                    JillVariable {
                        name: JillIdentifier("d".to_owned()),
                        value: JillExpression::FunctionCall(JillFunctionCall {
                            reference: JillFunctionReference {
                                modules_path: vec![JillIdentifier("Math".to_owned())],
                                associated_type: None,
                                function_name: JillIdentifier("sub".to_owned()),
                            },
                            arguments: vec![
                                JillExpression::VariableName(JillIdentifier("c".to_owned())),
                                JillExpression::Literal(JillLiteral::Integer(6)),
                            ],
                        }),
                    },
                ],
                return_expression: JillExpression::FunctionCall(JillFunctionCall {
                    reference: JillFunctionReference {
                        modules_path: vec![],
                        associated_type: None,
                        function_name: JillIdentifier("ifElse".to_owned()),
                    },
                    arguments: vec![
                        JillExpression::FunctionCall(JillFunctionCall {
                            reference: JillFunctionReference {
                                modules_path: vec![],
                                associated_type: None,
                                function_name: JillIdentifier("bar".to_owned()),
                            },
                            arguments: vec![JillExpression::VariableName(JillIdentifier(
                                "d".to_owned(),
                            ))],
                        }),
                        JillExpression::Literal(JillLiteral::Integer(1)),
                        JillExpression::Literal(JillLiteral::Integer(-1)),
                    ],
                }),
            },
        };

        let expected = [
            // nested function
            "function Test.foo_bar 0",
            "push argument 0",
            "push constant 2",
            "call Bool.gt 2",
            "return",
            // main function
            "function Test.foo 2",
            // let c = Math::mult(a, b)
            "push argument 0",
            "push argument 1",
            "call Math.mult 2",
            "pop local 0",
            // let d = Math::sub(c, 6)
            "push local 0",
            "push constant 6",
            "call Math.sub 2",
            "pop local 1",
            // ifElse(bar(d), 1, -1)
            "push local 1",
            "call Test.foo_bar 1",
            "push constant 0",
            "eq",
            "if-goto SKIP_TRUE_0",
            "push constant 1",
            "goto SKIP_FALSE_0",
            "label SKIP_TRUE_0",
            "push constant 1",
            "neg",
            "label SKIP_FALSE_0",
            "return",
        ]
        .join("\n");

        assert!(
            construct(&function, &mut module_context, &mut program_context).is_ok_and(
                |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
            )
        );
    }
}
