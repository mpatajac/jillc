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
    let has_captures = !function.captures.is_empty();

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
        // register only using function name - prefix will get constructed in the process
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

    // register captures in scope
    for capture in &function.captures {
        module_context.scope.add_variable(
            capture.0.clone(),
            VariableContextArguments::new(vm::Segment::Capture(arity)),
        )?;
    }

    let function_reference = ast::JillFunctionReference::from_function_definition(function);

    let vm_function_name = function_reference
        .to_fully_qualified_hack_name(&module_context.module_name, function_context.prefix);

    // add metadata (for dispatch)
    program_context.program_metadata.log_function_metadata(
        vm_function_name.clone(),
        arity,
        has_captures,
    )?;

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
        function.name.0.clone(),
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
    function_name: String,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableInstructions {
    let is_tail_recursive = tail_recursion::contains_tail_recursive_call(
        &function_body.return_expression,
        &function_name,
        module_context,
    );

    let function_start_label = module_context.scope.create_label("REC_CALL");
    let function_start_label_instructions = if is_tail_recursive {
        vec![vm::label(
            vm::LabelAction::Label,
            function_start_label.clone(),
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
            &function_body.return_expression,
            function_name,
            function_start_label,
            module_context,
            program_context,
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
        function_start_label_instructions,
        local_variables_instructions,
        return_instructions,
        vec![vm::vm_return()],
    ]
    .concat())
}

mod tail_recursion {
    use crate::{
        codegen::{
            common::{
                compiler_internal_call::{self, ArgumentConstruction},
                expression,
                function_call::{self, direct_call::LocalCallInfo, FunctionCallKind},
                helpers::function::JillFunctionReferenceExtensions,
            },
            context::{ModuleContext, ProgramContext},
            error::FallableInstructions,
            vm,
        },
        common::{ast, CompilerInternalFunction},
    };

    // give type alias to make OR's in match arms nicer
    type Cif = CompilerInternalFunction;

    pub(super) fn contains_tail_recursive_call(
        return_expression: &ast::JillExpression,
        original_function_name: &str,
        module_context: &mut ModuleContext,
    ) -> bool {
        // has to be a function call
        let ast::JillExpression::FunctionCall(function_call) = return_expression else {
            return false;
        };

        // direct recursive call
        if has_original_name(&function_call.reference, original_function_name) {
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

                contains_tail_recursive_call(last_argument, original_function_name, module_context)
            }

            // check that any variant expression is a nested recursive call
            Cif::Match => function_call.arguments.iter().skip(1).any(|expr| {
                contains_tail_recursive_call(expr, original_function_name, module_context)
            }),

            // cannot have a nested recursive call
            Cif::Todo | Cif::Free => false,
        }
    }

    pub(super) fn construct_tail_recursive_call(
        return_expression: &ast::JillExpression,
        original_function_name: String,
        function_start_label: String,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) -> FallableInstructions {
        RecursiveConstruction {
            original_function_name,
            function_start_label,
        }
        .construct(return_expression, module_context, program_context)
    }

    #[derive(Debug, Clone)]
    struct RecursiveConstruction {
        original_function_name: String,
        function_start_label: String,
    }

    impl function_call::direct_call::CallConstruction for RecursiveConstruction {
        fn construct(
            self,
            function_call: &ast::JillFunctionCall,
            _: Option<LocalCallInfo>,
            _: bool,
        ) -> Vec<vm::VMInstruction> {
            let argument_reset_instructions = (0..function_call.arguments.len())
                // argument values are "stacked" in order, so we need to pop them backwards
                .rev()
                .map(|i| vm::pop(vm::Segment::Argument, i))
                .collect();

            // reset the function arguments and jump to the start of the function
            [
                argument_reset_instructions,
                vec![vm::label(vm::LabelAction::Goto, self.function_start_label)],
            ]
            .concat()
        }
    }

    impl compiler_internal_call::ArgumentConstruction for RecursiveConstruction {
        fn construct(
            self,
            expression: &ast::JillExpression,
            module_context: &mut ModuleContext,
            program_context: &mut ProgramContext,
        ) -> FallableInstructions {
            // has to be a function call
            if let ast::JillExpression::FunctionCall(function_call) = expression {
                // direct recursive call
                if has_original_name(&function_call.reference, &self.original_function_name) {
                    if let Some(function_context) = module_context
                        .scope
                        .search_function(&function_call.reference.type_associated_function_name())
                    {
                        return function_call::direct_call::construct_module_local(
                            function_call,
                            function_context,
                            self,
                            module_context,
                            program_context,
                        );
                    }
                }

                // if not a direct recursive call, has to be a recursive call
                // nested in a compiler internal function call
                if let FunctionCallKind::CompilerInternal(compiler_internal_function) =
                    function_call::determine_function_call_kind(
                        &function_call.reference,
                        module_context,
                    )
                {
                    return compiler_internal_call::construct_with_custom_argument_construction(
                        function_call,
                        compiler_internal_function,
                        self,
                        module_context,
                        program_context,
                    );
                }
            }

            // not a tail-recursive call => apply regular expression construction
            expression::construct(expression, module_context, program_context)
        }
    }

    fn has_original_name(
        function_reference: &ast::JillFunctionReference,
        original_function_name: &str,
    ) -> bool {
        !function_reference.is_fully_qualified()
            && function_reference.associated_type.is_none()
            && function_reference.function_name.0 == original_function_name
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[allow(clippy::too_many_lines)]
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
            tail_recursion::contains_tail_recursive_call(
                expr,
                original_function_name,
                &mut module_context,
            )
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

        // fn g x = Int::add(x, 2).
        let function = JillFunction {
            name: JillIdentifier("g".to_owned()),
            arguments: vec![JillIdentifier("x".to_string())],
            captures: vec![],
            body: JillFunctionBody {
                local_functions: vec![],
                local_variables: vec![],
                return_expression: JillExpression::FunctionCall(JillFunctionCall {
                    reference: JillFunctionReference {
                        modules_path: vec![JillIdentifier("Int".to_string())],
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
            "add",
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

               let c = Int::mult(a, b),
               let d = Int::sub(c, 6),

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
                                modules_path: vec![JillIdentifier("Int".to_owned())],
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
                                modules_path: vec![JillIdentifier("Int".to_owned())],
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
            "gt",
            "return",
            // main function
            "function Test.foo 2",
            // let c = Int::mult(a, b)
            "push argument 0",
            "push argument 1",
            "call Math.multiply 2",
            "pop local 0",
            // let d = Int::sub(c, 6)
            "push local 0",
            "push constant 6",
            "sub",
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

    #[allow(clippy::too_many_lines)]
    #[test]
    fn test_shallow_tail_recursion() {
        use ast::*;

        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new("Test".to_owned());

        /*
           fn at list index =
               ifElse(
                   Bool::eq(index, 0),
                   List::List:head(list),
                   at(List::List:tail(list), Utils::MathExt::dec(index))
               ).
        */
        let function = JillFunction {
            name: JillIdentifier("at".to_owned()),
            arguments: vec![
                JillIdentifier("list".to_owned()),
                JillIdentifier("index".to_owned()),
            ],
            captures: vec![],
            body: JillFunctionBody {
                local_functions: vec![],
                local_variables: vec![],
                return_expression: JillExpression::FunctionCall(JillFunctionCall {
                    reference: JillFunctionReference {
                        modules_path: vec![],
                        associated_type: None,
                        function_name: JillIdentifier("ifElse".to_owned()),
                    },
                    arguments: vec![
                        JillExpression::FunctionCall(JillFunctionCall {
                            reference: JillFunctionReference {
                                modules_path: vec![JillIdentifier("Bool".to_owned())],
                                associated_type: None,
                                function_name: JillIdentifier("eq".to_owned()),
                            },
                            arguments: vec![
                                JillExpression::VariableName(JillIdentifier("index".to_owned())),
                                JillExpression::Literal(JillLiteral::Integer(0)),
                            ],
                        }),
                        JillExpression::FunctionCall(JillFunctionCall {
                            reference: JillFunctionReference {
                                modules_path: vec![JillIdentifier("List".to_owned())],
                                associated_type: Some(JillIdentifier("List".to_owned())),
                                function_name: JillIdentifier("head".to_owned()),
                            },
                            arguments: vec![JillExpression::VariableName(JillIdentifier(
                                "list".to_owned(),
                            ))],
                        }),
                        JillExpression::FunctionCall(JillFunctionCall {
                            reference: JillFunctionReference {
                                modules_path: vec![],
                                associated_type: None,
                                function_name: JillIdentifier("at".to_owned()),
                            },
                            arguments: vec![
                                JillExpression::FunctionCall(JillFunctionCall {
                                    reference: JillFunctionReference {
                                        modules_path: vec![JillIdentifier("List".to_owned())],
                                        associated_type: Some(JillIdentifier("List".to_owned())),
                                        function_name: JillIdentifier("tail".to_owned()),
                                    },
                                    arguments: vec![JillExpression::VariableName(JillIdentifier(
                                        "list".to_owned(),
                                    ))],
                                }),
                                JillExpression::FunctionCall(JillFunctionCall {
                                    reference: JillFunctionReference {
                                        modules_path: vec![
                                            JillIdentifier("Utils".to_owned()),
                                            JillIdentifier("MathExt".to_owned()),
                                        ],
                                        associated_type: None,
                                        function_name: JillIdentifier("dec".to_owned()),
                                    },
                                    arguments: vec![JillExpression::VariableName(JillIdentifier(
                                        "index".to_owned(),
                                    ))],
                                }),
                            ],
                        }),
                    ],
                }),
            },
        };

        let expected = [
            "function Test.at 0",
            "label REC_CALL_0",
            // Bool::eq(index, 0)
            "push argument 1",
            "push constant 0",
            "eq",
            "push constant 0",
            "eq",
            "if-goto SKIP_TRUE_0",
            // List::List:head(list)
            "push argument 0",
            "call List.List_head 1",
            "goto SKIP_FALSE_0",
            "label SKIP_TRUE_0",
            // at(List::List:tail(list), Utils::MathExt::dec(index))
            "push argument 0",
            "call List.List_tail 1",
            "push argument 1",
            "call Utils_MathExt.dec 1",
            "pop argument 1",
            "pop argument 0",
            "goto REC_CALL_0",
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

    #[test]
    fn test_deeply_nested_tail_recursion() {
        use ast::*;

        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new("Test".to_owned());

        /*
            fn _choosePosition startingPosition =
                do(
                    Sys::wait(100),
                    Game::clearPositionHighlight(5),
                    ifElse(True, 5, _choosePosition(5))
                ).

        */
        let function = JillFunction {
            name: JillIdentifier("_choosePosition".to_owned()),
            arguments: vec![JillIdentifier("startingPosition".to_owned())],
            captures: vec![],
            body: JillFunctionBody {
                local_functions: vec![],
                local_variables: vec![],
                return_expression: JillExpression::FunctionCall(JillFunctionCall {
                    reference: JillFunctionReference {
                        modules_path: vec![],
                        associated_type: None,
                        function_name: JillIdentifier("do".to_owned()),
                    },
                    arguments: vec![
                        JillExpression::FunctionCall(JillFunctionCall {
                            reference: JillFunctionReference {
                                modules_path: vec![JillIdentifier("Sys".to_owned())],
                                associated_type: None,
                                function_name: JillIdentifier("wait".to_owned()),
                            },
                            arguments: vec![JillExpression::Literal(JillLiteral::Integer(100))],
                        }),
                        JillExpression::FunctionCall(JillFunctionCall {
                            reference: JillFunctionReference {
                                modules_path: vec![JillIdentifier("Game".to_owned())],
                                associated_type: None,
                                function_name: JillIdentifier("clearPositionHighlight".to_owned()),
                            },
                            arguments: vec![JillExpression::Literal(JillLiteral::Integer(5))],
                        }),
                        JillExpression::FunctionCall(JillFunctionCall {
                            reference: JillFunctionReference {
                                modules_path: vec![],
                                associated_type: None,
                                function_name: JillIdentifier("ifElse".to_owned()),
                            },
                            arguments: vec![
                                JillExpression::Literal(JillLiteral::Bool(true)),
                                JillExpression::Literal(JillLiteral::Integer(5)),
                                JillExpression::FunctionCall(JillFunctionCall {
                                    reference: JillFunctionReference {
                                        modules_path: vec![],
                                        associated_type: None,
                                        function_name: JillIdentifier("_choosePosition".to_owned()),
                                    },
                                    arguments: vec![JillExpression::Literal(JillLiteral::Integer(
                                        5,
                                    ))],
                                }),
                            ],
                        }),
                    ],
                }),
            },
        };

        let expected = [
            "function Test._choosePosition 0",
            "label REC_CALL_0",
            // Sys::wait(100)
            "push constant 100",
            "call Sys.wait 1",
            "pop temp 0",
            // Game::clearPositionHighlight(5)
            "push constant 5",
            "call Game.clearPositionHighlight 1",
            "pop temp 0",
            // ifElse(True, 5, _choosePosition(5))
            "push constant 1",
            "neg",
            "push constant 0",
            "eq",
            "if-goto SKIP_TRUE_0",
            "push constant 5",
            "goto SKIP_FALSE_0",
            "label SKIP_TRUE_0",
            "push constant 5",
            "pop argument 0",
            "goto REC_CALL_0",
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
