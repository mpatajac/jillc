use crate::{
    codegen::context::{ModuleContext, ProgramContext},
    common::ast,
};

use super::{
    common,
    error::{Error, FallableAction},
};

pub fn construct(
    functions: Vec<ast::JillFunction>,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableAction {
    // check for captures in top-level functions (which don't
    // make sense, as there is nothing to capture)
    if let Some(function) = functions.iter().find(|f| !f.captures.is_empty()) {
        return Err(Error::CaptureInTopLevelFunction(function.name.0.clone()));
    }

    for function in functions {
        let instructions =
            common::function_declaration::construct(&function, module_context, program_context)?;

        module_context.output.add_block(instructions.into());
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function_construction() {
        use ast::*;

        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new("Test".to_owned());

        // fn f x = Int::add(x, 2).
        // fn g x = Module::other(x, 7).
        let functions = vec![
            JillFunction {
                name: JillIdentifier("f".to_owned()),
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
            },
            JillFunction {
                name: JillIdentifier("g".to_owned()),
                arguments: vec![JillIdentifier("x".to_string())],
                captures: vec![],
                body: JillFunctionBody {
                    local_functions: vec![],
                    local_variables: vec![],
                    return_expression: JillExpression::FunctionCall(JillFunctionCall {
                        reference: JillFunctionReference {
                            modules_path: vec![JillIdentifier("Module".to_string())],
                            associated_type: None,
                            function_name: JillIdentifier("other".to_owned()),
                        },
                        arguments: vec![
                            JillExpression::VariableName(JillIdentifier("x".to_string())),
                            JillExpression::Literal(JillLiteral::Integer(7)),
                        ],
                    }),
                },
            },
        ];

        let expected_fn1 = vec![
            "function Test.f 0",
            "push argument 0",
            "push constant 2",
            "add",
            "return",
        ];
        let expected_fn2 = vec![
            "function Test.g 0",
            "push argument 0",
            "push constant 7",
            "call Module.other 2",
            "return",
        ];
        let expected = [expected_fn1, expected_fn2].concat().join("\n");

        // construction successful and correct
        assert!(construct(functions, &mut module_context, &mut program_context).is_ok());
        assert_eq!(module_context.output.compile(), expected);
    }

    #[test]
    fn test_capture_in_top_level_function() {
        use ast::*;

        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new("Test".to_owned());

        // fn f x = Int::add(x, 2).
        // fn g x [test] = Module::other(x, test).
        let functions = vec![
            JillFunction {
                name: JillIdentifier("f".to_owned()),
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
            },
            JillFunction {
                name: JillIdentifier("g".to_owned()),
                arguments: vec![JillIdentifier("x".to_string())],
                captures: vec![JillIdentifier("test".to_string())],
                body: JillFunctionBody {
                    local_functions: vec![],
                    local_variables: vec![],
                    return_expression: JillExpression::FunctionCall(JillFunctionCall {
                        reference: JillFunctionReference {
                            modules_path: vec![JillIdentifier("Module".to_string())],
                            associated_type: None,
                            function_name: JillIdentifier("other".to_owned()),
                        },
                        arguments: vec![
                            JillExpression::VariableName(JillIdentifier("x".to_string())),
                            JillExpression::VariableName(JillIdentifier("test".to_string())),
                        ],
                    }),
                },
            },
        ];

        assert!(
            construct(functions, &mut module_context, &mut program_context)
                .is_err_and(|err| matches!(err, Error::CaptureInTopLevelFunction(_)))
        );
    }
}
