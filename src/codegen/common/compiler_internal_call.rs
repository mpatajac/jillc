use crate::{
    codegen::{
        common::{expression, helpers::function::JillFunctionReferenceExtensions},
        context::{module::VariableContext, ModuleContext, ProgramContext},
        error::{Error, FallableInstructions},
        vm,
    },
    common::{ast, CompilerInternalFunction},
};

pub trait ArgumentConstruction: Clone {
    fn construct(
        self,
        expression: &ast::JillExpression,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) -> FallableInstructions;
}

#[derive(Debug, Clone)]
struct NormalArgumentConstruction;

impl ArgumentConstruction for NormalArgumentConstruction {
    fn construct(
        self,
        expression: &ast::JillExpression,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) -> FallableInstructions {
        expression::construct(expression, module_context, program_context)
    }
}

pub(super) fn construct(
    function_call: &ast::JillFunctionCall,
    compiler_internal_function: CompilerInternalFunction,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableInstructions {
    construct_with_custom_argument_construction(
        function_call,
        compiler_internal_function,
        NormalArgumentConstruction,
        module_context,
        program_context,
    )
}

pub(super) fn construct_with_custom_argument_construction(
    function_call: &ast::JillFunctionCall,
    compiler_internal_function: CompilerInternalFunction,
    argument_construction: impl ArgumentConstruction,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableInstructions {
    match compiler_internal_function {
        CompilerInternalFunction::If => construct_if(
            function_call,
            argument_construction,
            module_context,
            program_context,
        ),
        CompilerInternalFunction::IfElse => construct_if_else(
            function_call,
            argument_construction,
            module_context,
            program_context,
        ),
        CompilerInternalFunction::Do => construct_do(
            function_call,
            argument_construction,
            module_context,
            program_context,
        ),
        CompilerInternalFunction::Match => construct_match(
            function_call,
            argument_construction,
            module_context,
            program_context,
        ),
        CompilerInternalFunction::Todo => {
            construct_todo(function_call, module_context, program_context)
        }
    }
}

fn construct_if(
    function_call: &ast::JillFunctionCall,
    argument_construction: impl ArgumentConstruction,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableInstructions {
    let function_reference = &function_call.reference;

    // region: Validation

    // `if` cannot be preceded (have module path or associated type)
    if is_preceded(function_reference) {
        return invalid_compiler_internal_function_call(function_reference);
    }

    // `if` MUST contain EXACTLY 2 expressions -
    // one which will be evaluated as a condition
    // and one which will be performed if condition is fulfilled
    if function_call.arguments.len() != 2 {
        return invalid_compiler_internal_function_call(function_reference);
    }

    // endregion

    // region: Construction

    let condition = &function_call.arguments[0];
    let action = &function_call.arguments[1];

    let skip_if_label = module_context.scope.create_label("SKIP_IF");

    // evaluate condition - if it is false, skip the provided action
    let condition_instructions = [
        expression::construct(condition, module_context, program_context)?,
        vec![
            vm::r#false(),
            vm::command(vm::VMCommand::Eq),
            vm::label(vm::LabelAction::IfGoto, skip_if_label.clone()),
        ],
    ]
    .concat();

    // condition check -> action -> label to skip to if condition not satisfied
    // 								+ null (so expression evaluates to a value)
    let instructions = [
        condition_instructions,
        argument_construction.construct(action, module_context, program_context)?,
        vec![vm::label(vm::LabelAction::Label, skip_if_label), vm::null()],
    ];

    // endregion

    Ok(instructions.concat())
}

fn construct_if_else(
    function_call: &ast::JillFunctionCall,
    argument_construction: impl ArgumentConstruction,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableInstructions {
    let function_reference = &function_call.reference;

    // region: Validation

    // `ifElse` cannot be preceded (have module path or associated type)
    if is_preceded(function_reference) {
        return invalid_compiler_internal_function_call(function_reference);
    }

    // `ifElse` MUST contain EXACTLY 3 expressions -
    // one which will be evaluated as a condition
    // one which will be performed if condition is fulfilled
    // and one which will be performed if condition is not fulfilled
    if function_call.arguments.len() != 3 {
        return invalid_compiler_internal_function_call(function_reference);
    }

    // endregion

    // region: Construction

    let condition = &function_call.arguments[0];
    let true_expression = &function_call.arguments[1];
    let false_expression = &function_call.arguments[2];

    let skip_true_label = module_context.scope.create_label("SKIP_TRUE");
    let skip_false_label = module_context.scope.create_label("SKIP_FALSE");

    // evaluate condition - if it is false, skip the provided action
    let condition_instructions = [
        expression::construct(condition, module_context, program_context)?,
        vec![
            vm::r#false(),
            vm::command(vm::VMCommand::Eq),
            vm::label(vm::LabelAction::IfGoto, skip_true_label.clone()),
        ],
    ]
    .concat();

    // evaluate "true" expression, skip false
    let true_instructions = [
        expression::construct(true_expression, module_context, program_context)?,
        vec![vm::label(vm::LabelAction::Goto, skip_false_label.clone())],
    ]
    .concat();

    // point to skip "true" to, evaluate "false" expression, point to skip "false" to
    let false_instructions = [
        vec![vm::label(vm::LabelAction::Label, skip_true_label)],
        argument_construction.construct(false_expression, module_context, program_context)?,
        vec![vm::label(vm::LabelAction::Label, skip_false_label)],
    ]
    .concat();

    // condition check -> expr if true -> expr if false
    let instructions = [
        condition_instructions,
        true_instructions,
        false_instructions,
    ];

    // endregion

    Ok(instructions.concat())
}

fn construct_do(
    function_call: &ast::JillFunctionCall,
    argument_construction: impl ArgumentConstruction,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableInstructions {
    let function_reference = &function_call.reference;

    // region: Validation

    // `do` cannot be preceded (have module path or associated type)
    if is_preceded(function_reference) {
        return invalid_compiler_internal_function_call(function_reference);
    }

    // `do` MUST contain AT LEAST 2 arguments -
    // function calls to perform (and discard)
    if function_call.arguments.len() < 2 {
        return invalid_compiler_internal_function_call(function_reference);
    }

    // check that the arguments are function calls
    if !function_call.arguments.iter().all(is_function_call) {
        return invalid_compiler_internal_function_call(function_reference);
    }

    // endregion

    // region: Construction

    // add `pop temp {i}` after every call (except last) to discard result
    let temp_index = program_context.temp_segment_index.request();
    let argument_count = function_call.arguments.len();

    let dropped_arguments_instructions = function_call
        .arguments
        .iter()
        .take(argument_count - 1)
        .map(|expr| {
            Ok([
                expression::construct(expr, module_context, program_context)?,
                vec![vm::pop(vm::Segment::Temp, temp_index)],
            ]
            .concat())
        })
        .collect::<Result<Vec<_>, _>>()?
        .concat();

    // last argument (outcome) is not dropped
    let last_argument = function_call
        .arguments
        .last()
        .expect("`do` should have (more than) one argument");

    let last_argument_instructions =
        argument_construction.construct(last_argument, module_context, program_context)?;

    let instructions = [dropped_arguments_instructions, last_argument_instructions].concat();

    program_context.temp_segment_index.release();

    // endregion

    Ok(instructions)
}

fn construct_match(
    function_call: &ast::JillFunctionCall,
    argument_construction: impl ArgumentConstruction,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableInstructions {
    let function_reference = &function_call.reference;

    // region: Validation

    // `match` MUST be type-preceded (it CAN have module path, but it is not required)
    if !is_type_preceded(function_reference) {
        return invalid_compiler_internal_function_call(function_reference);
    }

    // `match` MUST contain AT LEAST 3 expressions -
    // one representing the object whose variant (tag) will be checked
    // and 2 or more expressions to be evaluated based on the object's variant
    // (no point in "checking" only one variant)
    if function_call.arguments.len() < 3 {
        return invalid_compiler_internal_function_call(function_reference);
    }

    // object (instance of custom type) can only be present
    // as a variable or a result of a function call
    let object = function_call
        .arguments
        .first()
        .expect("should have (more than) one argument");

    if !can_be_object(object) {
        return invalid_compiler_internal_function_call(function_reference);
    }

    // endregion

    // region: Construction

    let end_label = module_context.scope.create_label("MATCH_END");

    let tag_fn_reference = ast::JillFunctionReference {
        modules_path: function_reference.modules_path.clone(),
        associated_type: function_reference.associated_type.clone(),
        function_name: ast::JillIdentifier(String::from("tag")),
    };

    // `{Type}_tag` is a type-associated top-level function, so it is not nested (no need for prefix)
    // might be a locally-defined type, so we provide current module's name as default
    let tag_fn_name =
        tag_fn_reference.to_fully_qualified_hack_name(&module_context.module_name, String::new());

    let temp_index = program_context.temp_segment_index.request();
    let tag_storage = VariableContext {
        segment: vm::Segment::Temp,
        index: temp_index,
    };
    // call the `_tag` function, store result in temp storage (to enable reuse)
    let object_tag_instructions = [
        expression::construct(object, module_context, program_context)?,
        vec![vm::call(tag_fn_name, 1), tag_storage.pop()],
    ]
    .concat();

    program_context.temp_segment_index.release();

    let variant_count = function_call.arguments.len() - 1;

    let variant_labels = (0..variant_count)
        .map(|i| module_context.scope.create_label(&format!("VARIANT_{i}")))
        .collect::<Vec<_>>();

    // create checks for all but last variant
    // (if it is not the first n-1, then it must be the last one)
    let variant_check_instructions = (0..variant_count - 1)
        .map(|i| {
            [
                // if tag equals current variant, go to corresponding expression
                tag_storage.push(),
                vec![
                    vm::push(vm::Segment::Constant, i),
                    vm::command(vm::VMCommand::Eq),
                    vm::label(vm::LabelAction::IfGoto, &variant_labels[i]),
                ],
            ]
            .concat()
        })
        .collect::<Vec<_>>()
        .concat();

    let variant_expr_instructions = function_call
        .arguments
        .iter()
        // skip match object
        .skip(1)
        // join with corresponding label
        .zip(variant_labels.iter())
        // variant evaluations are listed in reverse order
        .rev()
        .map(|(expr, label)| {
            Ok([
                // jump label, followed by variant expression and "end" label
                vec![vm::label(vm::LabelAction::Label, label)],
                argument_construction
                    .clone()
                    .construct(expr, module_context, program_context)?,
                vec![vm::label(vm::LabelAction::Goto, end_label.clone())],
            ]
            .concat())
        })
        .collect::<Result<Vec<_>, _>>()?
        .concat();

    // endregion

    let instructions = [
        object_tag_instructions,
        variant_check_instructions,
        variant_expr_instructions,
        vec![vm::label(vm::LabelAction::Label, end_label)],
    ];

    Ok(instructions.concat())
}

fn construct_todo(
    function_call: &ast::JillFunctionCall,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableInstructions {
    let function_reference = &function_call.reference;

    // region: Validation

    // `todo` cannot be preceded (have module path or associated type)
    if is_preceded(function_reference) {
        return invalid_compiler_internal_function_call(function_reference);
    }

    // `todo` can contain AT MOST 1 argument -
    // string containing the accompanying message
    if function_call.arguments.len() > 1 {
        return invalid_compiler_internal_function_call(function_reference);
    }

    // check that the argument (if exists) is a string
    if let Some(argument) = function_call.arguments.first() {
        if !is_string(argument) {
            return invalid_compiler_internal_function_call(function_reference);
        }
    }

    // endregion

    // region: Construction

    let instructions = vec![
        // use error code 0 for todo's
        vm::push(vm::Segment::Constant, 0),
        vm::call(vm::VMFunctionName::from_literal("Sys.error"), 1),
    ];

    // endregion

    Ok(instructions)
}

// region: Helpers

fn is_module_preceded(function_reference: &ast::JillFunctionReference) -> bool {
    function_reference.is_fully_qualified()
}

fn is_type_preceded(function_reference: &ast::JillFunctionReference) -> bool {
    function_reference.associated_type.is_some()
}

fn is_preceded(function_reference: &ast::JillFunctionReference) -> bool {
    is_module_preceded(function_reference) || is_type_preceded(function_reference)
}

fn is_function_call(expr: &ast::JillExpression) -> bool {
    matches!(expr, ast::JillExpression::FunctionCall(_))
}

fn is_string(expr: &ast::JillExpression) -> bool {
    matches!(
        expr,
        ast::JillExpression::Literal(ast::JillLiteral::String(_))
    )
}

fn can_be_object(expr: &ast::JillExpression) -> bool {
    // object (instance of custom type) can only be present
    // as a variable or a result of a function call
    matches!(
        expr,
        ast::JillExpression::VariableName(_) | ast::JillExpression::FunctionCall(_)
    )
}

fn invalid_compiler_internal_function_call(
    function_reference: &ast::JillFunctionReference,
) -> FallableInstructions {
    Err(Error::InvalidCompilerInternalFunctionCall(
        function_reference.reconstruct_source_name(),
    ))
}

// endregion

#[cfg(test)]
mod tests {
    use crate::codegen::{common::function_call, context::module::VariableContextArguments};

    use super::*;
    #[test]
    fn test_if_call() {
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new(String::from("Test"));

        // `if(b, Other::action())`

        assert!(module_context
            .scope
            .add_variable(
                String::from("b"),
                VariableContextArguments::new(vm::Segment::Local),
            )
            .is_ok());

        let call_reference = ast::JillFunctionReference {
            modules_path: vec![ast::JillIdentifier(String::from("Other"))],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("action")),
        };

        let function_reference = ast::JillFunctionReference {
            modules_path: vec![],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("if")),
        };
        let arguments = vec![
            ast::JillExpression::VariableName(ast::JillIdentifier(String::from("b"))),
            ast::JillExpression::FunctionCall(ast::JillFunctionCall {
                reference: call_reference,
                arguments: vec![],
            }),
        ];
        let function_call = ast::JillFunctionCall {
            reference: function_reference,
            arguments,
        };

        let expected = [
            "push local 0",
            "push constant 0",
            "eq",
            "if-goto SKIP_IF_0",
            "call Other.action 0",
            "label SKIP_IF_0",
            "push constant 0",
        ]
        .join("\n");

        assert!(function_call::construct(
            &function_call,
            &mut module_context,
            &mut program_context
        )
        .is_ok_and(
            |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
        ));
    }

    #[test]
    fn test_if_else_call() {
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new(String::from("Test"));

        // `ifElse(b, 5, -7)`

        assert!(module_context
            .scope
            .add_variable(
                String::from("b"),
                VariableContextArguments::new(vm::Segment::Local),
            )
            .is_ok());

        let function_reference = ast::JillFunctionReference {
            modules_path: vec![],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("ifElse")),
        };
        let arguments = vec![
            ast::JillExpression::VariableName(ast::JillIdentifier(String::from("b"))),
            ast::JillExpression::Literal(ast::JillLiteral::Integer(5)),
            ast::JillExpression::Literal(ast::JillLiteral::Integer(-7)),
        ];
        let function_call = ast::JillFunctionCall {
            reference: function_reference,
            arguments,
        };

        let expected = [
            // condition check
            "push local 0",
            "push constant 0",
            "eq",
            "if-goto SKIP_TRUE_0",
            // true
            "push constant 5",
            "goto SKIP_FALSE_0",
            // false
            "label SKIP_TRUE_0",
            "push constant 7",
            "neg",
            "label SKIP_FALSE_0",
        ]
        .join("\n");

        assert!(function_call::construct(
            &function_call,
            &mut module_context,
            &mut program_context
        )
        .is_ok_and(
            |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
        ));
    }

    #[test]
    fn test_do_call() {
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new(String::from("Test"));

        // `do(One::thing(), Other::action())`

        let one_thing = ast::JillFunctionReference {
            modules_path: vec![ast::JillIdentifier(String::from("One"))],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("thing")),
        };

        let other_action = ast::JillFunctionReference {
            modules_path: vec![ast::JillIdentifier(String::from("Other"))],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("action")),
        };

        let function_reference = ast::JillFunctionReference {
            modules_path: vec![],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("do")),
        };
        let arguments = vec![
            ast::JillExpression::FunctionCall(ast::JillFunctionCall {
                reference: one_thing,
                arguments: vec![],
            }),
            ast::JillExpression::FunctionCall(ast::JillFunctionCall {
                reference: other_action,
                arguments: vec![],
            }),
        ];
        let function_call = ast::JillFunctionCall {
            reference: function_reference,
            arguments,
        };

        let expected = ["call One.thing 0", "pop temp 0", "call Other.action 0"].join("\n");

        assert!(function_call::construct(
            &function_call,
            &mut module_context,
            &mut program_context
        )
        .is_ok_and(
            |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
        ));
    }

    #[test]
    fn test_match_call() {
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new(String::from("Test"));

        // `Option::Option:match(o, 0, Option::Some:value(o))`

        assert!(module_context
            .scope
            .add_variable(
                String::from("o"),
                VariableContextArguments::new(vm::Segment::Local),
            )
            .is_ok());

        let call_reference = ast::JillFunctionReference {
            modules_path: vec![ast::JillIdentifier(String::from("Option"))],
            associated_type: Some(ast::JillIdentifier(String::from("Some"))),
            function_name: ast::JillIdentifier(String::from("value")),
        };

        let function_reference = ast::JillFunctionReference {
            modules_path: vec![ast::JillIdentifier(String::from("Option"))],
            associated_type: Some(ast::JillIdentifier(String::from("Option"))),
            function_name: ast::JillIdentifier(String::from("match")),
        };

        let arguments = vec![
            ast::JillExpression::VariableName(ast::JillIdentifier(String::from("o"))),
            ast::JillExpression::Literal(ast::JillLiteral::Integer(0)),
            ast::JillExpression::FunctionCall(ast::JillFunctionCall {
                reference: call_reference,
                arguments: vec![ast::JillExpression::VariableName(ast::JillIdentifier(
                    String::from("o"),
                ))],
            }),
        ];
        let function_call = ast::JillFunctionCall {
            reference: function_reference,
            arguments,
        };

        let expected = [
            // tag
            "push local 0",
            "call Option.Option_tag 1",
            "pop temp 0",
            // variant checks
            "push temp 0",
            "push constant 0",
            "eq",
            "if-goto VARIANT_0_0",
            // variant 1
            "label VARIANT_1_0",
            "push local 0",
            "call Option.Some_value 1",
            "goto MATCH_END_0",
            // variant 0
            "label VARIANT_0_0",
            "push constant 0",
            "goto MATCH_END_0",
            // end
            "label MATCH_END_0",
        ]
        .join("\n");

        assert!(function_call::construct(
            &function_call,
            &mut module_context,
            &mut program_context
        )
        .is_ok_and(
            |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
        ));
    }

    #[test]
    fn test_todo_call() {
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new(String::from("Test"));

        // `todo()`
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("todo")),
        };
        let function_call = ast::JillFunctionCall {
            reference: function_reference,
            arguments: vec![],
        };

        let expected = ["push constant 0", "call Sys.error 1"].join("\n");

        assert!(function_call::construct(
            &function_call,
            &mut module_context,
            &mut program_context
        )
        .is_ok_and(
            |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
        ));

        // `todo("test message")`
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("todo")),
        };
        let function_call = ast::JillFunctionCall {
            reference: function_reference,
            arguments: vec![ast::JillExpression::Literal(ast::JillLiteral::String(
                "test message".to_string(),
            ))],
        };

        let expected = ["push constant 0", "call Sys.error 1"].join("\n");

        assert!(function_call::construct(
            &function_call,
            &mut module_context,
            &mut program_context
        )
        .is_ok_and(
            |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
        ));
    }

    #[allow(clippy::too_many_lines)]
    #[test]
    fn test_invariants() {
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new(String::from("Test"));

        // region: if

        let if_preceded = ast::JillFunctionCall {
            reference: ast::JillFunctionReference {
                modules_path: vec![ast::JillIdentifier(String::from("Mod"))],
                associated_type: None,
                function_name: ast::JillIdentifier(String::from("if")),
            },
            arguments: vec![],
        };

        let if_invalid_arg_count = ast::JillFunctionCall {
            reference: ast::JillFunctionReference {
                modules_path: vec![],
                associated_type: None,
                function_name: ast::JillIdentifier(String::from("if")),
            },
            arguments: vec![
                ast::JillExpression::Literal(ast::JillLiteral::Bool(true)),
                ast::JillExpression::Literal(ast::JillLiteral::Integer(5)),
                ast::JillExpression::Literal(ast::JillLiteral::Integer(7)),
                ast::JillExpression::Literal(ast::JillLiteral::Integer(3)),
            ],
        };

        // endregion

        // region: ifElse

        let if_else_preceded = ast::JillFunctionCall {
            reference: ast::JillFunctionReference {
                modules_path: vec![ast::JillIdentifier(String::from("Mod"))],
                associated_type: None,
                function_name: ast::JillIdentifier(String::from("ifElse")),
            },
            arguments: vec![],
        };

        let if_else_invalid_arg_count = ast::JillFunctionCall {
            reference: ast::JillFunctionReference {
                modules_path: vec![],
                associated_type: None,
                function_name: ast::JillIdentifier(String::from("ifElse")),
            },
            arguments: vec![
                ast::JillExpression::Literal(ast::JillLiteral::Bool(true)),
                ast::JillExpression::Literal(ast::JillLiteral::Integer(5)),
                ast::JillExpression::Literal(ast::JillLiteral::Integer(7)),
                ast::JillExpression::Literal(ast::JillLiteral::Integer(3)),
                ast::JillExpression::Literal(ast::JillLiteral::Integer(4)),
            ],
        };

        // endregion

        // region: do

        let do_preceded = ast::JillFunctionCall {
            reference: ast::JillFunctionReference {
                modules_path: vec![ast::JillIdentifier(String::from("Mod"))],
                associated_type: None,
                function_name: ast::JillIdentifier(String::from("do")),
            },
            arguments: vec![],
        };

        let do_empty = ast::JillFunctionCall {
            reference: ast::JillFunctionReference {
                modules_path: vec![],
                associated_type: None,
                function_name: ast::JillIdentifier(String::from("do")),
            },
            arguments: vec![],
        };

        let do_non_function_call = ast::JillFunctionCall {
            reference: ast::JillFunctionReference {
                modules_path: vec![],
                associated_type: None,
                function_name: ast::JillIdentifier(String::from("do")),
            },
            arguments: vec![
                ast::JillExpression::Literal(ast::JillLiteral::Integer(5)),
                ast::JillExpression::Literal(ast::JillLiteral::Integer(4)),
            ],
        };

        // endregion

        // region: match

        let match_not_type_preceded = ast::JillFunctionCall {
            reference: ast::JillFunctionReference {
                modules_path: vec![ast::JillIdentifier(String::from("Mod"))],
                associated_type: None,
                function_name: ast::JillIdentifier(String::from("match")),
            },
            arguments: vec![],
        };

        let match_invalid_object_kind = ast::JillFunctionCall {
            reference: ast::JillFunctionReference {
                modules_path: vec![ast::JillIdentifier(String::from("Mod"))],
                associated_type: Some(ast::JillIdentifier(String::from("Type"))),
                function_name: ast::JillIdentifier(String::from("match")),
            },
            arguments: vec![
                ast::JillExpression::Literal(ast::JillLiteral::Bool(true)),
                ast::JillExpression::Literal(ast::JillLiteral::Integer(5)),
                ast::JillExpression::Literal(ast::JillLiteral::Integer(7)),
            ],
        };

        assert!(module_context
            .scope
            .add_variable(
                String::from("a"),
                VariableContextArguments::new(vm::Segment::Static),
            )
            .is_ok());

        let match_invalid_arg_count = ast::JillFunctionCall {
            reference: ast::JillFunctionReference {
                modules_path: vec![ast::JillIdentifier(String::from("Mod"))],
                associated_type: Some(ast::JillIdentifier(String::from("Type"))),
                function_name: ast::JillIdentifier(String::from("match")),
            },
            arguments: vec![
                ast::JillExpression::VariableName(ast::JillIdentifier(String::from("a"))),
                ast::JillExpression::Literal(ast::JillLiteral::Integer(5)),
            ],
        };

        // endregion

        // region: todo

        let todo_preceded = ast::JillFunctionCall {
            reference: ast::JillFunctionReference {
                modules_path: vec![ast::JillIdentifier(String::from("Mod"))],
                associated_type: None,
                function_name: ast::JillIdentifier(String::from("todo")),
            },
            arguments: vec![],
        };

        let todo_too_many_args = ast::JillFunctionCall {
            reference: ast::JillFunctionReference {
                modules_path: vec![],
                associated_type: None,
                function_name: ast::JillIdentifier(String::from("todo")),
            },
            arguments: vec![
                ast::JillExpression::Literal(ast::JillLiteral::String("test message".to_string())),
                ast::JillExpression::Literal(ast::JillLiteral::String(
                    "another message".to_string(),
                )),
            ],
        };

        let todo_invalid_args = ast::JillFunctionCall {
            reference: ast::JillFunctionReference {
                modules_path: vec![],
                associated_type: None,
                function_name: ast::JillIdentifier(String::from("todo")),
            },
            arguments: vec![ast::JillExpression::Literal(ast::JillLiteral::Integer(3))],
        };

        // endregion

        let invalid_calls = [
            // if
            if_preceded,
            if_invalid_arg_count,
            // ifElse
            if_else_preceded,
            if_else_invalid_arg_count,
            // do
            do_preceded,
            do_empty,
            do_non_function_call,
            // match
            match_not_type_preceded,
            match_invalid_object_kind,
            match_invalid_arg_count,
            // todo
            todo_preceded,
            todo_too_many_args,
            todo_invalid_args,
        ];

        let is_invalid_call = |call| {
            function_call::construct(call, &mut module_context, &mut program_context)
                .is_err_and(|err| matches!(err, Error::InvalidCompilerInternalFunctionCall(_)))
        };

        assert!(invalid_calls.iter().all(is_invalid_call));
    }
}
