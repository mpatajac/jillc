use std::str::FromStr;

use strum::VariantNames;

use crate::{
    codegen::{
        context::{
            module::{FunctionContext, VariableContext},
            ModuleContext, ProgramContext,
        },
        error::{Error, FallableInstructions},
    },
    common::{ast, CompilerInternalFunction},
};

use super::helpers::function::JillFunctionReferenceExtensions;

pub fn construct(
    function_call: &ast::JillFunctionCall,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableInstructions {
    match determine_function_call_kind(&function_call.reference, module_context) {
        FunctionCallKind::CompilerInternal(compiler_internal_function) => {
            compiler_internal_call::construct(
                function_call,
                compiler_internal_function,
                module_context,
                program_context,
            )
        }
        FunctionCallKind::Variable(variable_context) => variable_call::construct(
            function_call,
            variable_context,
            module_context,
            program_context,
        ),
        FunctionCallKind::ModuleLocalFunction(function_context) => {
            direct_call::construct_module_local(
                function_call,
                function_context,
                module_context,
                program_context,
            )
        }
        FunctionCallKind::ModuleForeignFunction => {
            direct_call::construct_module_foreign(function_call, module_context, program_context)
        }
        FunctionCallKind::Invalid => invalid_function_call(function_call),
    }
}

fn invalid_function_call(function_call: &ast::JillFunctionCall) -> FallableInstructions {
    Err(Error::InvalidFunctionCall(
        function_call.reference.reconstruct_source_name(),
    ))
}

#[derive(Debug, PartialEq, Eq)]
enum FunctionCallKind {
    CompilerInternal(CompilerInternalFunction),
    Variable(VariableContext),
    ModuleLocalFunction(FunctionContext),
    ModuleForeignFunction,
    Invalid,
}

fn determine_function_call_kind(
    function_reference: &ast::JillFunctionReference,
    module_context: &ModuleContext,
) -> FunctionCallKind {
    let function_name = &function_reference.function_name.0;

    // check that the name matches one of the compiler internal functions
    // NOTE: dedicated function will check all required invariants
    if CompilerInternalFunction::VARIANTS.contains(&function_name.as_str()) {
        let compiler_internal_function = CompilerInternalFunction::from_str(function_name)
            .expect("should be one of compiler internal function variants");

        return FunctionCallKind::CompilerInternal(compiler_internal_function);
    }

    if function_reference.is_fully_qualified() {
        // regular, direct call to a function from another module
        return FunctionCallKind::ModuleForeignFunction;
    }

    // not fully qualified => variable or a module-local function
    // NOTE: this could also be a type-related function,
    // but they are also registered as a module-local function

    // check "variable" case
    if let Some(variable_context) = module_context.scope.search_variable(function_name) {
        return FunctionCallKind::Variable(variable_context);
    }

    // check "module-local function" case
    // NOTE: split from "module-foreign" functions
    // so we can use context for name prefix
    if let Some(function_context) = module_context
        .scope
        // TODO!: figure out naming
        .search_function(&function_reference.function_name.0)
    {
        return FunctionCallKind::ModuleLocalFunction(function_context);
    }

    // not fully qualified AND not found in scope => invalid
    FunctionCallKind::Invalid

    // NOTE: no check for "recursive" case - we cannot detect
    // tail-recursion at this point, and we cannot perform
    // the optimization to all recursive calls
}

mod compiler_internal_call {
    use crate::{
        codegen::{
            common::{expression, helpers::function::JillFunctionReferenceExtensions},
            error::{Error, FallableInstructions},
            vm,
        },
        common::CompilerInternalFunction,
    };

    use super::{ast, ModuleContext, ProgramContext};

    pub(super) fn construct(
        function_call: &ast::JillFunctionCall,
        compiler_internal_function: CompilerInternalFunction,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) -> FallableInstructions {
        match compiler_internal_function {
            CompilerInternalFunction::If => {
                construct_if(function_call, module_context, program_context)
            }
            CompilerInternalFunction::IfElse => {
                construct_if_else(function_call, module_context, program_context)
            }
            CompilerInternalFunction::Todo => {
                construct_todo(function_call, module_context, program_context)
            }
            _ => todo!(),
        }
    }

    fn construct_if(
        function_call: &ast::JillFunctionCall,
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
        let instructions = [
            condition_instructions,
            expression::construct(action, module_context, program_context)?,
            vec![vm::label(vm::LabelAction::Label, skip_if_label)],
        ];

        // endregion

        Ok(instructions.concat())
    }

    fn construct_if_else(
        function_call: &ast::JillFunctionCall,
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
            expression::construct(false_expression, module_context, program_context)?,
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

    fn is_string(expr: &ast::JillExpression) -> bool {
        matches!(
            expr,
            ast::JillExpression::Literal(ast::JillLiteral::String(_))
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
}

mod variable_call {
    use crate::codegen::{
        common::{expression, helpers},
        context::module::VariableContext,
        error::FallableInstructions,
        vm,
    };

    use super::{ast, ModuleContext, ProgramContext};

    pub(super) fn construct(
        function_call: &ast::JillFunctionCall,
        variable_context: VariableContext,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) -> FallableInstructions {
        let arguments_array_instructions = helpers::array::build_array_instructions(
            &function_call.arguments,
            |expr| expression::construct(expr, module_context, program_context),
            false,
        )?;

        let call_instructions = vec![
            variable_context.push(),
            // TODO: `request` index from scope
            vm::push(vm::Segment::Temp, 1),
            vm::call(vm::VMFunctionName::from_literal("Fn._call"), 2),
        ];

        Ok([arguments_array_instructions, call_instructions].concat())
    }
}

mod direct_call {
    use crate::codegen::{
        common::{expression, helpers::function::JillFunctionReferenceExtensions},
        context::module::FunctionContext,
        error::FallableInstructions,
        jillstd, vm,
    };

    use super::{ast, invalid_function_call, ModuleContext, ProgramContext};

    pub(super) fn construct_module_foreign(
        function_call: &ast::JillFunctionCall,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) -> FallableInstructions {
        // track (potential) `JillStd` function occurences
        let std_function_usage_note_outcome = program_context
            .std_usage_tracker
            .note_usage(&function_call.reference);
        if std_function_usage_note_outcome
            == jillstd::JillStdFunctionUsageNoteOutcome::FunctionNotPresentInModule
        {
            return invalid_function_call(function_call);
        }

        construct(function_call, None, module_context, program_context)
    }

    pub(super) fn construct_module_local(
        function_call: &ast::JillFunctionCall,
        function_context: FunctionContext,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) -> FallableInstructions {
        let local_call_info = LocalCallInfo {
            module_name: module_context.module_name.clone(),
            prefix: function_context.prefix,
        };

        construct(
            function_call,
            Some(local_call_info),
            module_context,
            program_context,
        )
    }

    #[derive(Debug, Default, Clone)]
    struct LocalCallInfo {
        module_name: String,
        prefix: String,
    }

    fn construct(
        function_call: &ast::JillFunctionCall,
        local_call_info: Option<LocalCallInfo>,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) -> FallableInstructions {
        let argument_instructions = function_call
            .arguments
            .iter()
            .map(|expr| expression::construct(expr, module_context, program_context))
            .collect::<Result<Vec<_>, _>>()?
            .concat();

        let call = vec![vm::call(
            // TODO!: figure out naming
            function_call.reference.to_fully_qualified_hack_name(
                &local_call_info.clone().unwrap_or_default().module_name,
                local_call_info.unwrap_or_default().prefix,
            ),
            function_call.arguments.len(),
        )];

        Ok([argument_instructions, call].concat())
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::{
        context::module::{FunctionContextArguments, VariableContextArguments},
        vm,
    };

    use super::*;

    #[test]
    fn test_function_call_kind_determination() {
        let mut module_context = ModuleContext::new(String::from("Test"));

        // case 1
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![ast::JillIdentifier(String::from("List"))],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("map")),
        };
        assert_eq!(
            determine_function_call_kind(&function_reference, &module_context),
            FunctionCallKind::ModuleForeignFunction
        );

        // case 2
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("ifElse")),
        };
        assert_eq!(
            determine_function_call_kind(&function_reference, &module_context),
            FunctionCallKind::CompilerInternal(CompilerInternalFunction::IfElse)
        );

        // case 3
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("foo")),
        };
        assert_eq!(
            determine_function_call_kind(&function_reference, &module_context),
            FunctionCallKind::Invalid
        );

        // case 4
        assert!(module_context
            .scope
            .add_variable(
                String::from("foo"),
                VariableContextArguments::new(vm::Segment::Local)
            )
            .is_ok());

        let function_reference = ast::JillFunctionReference {
            modules_path: vec![],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("foo")),
        };
        assert!(matches!(
            determine_function_call_kind(&function_reference, &module_context),
            FunctionCallKind::Variable(_)
        ));
    }

    #[test]
    fn test_module_foreign_function_call() {
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new(String::from("Test"));

        let function_reference = ast::JillFunctionReference {
            modules_path: vec![ast::JillIdentifier(String::from("Foo"))],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("bar")),
        };
        let arguments = vec![
            ast::JillExpression::Literal(ast::JillLiteral::Integer(5)),
            ast::JillExpression::Literal(ast::JillLiteral::Integer(-7)),
        ];
        let function_call = ast::JillFunctionCall {
            reference: function_reference,
            arguments,
        };

        let expected = [
            "push constant 5",
            "push constant 7",
            "neg",
            "call Foo.bar 2",
        ]
        .join("\n");

        assert!(
            construct(&function_call, &mut module_context, &mut program_context).is_ok_and(
                |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
            )
        );
    }

    #[test]
    fn test_module_local_function_call() {
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new(String::from("Test"));

        // top level module-local function
        assert!(module_context
            .scope
            .enter_function(String::from("foo"), FunctionContextArguments::new(2))
            .is_ok());

        module_context.scope.leave_function();

        assert!(module_context
            .scope
            .enter_function(String::from("bar"), FunctionContextArguments::new(2))
            .is_ok());

        let function_reference = ast::JillFunctionReference {
            modules_path: vec![],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("foo")),
        };
        let arguments = vec![
            ast::JillExpression::Literal(ast::JillLiteral::Integer(5)),
            ast::JillExpression::Literal(ast::JillLiteral::Integer(-7)),
        ];
        let function_call = ast::JillFunctionCall {
            reference: function_reference,
            arguments,
        };

        let expected = [
            "push constant 5",
            "push constant 7",
            "neg",
            "call Test.foo 2",
        ]
        .join("\n");

        assert!(
            construct(&function_call, &mut module_context, &mut program_context).is_ok_and(
                |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
            )
        );

        // --------------------------------

        // nested module-local function
        assert!(module_context
            .scope
            .enter_function(String::from("baz"), FunctionContextArguments::new(2))
            .is_ok());

        module_context.scope.leave_function();

        let function_reference = ast::JillFunctionReference {
            modules_path: vec![],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("baz")),
        };
        let arguments = vec![
            ast::JillExpression::Literal(ast::JillLiteral::Integer(5)),
            ast::JillExpression::Literal(ast::JillLiteral::Integer(-7)),
        ];
        let function_call = ast::JillFunctionCall {
            reference: function_reference,
            arguments,
        };

        let expected = [
            "push constant 5",
            "push constant 7",
            "neg",
            "call Test.bar_baz 2",
        ]
        .join("\n");

        assert!(
            construct(&function_call, &mut module_context, &mut program_context).is_ok_and(
                |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
            )
        );
    }

    #[test]
    fn test_variable_call() {
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new(String::from("Test"));

        assert!(module_context
            .scope
            .add_variable(
                "foo".to_string(),
                VariableContextArguments::new(vm::Segment::Local)
            )
            .is_ok());

        let function_reference = ast::JillFunctionReference {
            modules_path: vec![],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("foo")),
        };
        let arguments = vec![
            ast::JillExpression::Literal(ast::JillLiteral::Integer(5)),
            ast::JillExpression::Literal(ast::JillLiteral::Integer(-7)),
        ];
        let function_call = ast::JillFunctionCall {
            reference: function_reference,
            arguments,
        };

        let expected = [
            // create array
            "push constant 2",
            "call Array.new 1",
            "pop temp 1",
            // add `5`
            "push constant 0",
            "push temp 1",
            "add",
            "push constant 5",
            "pop temp 0",
            "pop pointer 1",
            "push temp 0",
            "pop that 0",
            // add `-7`
            "push constant 1",
            "push temp 1",
            "add",
            "push constant 7",
            "neg",
            "pop temp 0",
            "pop pointer 1",
            "push temp 0",
            "pop that 0",
            // call
            "push local 0",
            "push temp 1",
            "call Fn._call 2",
        ]
        .join("\n");

        assert!(
            construct(&function_call, &mut module_context, &mut program_context).is_ok_and(
                |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
            )
        );
    }

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
        ]
        .join("\n");

        assert!(
            construct(&function_call, &mut module_context, &mut program_context).is_ok_and(
                |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
            )
        );
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

        assert!(
            construct(&function_call, &mut module_context, &mut program_context).is_ok_and(
                |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
            )
        );
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

        assert!(
            construct(&function_call, &mut module_context, &mut program_context).is_ok_and(
                |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
            )
        );

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

        assert!(
            construct(&function_call, &mut module_context, &mut program_context).is_ok_and(
                |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
            )
        );
    }
}
