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

use super::{
    compiler_internal_call,
    helpers::{
        self, function::JillFunctionReferenceExtensions, function_override::FunctionOverrideKind,
    },
};

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
                // NOTE: general expressions don't need special call construction
                direct_call::NormalCallConstruction,
                module_context,
                program_context,
            )
        }
        FunctionCallKind::ModuleForeignFunction => {
            direct_call::construct_module_foreign(function_call, module_context, program_context)
        }
        FunctionCallKind::Override(override_kind) => direct_call::construct_override(
            function_call,
            override_kind,
            module_context,
            program_context,
        ),
        FunctionCallKind::Invalid => invalid_function_call(function_call),
    }
}

fn invalid_function_call<T>(function_call: &ast::JillFunctionCall) -> Result<T, Error> {
    Err(Error::InvalidFunctionCall(
        function_call.reference.reconstruct_source_name(),
    ))
}

#[derive(Debug, PartialEq, Eq)]
pub(super) enum FunctionCallKind {
    CompilerInternal(CompilerInternalFunction),
    Variable(VariableContext),
    ModuleLocalFunction(FunctionContext),
    ModuleForeignFunction,
    Override(FunctionOverrideKind),
    Invalid,
}

pub(super) fn determine_function_call_kind(
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
        // check for overrides
        if let Some(override_kind) = helpers::function_override::find_override(function_reference) {
            return FunctionCallKind::Override(override_kind);
        }

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
        .search_function(&function_reference.type_associated_function_name())
    {
        return FunctionCallKind::ModuleLocalFunction(function_context);
    }

    // not fully qualified AND not found in scope => invalid
    FunctionCallKind::Invalid

    // NOTE: no check for "recursive" case - we cannot detect
    // tail-recursion at this point, and we cannot perform
    // the optimization to all recursive calls
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
        let array_elem_temp_segment_index = program_context.temp_segment_index.request();
        let array_temp_segment_index = program_context.temp_segment_index.request();

        let array_instructions_build_config = helpers::array::ArrayBuildConfiguration {
            push_resulting_array: false,
            array_temp_segment_index,
            array_elem_temp_segment_index,
        };
        let arguments_array_instructions = helpers::array::build_array_instructions(
            &function_call.arguments,
            |expr| expression::construct(expr, module_context, program_context),
            array_instructions_build_config,
        )?;

        let function_arity = function_call.arguments.len();

        let call_instructions = vec![
            // closure
            variable_context.push(),
            vec![
                // arity
                vm::push(vm::Segment::Constant, function_arity),
                // arguments
                vm::push(vm::Segment::Temp, array_temp_segment_index),
                vm::call(vm::VMFunctionName::from_literal("Fn._call"), 3),
            ],
        ]
        .concat();

        // array
        program_context.temp_segment_index.release();
        // array elem storage
        program_context.temp_segment_index.release();

        Ok([arguments_array_instructions, call_instructions].concat())
    }
}

pub(super) mod direct_call {
    use crate::codegen::{
        common::{
            expression,
            helpers::{
                self, function::JillFunctionReferenceExtensions,
                function_override::FunctionOverrideKind,
            },
        },
        context::module::FunctionContext,
        error::{FallableAction, FallableInstructions},
        post_compilation::jillstd,
        vm,
    };

    use super::{ast, invalid_function_call, ModuleContext, ProgramContext};

    pub trait CallConstruction {
        fn construct(
            self,
            function_call: &ast::JillFunctionCall,
            local_call_info: Option<LocalCallInfo>,
            has_captures: bool,
        ) -> Vec<vm::VMInstruction>;
    }

    #[derive(Debug, Default, Clone)]
    pub struct LocalCallInfo {
        module_name: String,
        prefix: String,
    }

    #[derive(Debug)]
    pub struct NormalCallConstruction;

    impl CallConstruction for NormalCallConstruction {
        fn construct(
            self,
            function_call: &ast::JillFunctionCall,
            local_call_info: Option<LocalCallInfo>,
            has_captures: bool,
        ) -> Vec<vm::VMInstruction> {
            // increase argument count by one if there are captures (for capture array)
            let argument_count = function_call.arguments.len() + (has_captures as usize);

            vec![vm::call(
                function_call.reference.to_fully_qualified_hack_name(
                    &local_call_info.clone().unwrap_or_default().module_name,
                    local_call_info.unwrap_or_default().prefix,
                ),
                argument_count,
            )]
        }
    }

    // region: Module-foreign

    pub(super) fn construct_module_foreign(
        function_call: &ast::JillFunctionCall,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) -> FallableInstructions {
        // track (potential) `JillStd` function occurences
        note_potential_std_function_occurence(function_call, program_context)?;

        // call construction
        let argument_instructions =
            construct_arguments(function_call, module_context, program_context)?;

        // NOTE: module-foreign call cannot be recursive,
        // so we can hardcode call construction
        let call = NormalCallConstruction.construct(
            function_call,
            None,
            // module-foreign call can only be to a top-level function, and they cannot have captures
            false,
        );

        Ok([argument_instructions, call].concat())
    }

    fn note_potential_std_function_occurence(
        function_call: &ast::JillFunctionCall,
        program_context: &mut ProgramContext,
    ) -> FallableAction {
        let std_function_usage_note_outcome = program_context
            .std_usage_tracker
            .note_usage(&function_call.reference);

        if std_function_usage_note_outcome
            == jillstd::JillStdFunctionUsageNoteOutcome::FunctionNotPresentInModule
        {
            return invalid_function_call(function_call);
        }

        Ok(())
    }

    // endregion

    // region: Module-local

    pub fn construct_module_local(
        function_call: &ast::JillFunctionCall,
        function_context: FunctionContext,
        call_construction: impl CallConstruction,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) -> FallableInstructions {
        let argument_instructions =
            construct_arguments(function_call, module_context, program_context)?;

        // only evaluate (and push) captures array if there are any
        let called_function_has_captures = !function_context.captures.is_empty();
        let captures_instructions = if called_function_has_captures {
            helpers::capture::construct_captures_array(
                &function_context.captures,
                module_context,
                program_context,
            )?
        } else {
            Vec::new()
        };

        let local_call_info = LocalCallInfo {
            module_name: module_context.module_name.clone(),
            prefix: function_context.prefix,
        };

        let call = call_construction.construct(
            function_call,
            Some(local_call_info),
            called_function_has_captures,
        );

        Ok([argument_instructions, captures_instructions, call].concat())
    }

    // endregion

    // region: Override

    pub(super) fn construct_override(
        function_call: &ast::JillFunctionCall,
        override_kind: FunctionOverrideKind,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) -> FallableInstructions {
        // no need to track (potential) `JillStd` function occurences

        let argument_instructions =
            construct_arguments(function_call, module_context, program_context)?;

        let call = construct_override_call(function_call, override_kind);

        Ok([argument_instructions, call].concat())
    }

    fn construct_override_call(
        function_call: &ast::JillFunctionCall,
        override_kind: FunctionOverrideKind,
    ) -> Vec<vm::VMInstruction> {
        let instruction = match override_kind {
            FunctionOverrideKind::VM(vm_command) => vm::command(vm_command),
            FunctionOverrideKind::JackStd(module_name, function_name) => {
                // Jack std has no type-associated functions
                let vm_function_name =
                    vm::VMFunctionName::construct(module_name, "", function_name);

                vm::call(vm_function_name, function_call.arguments.len())
            }
        };

        vec![instruction]
    }

    // endregion

    fn construct_arguments(
        function_call: &ast::JillFunctionCall,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) -> FallableInstructions {
        Ok(function_call
            .arguments
            .iter()
            .map(|expr| expression::construct(expr, module_context, program_context))
            .collect::<Result<Vec<_>, _>>()?
            .concat())
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
    fn test_module_local_with_captures() {
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new(String::from("Test"));

        // fn bar x = _.
        assert!(module_context
            .scope
            .enter_function(String::from("bar"), FunctionContextArguments::new(1))
            .is_ok());

        assert!(module_context
            .scope
            .add_variable(
                String::from("x"),
                VariableContextArguments::new(vm::Segment::Argument)
            )
            .is_ok());

        // fn baz a [x] = _.
        assert!(module_context
            .scope
            .enter_function(
                String::from("baz"),
                FunctionContextArguments::new(1).with_captures(vec![String::from("x")])
            )
            .is_ok());

        module_context.scope.leave_function();

        let function_reference = ast::JillFunctionReference {
            modules_path: vec![],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("baz")),
        };
        let arguments = vec![ast::JillExpression::Literal(ast::JillLiteral::Integer(5))];
        let function_call = ast::JillFunctionCall {
            reference: function_reference,
            arguments,
        };

        let expected = [
            // argument
            "push constant 5",
            // captures
            // create array
            "push constant 1",
            "call Array.new 1",
            "pop temp 1",
            // add `x`
            "push constant 0",
            "push temp 1",
            "add",
            "push argument 0",
            "pop temp 0",
            "pop pointer 1",
            "push temp 0",
            "pop that 0",
            // push to stack
            "push temp 1",
            // call
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
            "push constant 2",
            "push temp 1",
            "call Fn._call 3",
        ]
        .join("\n");

        assert!(
            construct(&function_call, &mut module_context, &mut program_context).is_ok_and(
                |instructions| vm::VMInstructionBlock::from(instructions).compile() == expected
            )
        );
    }
}
