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

fn invalid_function_call(function_call: &ast::JillFunctionCall) -> FallableInstructions {
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

        let call_instructions = vec![
            variable_context.push(),
            vm::push(vm::Segment::Temp, array_temp_segment_index),
            vm::call(vm::VMFunctionName::from_literal("Fn._call"), 2),
        ];

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
                function::JillFunctionReferenceExtensions, function_override::FunctionOverrideKind,
            },
        },
        context::module::FunctionContext,
        error::FallableInstructions,
        jillstd, vm,
    };

    use super::{ast, invalid_function_call, ModuleContext, ProgramContext};

    pub trait CallConstruction {
        fn construct(
            self,
            function_call: &ast::JillFunctionCall,
            local_call_info: Option<LocalCallInfo>,
        ) -> Vec<vm::VMInstruction>;
    }

    #[derive(Debug)]
    pub struct NormalCallConstruction;

    impl CallConstruction for NormalCallConstruction {
        fn construct(
            self,
            function_call: &ast::JillFunctionCall,
            local_call_info: Option<LocalCallInfo>,
        ) -> Vec<vm::VMInstruction> {
            vec![vm::call(
                function_call.reference.to_fully_qualified_hack_name(
                    &local_call_info.clone().unwrap_or_default().module_name,
                    local_call_info.unwrap_or_default().prefix,
                ),
                function_call.arguments.len(),
            )]
        }
    }

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

        construct(
            function_call,
            None,
            // NOTE: module-foreign call cannot be recursive,
            // so we can hardcode call construction
            NormalCallConstruction,
            module_context,
            program_context,
        )
    }

    pub fn construct_module_local(
        function_call: &ast::JillFunctionCall,
        function_context: FunctionContext,
        call_construction: impl CallConstruction,
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
            call_construction,
            module_context,
            program_context,
        )
    }

    #[derive(Debug, Default, Clone)]
    pub struct LocalCallInfo {
        module_name: String,
        prefix: String,
    }

    fn construct(
        function_call: &ast::JillFunctionCall,
        local_call_info: Option<LocalCallInfo>,
        call_construction: impl CallConstruction,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) -> FallableInstructions {
        let argument_instructions = function_call
            .arguments
            .iter()
            .map(|expr| expression::construct(expr, module_context, program_context))
            .collect::<Result<Vec<_>, _>>()?
            .concat();

        let call = call_construction.construct(function_call, local_call_info);

        Ok([argument_instructions, call].concat())
    }

    #[derive(Debug)]
    struct OverrideCallConstruction(FunctionOverrideKind);

    impl CallConstruction for OverrideCallConstruction {
        fn construct(
            self,
            function_call: &ast::JillFunctionCall,
            _: Option<LocalCallInfo>,
        ) -> Vec<vm::VMInstruction> {
            let instruction = match self.0 {
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
    }

    pub(super) fn construct_override(
        function_call: &ast::JillFunctionCall,
        override_kind: FunctionOverrideKind,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) -> FallableInstructions {
        // no need to track (potential) `JillStd` function occurences

        construct(
            function_call,
            None,
            OverrideCallConstruction(override_kind),
            module_context,
            program_context,
        )
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
}
