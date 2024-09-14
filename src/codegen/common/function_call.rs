use std::str::FromStr;

use strum::VariantNames;

use crate::{
    codegen::{
        context::{ModuleContext, ProgramContext},
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
        FunctionCallKind::Variable => {
            variable_call::construct(function_call, module_context, program_context)
        }
        FunctionCallKind::Direct => {
            direct_call::construct(function_call, module_context, program_context)
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
    Variable,
    Direct,
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

    // not fully qualified => variable or a module-local function
    // NOTE: this could also be a type-related function,
    // but they are also registered as a module-local function
    if !function_reference.is_fully_qualified() {
        // check "variable" case
        if module_context
            .scope
            .search_variable(function_name)
            .is_some()
        {
            return FunctionCallKind::Variable;
        }

        // check "function" case
        // NOTE: if found, let it fall through; otherwise it is invalid
        // (as it is not a variable nor a module-local function)
        if module_context
            .scope
            // TODO!: figure out naming
            .search_function(&function_reference.function_name.0)
            .is_none()
        {
            return FunctionCallKind::Invalid;
        }

        // NOTE: no check for "recursive" case - we cannot detect
        // tail-recursion at this point, and we cannot perform
        // the optimization to all recursive calls
    }

    // if no "special" kind of call is detected, then it is a
    // regular, direct function call
    FunctionCallKind::Direct
}

mod compiler_internal_call {
    use crate::{
        codegen::{
            common::helpers::function::JillFunctionReferenceExtensions,
            error::{Error, FallableInstructions},
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
        todo!()
    }
}

mod variable_call {
    use crate::codegen::error::FallableInstructions;

    use super::{ast, ModuleContext, ProgramContext};

    pub(super) fn construct(
        function_call: &ast::JillFunctionCall,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) -> FallableInstructions {
        todo!()
    }
}

mod direct_call {
    use crate::codegen::{
        common::{expression, helpers::function::JillFunctionReferenceExtensions},
        error::FallableInstructions,
        vm,
    };

    use super::{ast, ModuleContext, ProgramContext};

    pub(super) fn construct(
        function_call: &ast::JillFunctionCall,
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
            function_call
                .reference
                .to_fully_qualified_hack_name(&String::new(), String::new()),
            function_call.arguments.len(),
        )];

        Ok([argument_instructions, call].concat())
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::{context::module::VariableContextArguments, vm};

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
            FunctionCallKind::Direct
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
        assert_eq!(
            determine_function_call_kind(&function_reference, &module_context),
            FunctionCallKind::Variable
        );
    }

    #[test]
    fn test_direct_call() {
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
}
