use crate::{
    codegen::{
        context::{ModuleContext, ProgramContext},
        error::FallableInstructions,
    },
    common::ast,
};

pub fn construct(
    function_call: &ast::JillFunctionCall,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableInstructions {
    match determine_function_call_kind(function_call) {
        FunctionCallKind::CompilerInternal => {
            compiler_internal_call::construct(function_call, module_context, program_context)
        }
        FunctionCallKind::Variable => {
            variable_call::construct(function_call, module_context, program_context)
        }
        FunctionCallKind::Recursive => {
            recursive_call::construct(function_call, module_context, program_context)
        }
        FunctionCallKind::Direct => {
            direct_call::construct(function_call, module_context, program_context)
        }
    }
}

#[derive(Debug)]
enum FunctionCallKind {
    CompilerInternal,
    Variable,
    Recursive,
    Direct,
}

fn determine_function_call_kind(function_call: &ast::JillFunctionCall) -> FunctionCallKind {
    todo!()
}

mod compiler_internal_call {
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

mod recursive_call {
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
