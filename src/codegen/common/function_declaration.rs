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
