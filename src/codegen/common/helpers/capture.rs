use crate::codegen::{
    context::{ModuleContext, ProgramContext},
    error::{Error, FallableInstructions},
};

pub fn construct_captures_array(
    function_captures: &[String],
    module_context: &ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableInstructions {
    let array_instructions_build_config = super::array::ArrayBuildConfiguration {
        push_resulting_array: true,
        // don't need `temp` segment values here, so we can just inline them
        array_elem_temp_segment_index: program_context.temp_segment_index.request(),
        array_temp_segment_index: program_context.temp_segment_index.request(),
    };

    // have to evaluate before releasing `temp` indices
    let array_build_instructions = super::array::build_array_instructions(
        function_captures,
        |capture_name| {
            module_context
                .scope
                .search_variable(capture_name)
                .map_or_else(
                    || Err(Error::CaptureNotInScope(capture_name.to_string())),
                    |capture_variable_context| Ok(capture_variable_context.push()),
                )
        },
        array_instructions_build_config,
    );

    // array
    program_context.temp_segment_index.release();
    // array elem storage
    program_context.temp_segment_index.release();

    array_build_instructions
}
