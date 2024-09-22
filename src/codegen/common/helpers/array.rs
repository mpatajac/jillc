use crate::codegen::{context::module::VariableContext, error::FallableInstructions, vm};

pub struct ArrayBuildConfiguration {
    pub push_resulting_array: bool,
    pub array_temp_segment_index: usize,
    pub array_elem_temp_segment_index: usize,
}

pub fn build_array_instructions<T, F>(
    array_items: &[T],
    mut item_instructions: F,
    build_configuration: ArrayBuildConfiguration,
) -> FallableInstructions
where
    F: FnMut(&T) -> FallableInstructions,
{
    if array_items.is_empty() {
        return Ok(vec![vm::null()]);
    }

    let array = VariableContext {
        segment: vm::Segment::Temp,
        // NOTE: have to use different `temp` segment index here
        // than in array construction because we store array
        // pointer in `temp` instead of `local`
        index: build_configuration.array_temp_segment_index,
    };

    let array_init_instructions = vec![
        vm::push(vm::Segment::Constant, array_items.len()),
        vm::call(vm::VMFunctionName::from_literal("Array.new"), 1),
        array.pop(),
    ];

    let array_elem_temp_storage = VariableContext {
        segment: vm::Segment::Temp,
        index: build_configuration.array_elem_temp_segment_index,
    };
    let array_items_instructions = array_items
        .iter()
        .enumerate()
        .map(|(index, item)| {
            Ok([
                vec![vm::push(vm::Segment::Constant, index)],
                array.push(),
                vec![vm::command(vm::VMCommand::Add)],
                item_instructions(item)?,
                vec![
                    array_elem_temp_storage.pop(),
                    vm::pop(vm::Segment::Pointer, 1),
                ],
                array_elem_temp_storage.push(),
                vec![vm::pop(vm::Segment::That, 0)],
            ]
            .concat())
        })
        .collect::<Result<Vec<_>, _>>()?
        .concat();

    let mut instructions = vec![array_init_instructions, array_items_instructions];

    if build_configuration.push_resulting_array {
        instructions.push(array.push());
    }

    Ok(instructions.concat())
}
