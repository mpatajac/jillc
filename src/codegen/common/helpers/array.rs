use crate::codegen::{context::module::VariableContext, error::FallableInstructions, vm};

pub fn build_array_instructions<T, F>(
    array_items: &[T],
    mut item_instructions: F,
) -> FallableInstructions
where
    F: FnMut(&T) -> FallableInstructions,
{
    if array_items.is_empty() {
        return Ok(vec![vm::null()]);
    }

    let array = VariableContext {
        segment: vm::Segment::Temp,
        // keep `temp 0` for array usage
        index: 1,
    };

    let array_init_instructions = vec![
        vm::push(vm::Segment::Constant, array_items.len()),
        vm::call(vm::VMFunctionName::from_literal("Array.new"), 1),
        array.pop(),
    ];

    let array_items_instructions = array_items
        .iter()
        .enumerate()
        .map(|(index, item)| {
            Ok([
                vec![
                    vm::push(vm::Segment::Constant, index),
                    array.push(),
                    vm::command(vm::VMCommand::Add),
                ],
                item_instructions(item)?,
                vec![
                    vm::pop(vm::Segment::Temp, 0),
                    vm::pop(vm::Segment::Pointer, 1),
                    vm::push(vm::Segment::Temp, 0),
                    vm::pop(vm::Segment::That, 0),
                ],
            ]
            .concat())
        })
        .collect::<Result<Vec<_>, _>>()?
        .concat();

    Ok([
        array_init_instructions,
        array_items_instructions,
        // add array to stack as function call argument
        vec![array.push()],
    ]
    .concat())
}
