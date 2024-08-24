use crate::common::ast;

use super::context::{self, ModuleContext, ProgramContext};

pub fn construct(
    types: Vec<ast::JillType>,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) {
    for jill_type in types {
        // reset stored type info
        module_context.type_info = context::module::TypeInfo::new();

        construct_type(jill_type, module_context, program_context);
    }
}

fn construct_type(
    jill_type: ast::JillType,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) {
    module_context.type_info.current_variant = 0;

    for variant in jill_type.variants {
        variant::construct(variant, module_context, program_context);

        module_context.type_info.current_variant += 1;
    }
}

mod variant {
    use crate::codegen::vm::{self, Segment};

    use super::{ast, ModuleContext, ProgramContext};

    use heck::ToTitleCase;

    pub(super) fn construct(
        variant: ast::JillTypeVariant,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) {
        construct_ctor(&variant, module_context, program_context);

        // TODO?: only generate if requested (`with get|update|eq|...`)
        construct_accessors(&variant, module_context, program_context);
        construct_updaters(&variant, module_context, program_context);
    }

    fn construct_ctor(
        variant: &ast::JillTypeVariant,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) {
        let number_of_fields = number_of_fields(variant);

        let variant_tag = module_context.type_info.current_variant;
        let function_name = ctor_name(variant, module_context);

        let tag_index = tag_index(variant);

        let field_assignment =
            |i: usize| vec![vm::push(Segment::Argument, i), vm::pop(Segment::This, i)];

        let assignments = (0..variant.fields.len())
            .flat_map(field_assignment)
            .collect::<Vec<_>>();

        let output_block = [
            vec![
                vm::function(function_name, 0),
                vm::push(Segment::Constant, number_of_fields),
                vm::call("Memory.alloc", 1),
                vm::pop(Segment::Pointer, 0),
            ],
            assignments,
            vec![
                vm::push(Segment::Constant, variant_tag),
                vm::pop(Segment::This, tag_index),
                vm::push(Segment::Pointer, 0),
                vm::vm_return(),
            ],
        ]
        .concat();

        module_context.output.add_block(output_block.into());
    }

    fn construct_accessors(
        variant: &ast::JillTypeVariant,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) {
        for (i, field) in variant.fields.iter().enumerate() {
            let function_name =
                format!("{}.{}_{}", module_context.module_name, variant.name, field);

            let output_block = vec![
                vm::function(function_name, 0),
                vm::push(Segment::Argument, 0),
                vm::pop(Segment::Pointer, 0),
                vm::push(Segment::This, i),
                vm::vm_return(),
            ];

            module_context.output.add_block(output_block.into());
        }
    }

    fn construct_updaters(
        variant: &ast::JillTypeVariant,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) {
        let ctor_name = ctor_name(variant, module_context);
        let ctor_argument_count = variant.fields.len();

        for (variant_index, field) in variant.fields.iter().enumerate() {
            let function_name = format!(
                "{}.{}_update{}",
                module_context.module_name,
                variant.name,
                field.0.to_title_case()
            );

            let ctor_arg_mapping = |i| {
                if i == variant_index {
                    // field that needs to be updated => push passed value
                    vm::push(Segment::Argument, 1)
                } else {
                    // other fields => push existing field value
                    vm::push(Segment::This, i)
                }
            };

            let ctor_args = (0..variant.fields.len())
                .map(ctor_arg_mapping)
                .collect::<Vec<_>>();

            let output_block = [
                vec![
                    vm::function(function_name, 0),
                    vm::push(Segment::Argument, 0),
                    vm::pop(Segment::Pointer, 0),
                ],
                ctor_args,
                vec![vm::call(&ctor_name, ctor_argument_count), vm::vm_return()],
            ]
            .concat();

            module_context.output.add_block(output_block.into());
        }
    }

    fn ctor_name(variant: &ast::JillTypeVariant, module_context: &ModuleContext) -> String {
        format!("{}.{}", module_context.module_name, variant.name)
    }

    fn number_of_fields(variant: &ast::JillTypeVariant) -> usize {
        // listed fields + tag
        variant.fields.len() + 1
    }

    fn tag_index(variant: &ast::JillTypeVariant) -> usize {
        // last item
        number_of_fields(variant) - 1
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_types_single_variant() {
        // setup
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new(String::from("Bar"));

        // type Bar = Bar(foo1, foo2).
        let types = vec![ast::JillType {
            name: ast::JillIdentifier(String::from("Bar")),
            variants: vec![ast::JillTypeVariant {
                name: ast::JillIdentifier(String::from("Bar")),
                fields: vec![
                    ast::JillIdentifier(String::from("foo1")),
                    ast::JillIdentifier(String::from("foo2")),
                ],
            }],
        }];

        let ctor = vec![
            "function Bar.Bar 0",
            "push constant 3",
            "call Memory.alloc 1",
            "pop pointer 0",
            "push argument 0",
            "pop this 0",
            "push argument 1",
            "pop this 1",
            "push constant 0",
            "pop this 2",
            "push pointer 0",
            "return",
        ];

        let get_foo1 = vec![
            "function Bar.Bar_foo1 0",
            "push argument 0",
            "pop pointer 0",
            "push this 0",
            "return",
        ];

        let get_foo2 = vec![
            "function Bar.Bar_foo2 0",
            "push argument 0",
            "pop pointer 0",
            "push this 1",
            "return",
        ];

        let update_foo1 = vec![
            "function Bar.Bar_updateFoo1 0",
            "push argument 0",
            "pop pointer 0",
            "push argument 1",
            "push this 1",
            "call Bar.Bar 2",
            "return",
        ];

        let update_foo2 = vec![
            "function Bar.Bar_updateFoo2 0",
            "push argument 0",
            "pop pointer 0",
            "push this 0",
            "push argument 1",
            "call Bar.Bar 2",
            "return",
        ];

        let expected = [ctor, get_foo1, get_foo2, update_foo1, update_foo2]
            .concat()
            .join("\n");

        construct(types, &mut module_context, &mut program_context);

        assert_eq!(module_context.output.compile(), expected);
    }
}
