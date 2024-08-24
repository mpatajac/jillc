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
    use crate::codegen::vm::{self, Segment, VMCommand};

    use super::{ast, ModuleContext, ProgramContext};

    pub(super) fn construct(
        variant: ast::JillTypeVariant,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) {
        construct_ctor(&variant, module_context, program_context);

        construct_accessors(&variant, module_context, program_context);
        // TODO: construct_updaters?
    }

    fn construct_ctor(
        variant: &ast::JillTypeVariant,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) {
        // listed fields + tag
        let number_of_fields = variant.fields.len() + 1;

        let variant_tag = module_context.type_info.current_variant;
        let function_name = format!("{}.{}", module_context.module_name, variant.name);

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
                vm::pop(Segment::This, number_of_fields - 1),
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
            let function_name = format!("{}.{}", module_context.module_name, field);

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
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_types_single_variant() {
        // setup
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new(String::from("Bar"));
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
            "function Bar.foo1 0",
            "push argument 0",
            "pop pointer 0",
            "push this 0",
            "return",
        ];

        let get_foo2 = vec![
            "function Bar.foo2 0",
            "push argument 0",
            "pop pointer 0",
            "push this 1",
            "return",
        ];

        let expected = [ctor, get_foo1, get_foo2].concat().join("\n");

        construct(types, &mut module_context, &mut program_context);

        assert_eq!(module_context.output.compile(), expected);
    }
}
