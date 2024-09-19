use crate::common::ast;

use super::{
    context::{self, ModuleContext, ProgramContext},
    error::FallableAction,
    vm::{self, Segment},
};

pub fn construct(
    types: Vec<ast::JillType>,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableAction {
    for jill_type in types {
        // reset stored type info
        module_context.type_info = context::module::TypeInfo::new();

        construct_type(&jill_type, module_context, program_context)?;
    }

    Ok(())
}

fn construct_type(
    jill_type: &ast::JillType,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) -> FallableAction {
    module_context.type_info.current_variant = 0;

    construct_tag(jill_type, module_context, program_context);

    for variant in &jill_type.variants {
        variant::construct(variant, module_context, program_context)?;

        module_context.type_info.current_variant += 1;
    }

    Ok(())
}

fn construct_tag(
    jill_type: &ast::JillType,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) {
    // "{module_context.module_name}.{type_name}_tag"
    let function_name =
        vm::VMFunctionName::construct(&module_context.module_name, &jill_type.name.0, "_tag");

    // NOTE: this function is meant for internal usage,
    // so we DO NOT add it to module scope.

    let output_block = vec![
        vm::function(function_name, 0),
        vm::push(Segment::Argument, 0),
        vm::pop(Segment::Pointer, 0),
        // `tag` is always at `this 0`
        vm::push(Segment::This, 0),
        vm::vm_return(),
    ];

    module_context.output.add_block(output_block.into());
}

mod variant {
    use crate::codegen::{
        context::module::FunctionContextArguments,
        error::FallableAction,
        vm::{self, Segment},
    };

    use super::{ast, ModuleContext, ProgramContext};

    use heck::ToTitleCase;

    pub(super) fn construct(
        variant: &ast::JillTypeVariant,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) -> FallableAction {
        construct_ctor(variant, module_context, program_context)?;

        // TODO?: only generate if requested (`with get|update|eq|...`)
        construct_accessors(variant, module_context, program_context)?;
        construct_updaters(variant, module_context, program_context)?;

        Ok(())
    }

    fn construct_ctor(
        variant: &ast::JillTypeVariant,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) -> FallableAction {
        let number_of_fields = number_of_fields(variant);

        let variant_tag = module_context.type_info.current_variant;
        let function_name = ctor_name(variant, module_context);

        add_function_to_scope(variant, function_name.clone(), module_context)?;

        let field_assignment = |i: usize| {
            vec![
                vm::push(Segment::Argument, i),
                // offset by one because `tag` is at `this 0`
                vm::pop(Segment::This, i + 1),
            ]
        };

        let assignments = (0..variant.fields.len())
            .flat_map(field_assignment)
            .collect::<Vec<_>>();

        let output_block = [
            vec![
                vm::function(function_name, 0),
                vm::push(Segment::Constant, number_of_fields),
                vm::call(vm::VMFunctionName::from_literal("Memory.alloc"), 1),
                vm::pop(Segment::Pointer, 0),
            ],
            assignments,
            vec![
                vm::push(Segment::Constant, variant_tag),
                vm::pop(Segment::This, 0),
                vm::push(Segment::Pointer, 0),
                vm::vm_return(),
            ],
        ]
        .concat();

        module_context.output.add_block(output_block.into());

        Ok(())
    }

    fn construct_accessors(
        variant: &ast::JillTypeVariant,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) -> FallableAction {
        for (i, field) in variant.fields.iter().enumerate() {
            // "{module_context.module_name}.{variant.name}_{field}"
            let function_name = vm::VMFunctionName::construct(
                &module_context.module_name,
                &format!("{}_", variant.name),
                &field.0,
            );

            add_function_to_scope(variant, function_name.clone(), module_context)?;

            let output_block = vec![
                vm::function(vm::VMFunctionName::from_literal(&function_name), 0),
                vm::push(Segment::Argument, 0),
                vm::pop(Segment::Pointer, 0),
                // offset by one because `tag` is at `this 0`
                vm::push(Segment::This, i + 1),
                vm::vm_return(),
            ];

            module_context.output.add_block(output_block.into());
        }

        Ok(())
    }

    fn construct_updaters(
        variant: &ast::JillTypeVariant,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) -> FallableAction {
        let ctor_name = ctor_name(variant, module_context);
        let ctor_argument_count = variant.fields.len();
        let ctor_call = vm::call(ctor_name, ctor_argument_count);

        for (variant_index, field) in variant.fields.iter().enumerate() {
            // let function_name = format!(
            //     "{}.{}_update{}",
            //     module_context.module_name,
            //     variant.name,
            //     field.0.to_title_case()
            // );
            let function_name = vm::VMFunctionName::construct(
                &module_context.module_name,
                &format!("{}_", variant.name),
                &format!("update{}", field.0.to_title_case()),
            );

            add_function_to_scope(variant, function_name.clone(), module_context)?;

            let ctor_arg_mapping = |i| {
                if i == variant_index {
                    // field that needs to be updated => push passed value
                    vm::push(Segment::Argument, 1)
                } else {
                    // other fields => push existing field value
                    // offset by one because `tag` is at `this 0`
                    vm::push(Segment::This, i + 1)
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
                vec![ctor_call.clone(), vm::vm_return()],
            ]
            .concat();

            module_context.output.add_block(output_block.into());
        }

        Ok(())
    }

    /// Type-related functions have no nested functions,
    /// so we can instantly close their frame.
    fn add_function_to_scope(
        variant: &ast::JillTypeVariant,
        function_name: vm::VMFunctionName,
        module_context: &mut ModuleContext,
    ) -> FallableAction {
        module_context.scope.enter_function(
            function_name.to_string(),
            FunctionContextArguments::new(number_of_arguments(variant)),
        )?;
        module_context.scope.leave_function();

        Ok(())
    }

    fn ctor_name(
        variant: &ast::JillTypeVariant,
        module_context: &ModuleContext,
    ) -> vm::VMFunctionName {
        // "{module_context.module_name}.{variant.name}"
        vm::VMFunctionName::construct(&module_context.module_name, "", &variant.name.0)
    }

    fn number_of_arguments(variant: &ast::JillTypeVariant) -> usize {
        variant.fields.len()
    }

    fn number_of_fields(variant: &ast::JillTypeVariant) -> usize {
        // listed fields + tag
        number_of_arguments(variant) + 1
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

        let tag = vec![
            "function Bar.Bar_tag 0",
            "push argument 0",
            "pop pointer 0",
            "push this 0",
            "return",
        ];

        let ctor = vec![
            "function Bar.Bar 0",
            "push constant 3",
            "call Memory.alloc 1",
            "pop pointer 0",
            "push argument 0",
            "pop this 1",
            "push argument 1",
            "pop this 2",
            "push constant 0",
            "pop this 0",
            "push pointer 0",
            "return",
        ];

        let get_foo1 = vec![
            "function Bar.Bar_foo1 0",
            "push argument 0",
            "pop pointer 0",
            "push this 1",
            "return",
        ];

        let get_foo2 = vec![
            "function Bar.Bar_foo2 0",
            "push argument 0",
            "pop pointer 0",
            "push this 2",
            "return",
        ];

        let update_foo1 = vec![
            "function Bar.Bar_updateFoo1 0",
            "push argument 0",
            "pop pointer 0",
            "push argument 1",
            "push this 2",
            "call Bar.Bar 2",
            "return",
        ];

        let update_foo2 = vec![
            "function Bar.Bar_updateFoo2 0",
            "push argument 0",
            "pop pointer 0",
            "push this 1",
            "push argument 1",
            "call Bar.Bar 2",
            "return",
        ];

        let expected = [tag, ctor, get_foo1, get_foo2, update_foo1, update_foo2]
            .concat()
            .join("\n");

        assert!(construct(types, &mut module_context, &mut program_context).is_ok());

        // compiled instructions match expected ones
        assert_eq!(module_context.output.compile(), expected);

        // function added to scope
        assert!(module_context
            .scope
            // TODO!: figure out naming
            .search_function(&String::from("Bar.Bar_updateFoo1"))
            .is_some());

        // `tag` function NOT in scope
        assert!(module_context
            .scope
            .search_function(&String::from("Bar._tag"))
            .is_none());
    }

    #[test]
    fn test_types_option() {
        // setup
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new(String::from("Option"));

        // type Option = None, Some(value).
        let types = vec![ast::JillType {
            name: ast::JillIdentifier(String::from("Option")),
            variants: vec![
                ast::JillTypeVariant {
                    name: ast::JillIdentifier(String::from("None")),
                    fields: vec![],
                },
                ast::JillTypeVariant {
                    name: ast::JillIdentifier(String::from("Some")),
                    fields: vec![ast::JillIdentifier(String::from("value"))],
                },
            ],
        }];

        let tag = vec![
            "function Option.Option_tag 0",
            "push argument 0",
            "pop pointer 0",
            "push this 0",
            "return",
        ];

        let none_ctor = vec![
            "function Option.None 0",
            "push constant 1",
            "call Memory.alloc 1",
            "pop pointer 0",
            "push constant 0",
            "pop this 0",
            "push pointer 0",
            "return",
        ];

        let some_ctor = vec![
            "function Option.Some 0",
            "push constant 2",
            "call Memory.alloc 1",
            "pop pointer 0",
            "push argument 0",
            "pop this 1",
            "push constant 1",
            "pop this 0",
            "push pointer 0",
            "return",
        ];

        let some_get_value = vec![
            "function Option.Some_value 0",
            "push argument 0",
            "pop pointer 0",
            "push this 1",
            "return",
        ];

        let some_update_value = vec![
            "function Option.Some_updateValue 0",
            "push argument 0",
            "pop pointer 0",
            "push argument 1",
            "call Option.Some 1",
            "return",
        ];

        let expected = [tag, none_ctor, some_ctor, some_get_value, some_update_value]
            .concat()
            .join("\n");

        assert!(construct(types, &mut module_context, &mut program_context).is_ok());

        // compiled instructions match expected ones
        assert_eq!(module_context.output.compile(), expected);

        // function added to scope
        assert!(module_context
            .scope
            .search_function(&String::from("Option.None"))
            .is_some());

        assert!(module_context
            .scope
            .search_function(&String::from("Option.Some_updateValue"))
            .is_some());

        // `tag` function NOT in scope
        assert!(module_context
            .scope
            .search_function(&String::from("Bar.tag"))
            .is_none());

        assert!(module_context
            .scope
            .search_function(&String::from("Bar._tag"))
            .is_none());
    }

    #[allow(clippy::too_many_lines)]
    #[test]
    fn test_multiple_types() {
        // setup
        let mut program_context = ProgramContext::new();
        let mut module_context = ModuleContext::new(String::from("Test"));

        // type Foo = Foo(value).
        // type Bar = Baz(x).
        let types = vec![
            ast::JillType {
                name: ast::JillIdentifier(String::from("Foo")),
                variants: vec![ast::JillTypeVariant {
                    name: ast::JillIdentifier(String::from("Foo")),
                    fields: vec![ast::JillIdentifier(String::from("value"))],
                }],
            },
            ast::JillType {
                name: ast::JillIdentifier(String::from("Bar")),
                variants: vec![ast::JillTypeVariant {
                    name: ast::JillIdentifier(String::from("Baz")),
                    fields: vec![ast::JillIdentifier(String::from("x"))],
                }],
            },
        ];

        let foo_tag = vec![
            "function Test.Foo_tag 0",
            "push argument 0",
            "pop pointer 0",
            "push this 0",
            "return",
        ];

        let foo_ctor = vec![
            "function Test.Foo 0",
            "push constant 2",
            "call Memory.alloc 1",
            "pop pointer 0",
            "push argument 0",
            "pop this 1",
            "push constant 0",
            "pop this 0",
            "push pointer 0",
            "return",
        ];

        let foo_get_value = vec![
            "function Test.Foo_value 0",
            "push argument 0",
            "pop pointer 0",
            "push this 1",
            "return",
        ];

        let foo_update_value = vec![
            "function Test.Foo_updateValue 0",
            "push argument 0",
            "pop pointer 0",
            "push argument 1",
            "call Test.Foo 1",
            "return",
        ];

        let bar_tag = vec![
            "function Test.Bar_tag 0",
            "push argument 0",
            "pop pointer 0",
            "push this 0",
            "return",
        ];

        let baz_ctor = vec![
            "function Test.Baz 0",
            "push constant 2",
            "call Memory.alloc 1",
            "pop pointer 0",
            "push argument 0",
            "pop this 1",
            "push constant 0",
            "pop this 0",
            "push pointer 0",
            "return",
        ];

        let baz_get_x = vec![
            "function Test.Baz_x 0",
            "push argument 0",
            "pop pointer 0",
            "push this 1",
            "return",
        ];

        let baz_update_x = vec![
            "function Test.Baz_updateX 0",
            "push argument 0",
            "pop pointer 0",
            "push argument 1",
            "call Test.Baz 1",
            "return",
        ];

        let expected = [
            foo_tag,
            foo_ctor,
            foo_get_value,
            foo_update_value,
            bar_tag,
            baz_ctor,
            baz_get_x,
            baz_update_x,
        ]
        .concat()
        .join("\n");

        assert!(construct(types, &mut module_context, &mut program_context).is_ok());

        // compiled instructions match expected ones
        assert_eq!(module_context.output.compile(), expected);
    }
}
