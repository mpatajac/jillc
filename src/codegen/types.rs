use crate::common::ast;

use super::context::{self, ModuleContext, ProgramContext};

pub fn construct(
    types: Vec<ast::JillType>,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) {
    for jill_type in types {
        // re-set stored type info
        module_context.type_info = context::module::TypeInfo::new();

        construct_type(jill_type, module_context, program_context);
    }
}

fn construct_type(
    jill_type: ast::JillType,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) {
    module_context.type_info.is_multivariant = jill_type.variants.len() > 1;
    module_context.type_info.current_variant = 0;

    if module_context.type_info.is_multivariant {
        // TODO: do we actually use a dedicated `match` function
        // or is everything handled in the "call"?
        construct_match(&jill_type, module_context, program_context);
    }

    for variant in jill_type.variants {
        variant::construct(variant, module_context, program_context);

        module_context.type_info.current_variant += 1;
    }
}

fn construct_match(
    jill_type: &ast::JillType,
    module_context: &mut ModuleContext,
    program_context: &mut ProgramContext,
) {
    todo!()
}

mod variant {
    use super::{ast, ModuleContext, ProgramContext};

    pub(super) fn construct(
        variant: ast::JillTypeVariant,
        module_context: &mut ModuleContext,
        program_context: &mut ProgramContext,
    ) {
        construct_ctor(&variant, module_context, program_context);

        // TODO: construct_accessors
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

        let assignment = |idx: usize| {
            // TODO!: create VM "primitives" (push/pop/call..., segments etc.),
            // each with appropriate String mapping
            format!("push argument {idx}\npop this {idx}")
        };

        let assignments = (0..variant.fields.len())
            .map(assignment)
            .collect::<Vec<_>>()
            .join("\n");

        let output = format!(
            "function {function_name} 0
push constant {number_of_fields}
call Memory.alloc 1
pop pointer 0
{assignments}
push constant {variant_tag}
pop this {variant_tag_idx}
push pointer 0
return",
            variant_tag_idx = number_of_fields - 1
        );

        module_context.output_blocks.push(output);
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_types_single_ctor() {
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

        let expected = "function Bar.Bar 0
push constant 3
call Memory.alloc 1
pop pointer 0
push argument 0
pop this 0
push argument 1
pop this 1
push constant 0
pop this 2
push pointer 0
return"
            .trim();

        construct(types, &mut module_context, &mut program_context);

        assert_eq!(module_context.output_blocks, vec![expected]);
    }
}
