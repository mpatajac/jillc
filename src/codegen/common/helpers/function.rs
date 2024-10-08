use crate::{codegen::vm, common::ast};

pub trait JillFunctionReferenceExtensions {
    fn is_fully_qualified(&self) -> bool;
    fn reconstruct_source_name(&self) -> String;
    fn to_fully_qualified_hack_name(
        &self,
        module_name: &String,
        function_prefix: String,
    ) -> vm::VMFunctionName;
    fn from_function_definition(function_definition: &ast::JillFunction) -> Self;
    fn type_associated_function_name(&self) -> String;
}

impl JillFunctionReferenceExtensions for ast::JillFunctionReference {
    fn is_fully_qualified(&self) -> bool {
        !self.modules_path.is_empty()
    }

    fn reconstruct_source_name(&self) -> String {
        let module_path = self
            .modules_path
            .iter()
            .fold(String::new(), |s, module| s + &module.0 + "::");

        let type_path = self
            .associated_type
            .clone()
            .map(|t| format!("{t}:"))
            .unwrap_or_default();

        let function_name = &self.function_name.0;

        format!("{module_path}{type_path}{function_name}")
    }

    // TODO: try to do this more elegantly (and give a nicer name)
    fn to_fully_qualified_hack_name(
        &self,
        local_module_name: &String,
        local_function_prefix: String,
    ) -> vm::VMFunctionName {
        // format: `Module_Path_Elements.[OptionalType_][optionalFunction_prefixes_]functionName`
        let module_path = if self.is_fully_qualified() {
            self.modules_path
                .iter()
                .map(|module_identifier| module_identifier.0.clone())
                .collect::<Vec<_>>()
                .join("_")
        } else {
            // if it is local, use the name of the module it is defined in
            local_module_name.to_string()
        };

        let type_name = self
            .associated_type
            .clone()
            .map(|t| t.0 + "_")
            .unwrap_or_default();

        let function_name = if self.is_fully_qualified() {
            self.function_name.0.clone()
        } else {
            // if it is local, prepend the function prefix
            local_function_prefix + &self.function_name.0
        };

        vm::VMFunctionName::construct(&module_path, &type_name, &function_name)
    }

    fn from_function_definition(function_definition: &ast::JillFunction) -> Self {
        Self {
            modules_path: vec![],
            associated_type: None,
            function_name: function_definition.name.clone(),
        }
    }

    fn type_associated_function_name(&self) -> String {
        let associated_type_prefix = self
            .associated_type
            .clone()
            .map(|t| format!("{t}_"))
            .unwrap_or_default();

        associated_type_prefix + &self.function_name.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function_reference_reconstruction() {
        // case 1
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("test")),
        };

        assert_eq!(&function_reference.reconstruct_source_name(), "test");

        // case 2
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![ast::JillIdentifier(String::from("List"))],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("map")),
        };

        assert_eq!(&function_reference.reconstruct_source_name(), "List::map");

        // case 3
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![
                ast::JillIdentifier(String::from("Utils")),
                ast::JillIdentifier(String::from("Primitives")),
                ast::JillIdentifier(String::from("Option")),
            ],
            associated_type: Some(ast::JillIdentifier(String::from("Some"))),
            function_name: ast::JillIdentifier(String::from("value")),
        };

        assert_eq!(
            &function_reference.reconstruct_source_name(),
            "Utils::Primitives::Option::Some:value"
        );
    }

    #[test]
    fn test_fully_qualified_name_generation() {
        // case 1
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("test")),
        };
        let module_name = &String::from("Test");
        let function_prefix = String::new();

        assert_eq!(
            function_reference.to_fully_qualified_hack_name(module_name, function_prefix),
            vm::VMFunctionName::from_literal("Test.test")
        );

        // case 2
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![ast::JillIdentifier(String::from("List"))],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("map")),
        };
        let module_name = &String::new();
        let function_prefix = String::new();

        assert_eq!(
            function_reference.to_fully_qualified_hack_name(module_name, function_prefix),
            vm::VMFunctionName::from_literal("List.map")
        );

        // case 3
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![
                ast::JillIdentifier(String::from("Utils")),
                ast::JillIdentifier(String::from("Primitives")),
                ast::JillIdentifier(String::from("Option")),
            ],
            associated_type: Some(ast::JillIdentifier(String::from("Some"))),
            function_name: ast::JillIdentifier(String::from("value")),
        };
        let module_name = &String::new();
        let function_prefix = String::new();

        assert_eq!(
            function_reference.to_fully_qualified_hack_name(module_name, function_prefix),
            vm::VMFunctionName::from_literal("Utils_Primitives_Option.Some_value")
        );

        // case 4
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("biz")),
        };
        let module_name = &String::from("Foo");
        let function_prefix = String::from("bar_baz_");

        assert_eq!(
            function_reference.to_fully_qualified_hack_name(module_name, function_prefix),
            vm::VMFunctionName::from_literal("Foo.bar_baz_biz")
        );
    }

    #[test]
    fn test_type_associated_function_name() {
        // case 1 (no associated type)
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("test")),
        };

        assert_eq!(
            function_reference.type_associated_function_name(),
            String::from("test")
        );

        // case 2 (associated type)
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![],
            associated_type: Some(ast::JillIdentifier(String::from("Type"))),
            function_name: ast::JillIdentifier(String::from("test")),
        };

        assert_eq!(
            function_reference.type_associated_function_name(),
            String::from("Type_test")
        );

        // case 3 (module present - doesn't change a thing)
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![ast::JillIdentifier(String::from("Mod"))],
            associated_type: Some(ast::JillIdentifier(String::from("Type"))),
            function_name: ast::JillIdentifier(String::from("test")),
        };

        assert_eq!(
            function_reference.type_associated_function_name(),
            String::from("Type_test")
        );
    }
}
