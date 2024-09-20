use std::collections::HashMap;

use strum::VariantNames;

use crate::common::ast;

use super::{common::helpers::function::JillFunctionReferenceExtensions, vm};

// region: JillStd declaration

/// List of Jill-specific standard modules.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, strum::EnumString, strum::IntoStaticStr)]
#[strum(serialize_all = "PascalCase")]
enum JillStdModule {
    Math,
    Bool,
    List,
    Random,
    Fn,
}

/// List of functions available inside Jill-specific `Math` standard module.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, strum::EnumString, strum::VariantNames)]
#[strum(serialize_all = "camelCase")]
enum JillStdMath {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mult,
    /// `/`
    Div,
    /// `%`
    Mod,
    /// `++`
    Inc,
    /// `--`
    Dec,
}

/// List of functions available inside Jill-specific `Bool` standard module.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, strum::EnumString, strum::VariantNames)]
#[strum(serialize_all = "camelCase")]
enum JillStdBool {
    /// `==`
    Eq,
    /// `!=`
    Ne,
    /// `&&`
    And,
    /// `||`
    Or,
    /// `!`
    Not,
    /// `<`
    Lt,
    /// `>`
    Gt,
    /// `<=`
    Le,
    /// `>=`
    Ge,
}

/// List of functions available inside Jill-specific `List` standard module.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, strum::EnumString, strum::VariantNames)]
#[strum(serialize_all = "camelCase")]
enum JillStdList {
    // type-associated functions
    #[strum(serialize = "Empty")]
    Empty,
    #[strum(serialize = "List")]
    List,
    #[strum(serialize = "List_head")]
    Head,
    #[strum(serialize = "List_tail")]
    Tail,

    // module functions
    Map,
    Filter,
    Fold,
    Repeat,
    Length,
    Zip,
    All,
    Any,
    IsEmpty,
    Concat,
    Range,
}

/// List of functions available inside Jill-specific `Fn` standard module.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, strum::EnumString, strum::VariantNames)]
#[strum(serialize_all = "camelCase")]
enum JillStdFn {
    Identity,
}

/// List of functions available inside Jill-specific `Random` standard module.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, strum::EnumString, strum::VariantNames)]
#[strum(serialize_all = "camelCase")]
enum JillStdRandom {
    // type-associated functions
    #[strum(serialize = "Random")]
    Random,

    // module functions
    Next,
    FromRange,
}

// endregion

// region: JillStd usage tracker

#[derive(Debug)]
pub struct JillStdUsageTracker(HashMap<&'static str, HashMap<&'static str, bool>>);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum JillStdFunctionUsageNoteOutcome {
    NotPartOfJillStd,
    FunctionNotPresentInModule,
    JillStdFunctionUsageNoted,
}

impl JillStdUsageTracker {
    pub fn new() -> Self {
        let jillstd = [
            (JillStdModule::Math.into(), JillStdMath::VARIANTS),
            (JillStdModule::Bool.into(), JillStdBool::VARIANTS),
            (JillStdModule::List.into(), JillStdList::VARIANTS),
            (JillStdModule::Fn.into(), JillStdFn::VARIANTS),
            (JillStdModule::Random.into(), JillStdRandom::VARIANTS),
        ];

        Self(
            jillstd
                .map(|(module, functions)| {
                    (
                        module,
                        functions
                            .iter()
                            // convert each function into a pair `({function}, false)`
                            .map(|&function| (function, false))
                            .collect(),
                    )
                })
                .into(),
        )
    }

    pub fn note_usage(
        &mut self,
        function_reference: &ast::JillFunctionReference,
    ) -> JillStdFunctionUsageNoteOutcome {
        // local function, surely not part of JillStd
        if !function_reference.is_fully_qualified() {
            return JillStdFunctionUsageNoteOutcome::NotPartOfJillStd;
        }

        // NOTE: full path joined, in case the user has folder structure
        // that partially matches JillStd (e.g. `List/.../...`)
        let module_path = function_reference
            .modules_path
            .iter()
            .map(|module_identifier| module_identifier.0.clone())
            .collect::<Vec<_>>()
            .join("::");

        // check that the function reference module is one of JillStd ones
        let Some(module_functions) = self.0.get_mut(module_path.as_str()) else {
            return JillStdFunctionUsageNoteOutcome::NotPartOfJillStd;
        };

        // check that the function reference function name is one of ones in the module
        let function_name = function_reference.type_associated_function_name();
        let Some(function_used) = module_functions.get_mut(function_name.as_str()) else {
            return JillStdFunctionUsageNoteOutcome::FunctionNotPresentInModule;
        };

        *function_used = true;

        JillStdFunctionUsageNoteOutcome::JillStdFunctionUsageNoted
    }

    pub fn construct(&self) -> Vec<vm::VMInstruction> {
        // TODO:
        // option 1: make maps in each module (func name -> generator func) [verbose?]
        // option 2: load definitions (VM instructions) from dedicated files
        // 			 [would have to parse/decode raw instructions]
        todo!()
    }
}

// endregion

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_jill_std_function_usage_tracker() {
        let mut usage_tracker = JillStdUsageTracker::new();

        // case 1
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![ast::JillIdentifier(String::from("List"))],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("map")),
        };

        assert_eq!(
            usage_tracker.note_usage(&function_reference),
            JillStdFunctionUsageNoteOutcome::JillStdFunctionUsageNoted
        );

        // case 2
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![ast::JillIdentifier(String::from("List"))],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("foo")),
        };

        assert_eq!(
            usage_tracker.note_usage(&function_reference),
            JillStdFunctionUsageNoteOutcome::FunctionNotPresentInModule
        );

        // case 3
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![
                ast::JillIdentifier(String::from("List")),
                ast::JillIdentifier(String::from("Items")),
            ],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("foo")),
        };

        assert_eq!(
            usage_tracker.note_usage(&function_reference),
            JillStdFunctionUsageNoteOutcome::NotPartOfJillStd
        );

        // case 4
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![ast::JillIdentifier(String::from("Game"))],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("start")),
        };

        assert_eq!(
            usage_tracker.note_usage(&function_reference),
            JillStdFunctionUsageNoteOutcome::NotPartOfJillStd
        );

        // case 5
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("init")),
        };

        assert_eq!(
            usage_tracker.note_usage(&function_reference),
            JillStdFunctionUsageNoteOutcome::NotPartOfJillStd
        );

        // case 6 (type constructor)
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![ast::JillIdentifier(String::from("List"))],
            associated_type: None,
            function_name: ast::JillIdentifier(String::from("Empty")),
        };

        assert_eq!(
            usage_tracker.note_usage(&function_reference),
            JillStdFunctionUsageNoteOutcome::JillStdFunctionUsageNoted
        );

        // case 7 (variant-associated function)
        let function_reference = ast::JillFunctionReference {
            modules_path: vec![ast::JillIdentifier(String::from("List"))],
            associated_type: Some(ast::JillIdentifier(String::from("List"))),
            function_name: ast::JillIdentifier(String::from("tail")),
        };

        assert_eq!(
            usage_tracker.note_usage(&function_reference),
            JillStdFunctionUsageNoteOutcome::JillStdFunctionUsageNoted
        );
    }
}
