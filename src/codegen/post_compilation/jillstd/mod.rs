use std::{collections::HashMap, str::FromStr};

use strum::{EnumProperty, VariantArray};

use crate::{
    codegen::{
        common::helpers::function::JillFunctionReferenceExtensions, context::ProgramContext, vm,
    },
    common::ast,
    fileio::{self, output::OutputFile},
};

// region: JillStd declaration

/// List of Jill-specific standard modules.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, strum::EnumDiscriminants)]
#[strum(serialize_all = "PascalCase")]
#[strum_discriminants(derive(Hash, strum::EnumString, strum::Display))]
enum JillStdModule {
    Math(JillStdMath),
    Bool(JillStdBool),
    List(JillStdList),
    Random(JillStdRandom),
    Fn(JillStdFn),
}

impl JillStdModuleDiscriminants {
    fn to_std_module(self, function_name: &str) -> Result<JillStdModule, strum::ParseError> {
        match self {
            Self::Math => JillStdMath::from_str(function_name).map(JillStdModule::Math),
            Self::Bool => JillStdBool::from_str(function_name).map(JillStdModule::Bool),
            Self::List => JillStdList::from_str(function_name).map(JillStdModule::List),
            Self::Fn => JillStdFn::from_str(function_name).map(JillStdModule::Fn),
            Self::Random => JillStdRandom::from_str(function_name).map(JillStdModule::Random),
        }
    }
}

impl JillStdModule {
    fn function_name(self) -> String {
        match self {
            Self::Math(f) => f.to_string(),
            Self::Bool(f) => f.to_string(),
            Self::List(f) => f.to_string(),
            Self::Fn(f) => f.to_string(),
            Self::Random(f) => f.to_string(),
        }
    }

    fn function_arity(self) -> usize {
        let raw_arity = match self {
            Self::Math(f) => f.get_str("Arity"),
            Self::Bool(f) => f.get_str("Arity"),
            Self::List(f) => f.get_str("Arity"),
            Self::Fn(f) => f.get_str("Arity"),
            Self::Random(f) => f.get_str("Arity"),
        };

        raw_arity
            .expect("all jill std variants should have associated arity")
            .parse()
            .expect("all associated arities should be valid (usize) numbers")
    }

    const fn instructions(self) -> &'static str {
        // TODO: figure out if there is a more elegant way to do this (proc macro?)
        match self {
            Self::Math(f) => f.instructions(),
            Self::Bool(f) => f.instructions(),
            Self::List(f) => f.instructions(),
            Self::Fn(f) => f.instructions(),
            Self::Random(f) => f.instructions(),
        }
    }
}

// region: Math

/// List of functions available inside Jill-specific `Math` standard module.
#[derive(
    Debug,
    PartialEq,
    Eq,
    Hash,
    Clone,
    Copy,
    strum::EnumString,
    strum::VariantArray,
    strum::Display,
    strum_macros::EnumProperty,
)]
#[strum(serialize_all = "camelCase")]
enum JillStdMath {
    /// `+`
    #[strum(props(Arity = "2"))]
    Add,
    /// `-`
    #[strum(props(Arity = "2"))]
    Sub,
    /// `*`
    #[strum(props(Arity = "2"))]
    Mult,
    /// `/`
    #[strum(props(Arity = "2"))]
    Div,
    /// `%`
    #[strum(props(Arity = "2"))]
    Mod,
    /// `++`
    #[strum(props(Arity = "1"))]
    Inc,
    /// `--`
    #[strum(props(Arity = "1"))]
    Dec,
    // Jack std name matches
    #[strum(props(Arity = "2"))]
    Min,
    #[strum(props(Arity = "2"))]
    Max,
    #[strum(props(Arity = "1"))]
    Sqrt,
}

impl JillStdMath {
    const fn instructions(self) -> &'static str {
        match self {
            Self::Add => include_str!("Math/add.vm"),
            Self::Sub => include_str!("Math/sub.vm"),
            _ => todo!(),
        }
    }
}

// endregion

// region: Bool

/// List of functions available inside Jill-specific `Bool` standard module.
#[derive(
    Debug,
    PartialEq,
    Eq,
    Hash,
    Clone,
    Copy,
    strum::EnumString,
    strum::VariantArray,
    strum::Display,
    strum_macros::EnumProperty,
)]
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

impl JillStdBool {
    const fn instructions(self) -> &'static str {
        todo!()
    }
}

// endregion

// region: List

/// List of functions available inside Jill-specific `List` standard module.
#[derive(
    Debug,
    PartialEq,
    Eq,
    Hash,
    Clone,
    Copy,
    strum::EnumString,
    strum::VariantArray,
    strum::Display,
    strum_macros::EnumProperty,
)]
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

impl JillStdList {
    const fn instructions(self) -> &'static str {
        todo!()
    }
}

// endregion

// region: Fn

/// List of functions available inside Jill-specific `Fn` standard module.
#[derive(
    Debug,
    PartialEq,
    Eq,
    Hash,
    Clone,
    Copy,
    strum::EnumString,
    strum::VariantArray,
    strum::Display,
    strum_macros::EnumProperty,
)]
#[strum(serialize_all = "camelCase")]
enum JillStdFn {
    Identity,
}

impl JillStdFn {
    const fn instructions(self) -> &'static str {
        todo!()
    }
}

// endregion

// region: Random

/// List of functions available inside Jill-specific `Random` standard module.
#[derive(
    Debug,
    PartialEq,
    Eq,
    Hash,
    Clone,
    Copy,
    strum::EnumString,
    strum::VariantArray,
    strum::Display,
    strum_macros::EnumProperty,
)]
#[strum(serialize_all = "camelCase")]
enum JillStdRandom {
    // type-associated functions
    #[strum(serialize = "Random")]
    Random,

    // module functions
    Next,
    FromRange,
}

impl JillStdRandom {
    const fn instructions(self) -> &'static str {
        todo!()
    }
}

// endregion

// endregion

// region: JillStd usage tracker

#[derive(Debug)]
pub struct JillStdUsageTracker(HashMap<JillStdModuleDiscriminants, HashMap<JillStdModule, bool>>);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum JillStdFunctionUsageNoteOutcome {
    NotPartOfJillStd,
    FunctionNotPresentInModule,
    JillStdFunctionUsageNoted,
}

impl JillStdUsageTracker {
    pub fn new() -> Self {
        let jillstd = [
            (
                JillStdModuleDiscriminants::Math,
                Self::functions_from_variants(JillStdModule::Math),
            ),
            (
                JillStdModuleDiscriminants::Bool,
                Self::functions_from_variants(JillStdModule::Bool),
            ),
            (
                JillStdModuleDiscriminants::List,
                Self::functions_from_variants(JillStdModule::List),
            ),
            (
                JillStdModuleDiscriminants::Fn,
                Self::functions_from_variants(JillStdModule::Fn),
            ),
            (
                JillStdModuleDiscriminants::Random,
                Self::functions_from_variants(JillStdModule::Random),
            ),
        ];

        Self(
            jillstd
                .map(|(module, functions)| {
                    (
                        module,
                        functions
                            .into_iter()
                            // convert each function into a pair `({function}, false)`
                            .map(|function| (function, false))
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
        let Ok(module_name) = JillStdModuleDiscriminants::from_str(&module_path) else {
            return JillStdFunctionUsageNoteOutcome::NotPartOfJillStd;
        };

        let module_functions = self
            .0
            .get_mut(&module_name)
            .expect("should be one of Jill std modules (because it successfully parsed)");

        // check that the function reference function name is one of ones in the module
        let function_name = function_reference.type_associated_function_name();
        let Ok(function) = module_name.to_std_module(&function_name) else {
            return JillStdFunctionUsageNoteOutcome::FunctionNotPresentInModule;
        };
        let Some(function_used) = module_functions.get_mut(&function) else {
            return JillStdFunctionUsageNoteOutcome::FunctionNotPresentInModule;
        };

        *function_used = true;

        JillStdFunctionUsageNoteOutcome::JillStdFunctionUsageNoted
    }

    fn functions_from_variants<T: VariantArray + std::marker::Copy>(
        jillstd_module_mapping: fn(T) -> JillStdModule,
    ) -> Vec<JillStdModule> {
        T::VARIANTS
            .iter()
            .copied()
            .map(jillstd_module_mapping)
            .collect()
    }
}

// endregion

pub fn sys_output() -> fileio::output::OutputFile {
    let sys_vm = include_str!("Sys.vm").to_string();

    fileio::output::OutputFile::new(String::from("Sys.vm"), sys_vm)
}

pub fn construct(program_context: &mut ProgramContext) -> impl Iterator<Item = OutputFile> + '_ {
    program_context
        .std_usage_tracker
        .0
        .iter()
        .filter_map(|(&module_kind, functions)| {
            // if no function in module is marked as used, then we can skip the whole module
            if !functions.iter().any(|(_, &is_used)| is_used) {
                return None;
            }

            let instructions = functions
                .iter()
                .filter_map(|(f, &is_used)| if is_used { Some(f) } else { None })
                .map(|module_function| {
                    // note function arity
                    let vm_function_name = vm::VMFunctionName::construct(
                        &module_kind.to_string(),
                        "",
                        &module_function.function_name(),
                    );

                    program_context
                        .program_metadata
                        .log_function_arity(vm_function_name, module_function.function_arity())
                        .expect("should be a valid JillStd function");

                    // map to compiled VM
                    module_function.instructions().trim()
                })
                .collect::<Vec<_>>()
                .join("\n");

            Some(OutputFile::new(module_kind.to_string(), instructions))
        })
}

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
