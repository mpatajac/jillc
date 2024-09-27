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
    Int(JillStdInt),
    Bool(JillStdBool),
    List(JillStdList),
    Random(JillStdRandom),
    StdUtils(JillStdUtils),
}

impl JillStdModuleDiscriminants {
    fn to_std_module(self, function_name: &str) -> Result<JillStdModule, strum::ParseError> {
        match self {
            Self::Int => JillStdInt::from_str(function_name).map(JillStdModule::Int),
            Self::Bool => JillStdBool::from_str(function_name).map(JillStdModule::Bool),
            Self::List => JillStdList::from_str(function_name).map(JillStdModule::List),
            Self::StdUtils => JillStdUtils::from_str(function_name).map(JillStdModule::StdUtils),
            Self::Random => JillStdRandom::from_str(function_name).map(JillStdModule::Random),
        }
    }
}

impl JillStdModule {
    fn function_name(self) -> String {
        match self {
            Self::Int(f) => f.to_string(),
            Self::Bool(f) => f.to_string(),
            Self::List(f) => f.to_string(),
            Self::StdUtils(f) => f.to_string(),
            Self::Random(f) => f.to_string(),
        }
    }

    fn function_arity(self) -> usize {
        self.get_attribute("Arity")
            .expect("all jill std variants should have associated arity")
            .parse()
            .expect("all associated arities should be valid (usize) numbers")
    }

    const fn instructions(self) -> &'static str {
        // TODO: figure out if there is a more elegant way to do this (proc macro?)
        match self {
            Self::Int(f) => f.instructions(),
            Self::Bool(f) => f.instructions(),
            Self::List(f) => f.instructions(),
            Self::StdUtils(f) => f.instructions(),
            Self::Random(f) => f.instructions(),
        }
    }

    fn dependencies(self) -> Vec<ast::JillFunctionReference> {
        self.get_attribute("Dependencies")
            .map_or_else(Vec::new, |dependencies| {
                dependencies
                    .split(',')
                    // trailing comma in dependency list leaves an "empty dependency"
                    .filter(|d| !d.trim().is_empty())
                    .map(jillstd_literal_to_function_reference)
                    .collect()
            })
    }

    fn get_attribute(self, enum_attribute: &str) -> Option<&'static str> {
        match self {
            Self::Int(f) => f.get_str(enum_attribute),
            Self::Bool(f) => f.get_str(enum_attribute),
            Self::List(f) => f.get_str(enum_attribute),
            Self::StdUtils(f) => f.get_str(enum_attribute),
            Self::Random(f) => f.get_str(enum_attribute),
        }
    }
}

fn jillstd_literal_to_function_reference(literal: &str) -> ast::JillFunctionReference {
    let function_components: Vec<_> = literal.trim().split('.').collect();

    // expect literals to be in form `Module.function`
    assert!(
        function_components.len() == 2,
        "invalid jillstd literal: {literal}"
    );

    let module_name = function_components[0];
    let function_name = function_components[1];

    ast::JillFunctionReference {
        modules_path: vec![ast::JillIdentifier(module_name.to_string())],
        associated_type: None,
        function_name: ast::JillIdentifier(function_name.to_string()),
    }
}

// region: Int

/// List of functions available inside Jill-specific `Int` standard module.
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
enum JillStdInt {
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

impl JillStdInt {
    const fn instructions(self) -> &'static str {
        match self {
            Self::Add => include_str!("Int/add.vm"),
            Self::Sub => include_str!("Int/sub.vm"),
            Self::Mult => include_str!("Int/mult.vm"),
            Self::Div => include_str!("Int/div.vm"),
            Self::Mod => include_str!("Int/mod.vm"),
            Self::Inc => include_str!("Int/inc.vm"),
            Self::Dec => include_str!("Int/dec.vm"),
            // Jack API overrides - no need to generate anything
            Self::Min | Self::Max | Self::Sqrt => "",
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
    #[strum(props(Arity = "2"))]
    Eq,
    /// `!=`
    #[strum(props(Arity = "2"))]
    Ne,
    /// `&&`
    #[strum(props(Arity = "2"))]
    And,
    /// `||`
    #[strum(props(Arity = "2"))]
    Or,
    /// `!`
    #[strum(props(Arity = "1"))]
    Not,
    /// `<`
    #[strum(props(Arity = "2"))]
    Lt,
    /// `>`
    #[strum(props(Arity = "2"))]
    Gt,
    /// `<=`
    #[strum(props(Arity = "2"))]
    Le,
    /// `>=`
    #[strum(props(Arity = "2"))]
    Ge,
}

impl JillStdBool {
    const fn instructions(self) -> &'static str {
        match self {
            Self::Eq => include_str!("Bool/eq.vm"),
            Self::Ne => include_str!("Bool/ne.vm"),
            Self::And => include_str!("Bool/and.vm"),
            Self::Or => include_str!("Bool/or.vm"),
            Self::Not => include_str!("Bool/not.vm"),
            Self::Lt => include_str!("Bool/lt.vm"),
            Self::Gt => include_str!("Bool/gt.vm"),
            Self::Le => include_str!("Bool/le.vm"),
            Self::Ge => include_str!("Bool/ge.vm"),
        }
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
    #[strum(props(Arity = "1"))]
    #[strum(serialize = "List_tag")]
    Tag,

    #[strum(props(Arity = "0"))]
    #[strum(serialize = "Empty")]
    Empty,

    #[strum(props(Arity = "2"))]
    #[strum(serialize = "List")]
    List,

    #[strum(props(Arity = "1"))]
    #[strum(serialize = "List_head")]
    Head,

    #[strum(props(Arity = "1"))]
    #[strum(serialize = "List_tail")]
    Tail,

    // module functions
    #[strum(props(Arity = "1"))]
    #[strum(props(Dependencies = "
		List.List_tag,
		List.Empty,
		List.List,
		List.List_head,
		List.List_tail,
	"))]
    Reverse,

    #[strum(props(Arity = "2"))]
    #[strum(props(Dependencies = "
		List.List_tag,
		List.Empty,
		List.List,
		List.List_head,
		List.List_tail,
		List.reverse
	"))]
    Map,

    #[strum(props(Arity = "2"))]
    #[strum(props(Dependencies = "
		List.List_tag,
		List.Empty,
		List.List,
		List.List_head,
		List.List_tail,
		List.reverse
	"))]
    Filter,

    #[strum(props(Arity = "3"))]
    #[strum(props(Dependencies = "
		List.List_tag,
		List.List_head,
		List.List_tail,
	"))]
    Fold,

    #[strum(props(Arity = "2"))]
    #[strum(props(Dependencies = "
		Int.dec,
		List.Empty,
		List.List,
	"))]
    Repeat,

    #[strum(props(Arity = "2"))]
    #[strum(props(Dependencies = "
		Int.dec,
		List.Empty,
		List.List,
	"))]
    Range,

    #[strum(props(Arity = "1"))]
    #[strum(props(Dependencies = "
		Int.inc,
		List.List_tag,
		List.List_tail,
	"))]
    Length,

    #[strum(props(Arity = "1"))]
    #[strum(props(Dependencies = "List.List_tag"))]
    IsEmpty,

    #[strum(props(Arity = "2"))]
    #[strum(props(Dependencies = "
		List.List_tag,
		List.List_head,
		List.List_tail,
	"))]
    All,

    #[strum(props(Arity = "2"))]
    #[strum(props(Dependencies = "
		List.List_tag,
		List.List_head,
		List.List_tail,
	"))]
    Any,
}

impl JillStdList {
    const fn instructions(self) -> &'static str {
        match self {
            Self::Tag => include_str!("List/List_tag.vm"),
            Self::Empty => include_str!("List/Empty.vm"),
            Self::List => include_str!("List/List.vm"),
            Self::Head => include_str!("List/List_head.vm"),
            Self::Tail => include_str!("List/List_tail.vm"),
            Self::Reverse => include_str!("List/reverse.vm"),
            Self::Map => include_str!("List/map.vm"),
            Self::Filter => include_str!("List/filter.vm"),
            Self::Fold => include_str!("List/fold.vm"),
            Self::Repeat => include_str!("List/repeat.vm"),
            Self::Range => include_str!("List/range.vm"),
            Self::Length => include_str!("List/length.vm"),
            Self::IsEmpty => include_str!("List/isEmpty.vm"),
            Self::All => include_str!("List/all.vm"),
            Self::Any => include_str!("List/any.vm"),
        }
    }
}

// endregion

// region: StdUtils

/// List of functions available inside Jill-specific `StdUtils` standard module.
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
enum JillStdUtils {
    #[strum(props(Arity = "1"))]
    Identity,
}

impl JillStdUtils {
    const fn instructions(self) -> &'static str {
        match self {
            Self::Identity => include_str!("StdUtils/identity.vm"),
        }
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
    #[strum(props(Arity = "1"))]
    #[strum(serialize = "Random")]
    Random,

    // module functions
    #[strum(props(Arity = "1"))]
    #[strum(props(Dependencies = "Int.mod"))]
    Next,

    #[strum(props(Arity = "3"))]
    #[strum(props(Dependencies = "
		Int.mod,
		Random.next
	"))]
    FromRange,
}

impl JillStdRandom {
    const fn instructions(self) -> &'static str {
        match self {
            Self::Random => include_str!("Random/Random.vm"),
            Self::Next => include_str!("Random/next.vm"),
            Self::FromRange => include_str!("Random/fromRange.vm"),
        }
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
                JillStdModuleDiscriminants::Int,
                Self::functions_from_variants(JillStdModule::Int),
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
                JillStdModuleDiscriminants::StdUtils,
                Self::functions_from_variants(JillStdModule::StdUtils),
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
            .join("_");

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

        // note dependencies (if any)
        for dependency in function.dependencies() {
            self.note_usage(&dependency);
        }

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
                        .log_function_metadata(
                            vm_function_name,
                            module_function.function_arity(),
                            // JillStd functions have no captures
                            false,
                        )
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

        // check that `List.map` was successfully added
        assert_eq!(
            usage_tracker.note_usage(&function_reference),
            JillStdFunctionUsageNoteOutcome::JillStdFunctionUsageNoted
        );

        // check that `List.map` dependencies were successfully added
        let list_noted_functions = usage_tracker
            .0
            .get(&JillStdModuleDiscriminants::List)
            .expect("List module should be present in JillStd usage notes");

        let is_list_function_noted = |&f| list_noted_functions.get(&f).is_some_and(|&b| b);

        assert!([
            JillStdModule::List(JillStdList::Tag),
            JillStdModule::List(JillStdList::Empty),
            JillStdModule::List(JillStdList::List),
            JillStdModule::List(JillStdList::Head),
            JillStdModule::List(JillStdList::Tail),
            JillStdModule::List(JillStdList::Reverse),
        ]
        .iter()
        .all(is_list_function_noted));

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
