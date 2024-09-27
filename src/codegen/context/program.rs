use std::collections::{HashMap, HashSet};

use crate::codegen::{
    error::{Error, FallableAction},
    post_compilation::jillstd::JillStdUsageTracker,
    vm,
};

// region: Context

type ModuleName = String;

/// Context information regarding the entire program, across modules
/// (e.g. which parts of the `std` need to be generated,
/// which functions need to be dispatched etc.).
#[derive(Debug)]
pub struct Context {
    pub function_dispatch: FunctionDispatch,
    pub std_usage_tracker: JillStdUsageTracker,
    pub program_metadata: JillProgramMetadata,
    pub globals: HashSet<ModuleName>,
    pub temp_segment_index: TempSegmentIndex,
}

impl Context {
    pub fn new() -> Self {
        Self {
            function_dispatch: FunctionDispatch::new(),
            std_usage_tracker: JillStdUsageTracker::new(),
            program_metadata: JillProgramMetadata::new(),
            globals: HashSet::new(),
            temp_segment_index: TempSegmentIndex(0),
        }
    }
}

// endregion

// region: Function Dispatch

pub type FunctionReferenceIndex = usize;
type FunctionReferenceCount = usize;

/// Track functions which are used as a first-class object
/// (i.e. function references from AST) for dispatch generation.
#[derive(Debug)]
pub struct FunctionDispatch {
    references: HashMap<vm::VMFunctionName, (FunctionReferenceIndex, FunctionReferenceCount)>,
}

impl FunctionDispatch {
    pub fn new() -> Self {
        Self {
            references: HashMap::new(),
        }
    }

    /// Handle function reference encounter by either
    /// adding it to existing references (if not already present)
    /// or increasing its count.
    pub fn encounter(&mut self, name: vm::VMFunctionName) -> FunctionReferenceIndex {
        let num_of_items = self.references.len();

        self.references
            .entry(name)
            .and_modify(|(_, count)| *count += 1)
            .or_insert((num_of_items, 1))
            .0
    }

    /// Return a [Vec] of encountered function references with their indices,
    /// ordered by their reference count.
    pub fn collect(&self) -> Vec<(vm::VMFunctionName, FunctionReferenceIndex)> {
        let mut items: Vec<_> = self.references.iter().collect();

        // sort items by their count, descending
        items.sort_by(|(_, (_, a_count)), (_, (_, b_count))| a_count.cmp(b_count).reverse());

        // map to (name, index)
        items
            .into_iter()
            .map(|(name, (idx, _))| (name.clone(), *idx))
            .collect()
    }
}

// endregion

// region: Program metadata

#[derive(Debug)]
pub struct JillProgramMetadata {
    top_level_function_arities: HashMap<vm::VMFunctionName, usize>,
}

impl JillProgramMetadata {
    pub fn new() -> Self {
        Self {
            top_level_function_arities: HashMap::new(),
        }
    }

    pub fn log_function_arity(&mut self, name: vm::VMFunctionName, arity: usize) -> FallableAction {
        if self
            .top_level_function_arities
            .insert(name.clone(), arity)
            .is_some()
        {
            Err(Error::MultipleFunctionDefinitions(name))
        } else {
            Ok(())
        }
    }

    pub fn get_function_arity(&self, name: vm::VMFunctionName) -> Option<usize> {
        self.top_level_function_arities.get(&name).copied()
    }
}

// endregion

#[derive(Debug)]
pub struct TempSegmentIndex(usize);

impl TempSegmentIndex {
    /// Request the first available index for the `temp` segment
    /// (incrementing the counter in the process for further usage).
    pub fn request(&mut self) -> usize {
        let next = self.0 + 1;

        // replace current (free) index with the next one
        // and return the current (previous) index
        std::mem::replace(&mut self.0, next)
    }

    /// Mark the index that was last used as free for further usage
    /// (by decrementing the internal counter).
    pub fn release(&mut self) {
        self.0 -= 1;
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::{error::Error, vm};

    use super::JillProgramMetadata;

    #[allow(clippy::similar_names)]
    #[test]
    fn test_function_dispatch() {
        let mut fn_dispatch = super::FunctionDispatch::new();

        let foo = vm::VMFunctionName::from_literal("foo");
        let baz = vm::VMFunctionName::from_literal("baz");
        let bar = vm::VMFunctionName::from_literal("bar");
        let biz = vm::VMFunctionName::from_literal("biz");

        fn_dispatch.encounter(foo.clone());
        fn_dispatch.encounter(bar.clone());
        fn_dispatch.encounter(foo.clone());
        fn_dispatch.encounter(baz.clone());
        fn_dispatch.encounter(biz.clone());
        fn_dispatch.encounter(bar.clone());
        fn_dispatch.encounter(foo.clone());
        fn_dispatch.encounter(baz.clone());
        fn_dispatch.encounter(foo.clone());
        fn_dispatch.encounter(baz.clone());

        let collection = fn_dispatch.collect();

        assert_eq!(collection, vec![(foo, 0), (baz, 2), (bar, 1), (biz, 3),]);
    }

    #[test]
    fn test_program_metadata() {
        let mut program_metadata = JillProgramMetadata::new();

        // setup
        assert!(program_metadata
            .log_function_arity(vm::VMFunctionName::from_literal("Foo.foo"), 2)
            .is_ok());

        assert!(program_metadata
            .log_function_arity(vm::VMFunctionName::from_literal("Foo.bar"), 1)
            .is_ok());

        assert!(program_metadata
            .log_function_arity(vm::VMFunctionName::from_literal("Bar.bar"), 4)
            .is_ok());

        // existing function
        assert_eq!(
            program_metadata.get_function_arity(vm::VMFunctionName::from_literal("Foo.bar")),
            Some(1)
        );

        // non-existing function
        assert!(program_metadata
            .get_function_arity(vm::VMFunctionName::from_literal("Foo.baz"))
            .is_none());

        // duplicate function log
        assert!(program_metadata
            .log_function_arity(vm::VMFunctionName::from_literal("Bar.bar"), 4)
            .is_err_and(|err| matches!(err, Error::MultipleFunctionDefinitions(_))));
    }
}
