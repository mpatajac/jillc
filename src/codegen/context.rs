pub use module::Context as ModuleContext;
pub use program::Context as ProgramContext;

pub mod module {
    use crate::codegen::vm;

    /// Context information regarding the current module.
    #[derive(Debug)]
    pub struct Context {
        pub module_name: String,
        // TODO: scope (funcs | vars)
        pub type_info: TypeInfo,
        pub output: vm::VMModule,
    }

    impl Context {
        pub const fn new(module_name: String) -> Self {
            Self {
                module_name,
                type_info: TypeInfo::new(),
                output: vm::VMModule::new(),
            }
        }
    }

    #[derive(Debug)]
    pub struct TypeInfo {
        pub is_multivariant: bool,
        pub current_variant: usize,
    }

    impl TypeInfo {
        pub const fn new() -> Self {
            Self {
                is_multivariant: false,
                current_variant: 0,
            }
        }
    }
}

pub mod program {
    use std::collections::HashMap;

    /// Context information regarding the entire program, across modules
    /// (e.g. which parts of the `std` need to be generated,
    /// which functions need to be dispatched etc.).
    #[derive(Debug)]
    pub struct Context {
        pub function_dispatch: FunctionDispatch,
    }

    impl Context {
        pub fn new() -> Self {
            Self {
                function_dispatch: FunctionDispatch::new(),
            }
        }
    }

    type FunctionReferenceIndex = usize;
    type FunctionReferenceCount = usize;
    type FunctionReferenceName = String;

    /// Track functions which are used as a first-class object
    /// (i.e. function references from AST) for dispatch generation.
    #[derive(Debug)]
    pub struct FunctionDispatch {
        references:
            HashMap<FunctionReferenceName, (FunctionReferenceIndex, FunctionReferenceCount)>,
    }

    impl FunctionDispatch {
        pub fn new() -> Self {
            Self {
                references: HashMap::new(),
            }
        }

        /// Handle function reference encounter by either
        /// adding it to existing references (if not already present)
        /// or increasing it's count.
        pub fn encounter(&mut self, name: FunctionReferenceName) -> FunctionReferenceIndex {
            let num_of_items = self.references.len();

            self.references
                .entry(name)
                .and_modify(|(_, count)| *count += 1)
                .or_insert((num_of_items, 1))
                .0
        }

        /// Return a [Vec] of encountered function references with their indices,
        /// ordered by their reference count.
        pub fn collect(self) -> Vec<(FunctionReferenceName, FunctionReferenceIndex)> {
            let mut items: Vec<_> = self.references.into_iter().collect();

            // sort items by their count, descending
            items.sort_by(|(_, (_, a_count)), (_, (_, b_count))| a_count.cmp(b_count).reverse());

            // map to (name, index)
            items
                .into_iter()
                .map(|(name, (idx, _))| (name, idx))
                .collect()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function_dispatch() {
        let mut fn_dispatch = program::FunctionDispatch::new();

        fn_dispatch.encounter(String::from("foo"));
        fn_dispatch.encounter(String::from("bar"));
        fn_dispatch.encounter(String::from("foo"));
        fn_dispatch.encounter(String::from("baz"));
        fn_dispatch.encounter(String::from("biz"));
        fn_dispatch.encounter(String::from("bar"));
        fn_dispatch.encounter(String::from("foo"));
        fn_dispatch.encounter(String::from("baz"));
        fn_dispatch.encounter(String::from("foo"));
        fn_dispatch.encounter(String::from("baz"));

        let collection = fn_dispatch.collect();

        assert_eq!(
            collection,
            vec![
                (String::from("foo"), 0),
                (String::from("baz"), 2),
                (String::from("bar"), 1),
                (String::from("biz"), 3),
            ]
        );
    }
}
