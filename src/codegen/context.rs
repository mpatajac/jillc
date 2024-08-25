pub use module::Context as ModuleContext;
pub use program::Context as ProgramContext;

pub mod module {
    use std::collections::HashMap;

    use crate::codegen::vm;

    /// Context information regarding the current module.
    #[derive(Debug)]
    pub struct Context {
        pub module_name: String,
        pub type_info: TypeInfo,
        pub scope: Scope,
        pub output: vm::VMModule,
    }

    impl Context {
        pub fn new(module_name: String) -> Self {
            Self {
                module_name,
                type_info: TypeInfo::new(),
                scope: Scope::new(),
                output: vm::VMModule::new(),
            }
        }
    }

    #[derive(Debug)]
    pub struct TypeInfo {
        pub current_variant: usize,
    }

    impl TypeInfo {
        pub const fn new() -> Self {
            Self { current_variant: 0 }
        }
    }

    #[derive(Debug)]
    pub struct Scope {
        frames: Vec<ScopeFrame>,
    }

    #[derive(Debug)]
    pub enum ScopeSearchOutcome {
        NotFound,
        Variable(VariableContext),
        Function(FunctionContext),
    }

    impl Scope {
        // TODO: test `Scope` framework

        pub fn new() -> Self {
            Self {
                // initialize with the module-level scope
                frames: vec![ScopeFrame::new()],
            }
        }

        /// Add a new frame to scope.
        /// This is usually performed when entering a new function definition.
        pub fn add_frame(&mut self) {
            self.frames.push(ScopeFrame::new());
        }

        /// Drop the last frame from the scope.
        /// This is usually performed when leaving a function definition.
        pub fn drop_frame(&mut self) {
            self.frames.pop();
        }

        /// Add a new function to the (latest frame of the) scope.
        /// This is usually performed after a function definition.
        pub fn add_function(&mut self, name: Name, context: FunctionContext) {
            // TODO?: check for a variable (or a function?) with the same name
            self.last_frame().functions.insert(name, context);
        }

        /// Add a new variable to the (latest frame of the) scope.
        /// This is usually performed after a variable definition.
        pub fn add_variable(&mut self, name: Name, context: VariableContext) {
            // TODO?: check for a function with the same name
            // TODO: what to do with variable shadowing?
            self.last_frame().variables.insert(name, context);
        }

        /// Returns the last (latest, currently active) scope frame.
        fn last_frame(&mut self) -> &mut ScopeFrame {
            self.frames
                .last_mut()
                .expect("scope shoule at least have the module-level frame")
        }

        /// Search through the scope frames for a (local) variable
        /// or a function with a given identifier,
        /// returning its context upon a successful search.
        // TODO?: split into separate functions (for vars/funcs)
        pub fn search(&self, identifier: &Name) -> ScopeSearchOutcome {
            // search from latest (last) to oldest (first)
            for frame in self.frames.iter().rev() {
                // variables have priority
                if let Some(variable) = frame.variables.get(identifier) {
                    return ScopeSearchOutcome::Variable(variable.clone());
                }

                if let Some(function) = frame.functions.get(identifier) {
                    return ScopeSearchOutcome::Function(function.clone());
                }
            }

            ScopeSearchOutcome::NotFound
        }
    }

    type Name = String;
    #[derive(Debug)]
    pub struct ScopeFrame {
        functions: HashMap<Name, FunctionContext>,
        variables: HashMap<Name, VariableContext>,
    }

    impl ScopeFrame {
        fn new() -> Self {
            Self {
                functions: HashMap::new(),
                variables: HashMap::new(),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct FunctionContext {
        number_of_arguments: usize,
    }

    #[derive(Debug, Clone)]
    pub struct VariableContext {
        // TODO?: separate variables by segments?
        segment: vm::Segment,
        index: usize,
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
