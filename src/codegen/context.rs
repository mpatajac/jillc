pub use module::Context as ModuleContext;
pub use program::Context as ProgramContext;

pub mod module {
    /// Context information regarding the current module.
    #[derive(Debug)]
    pub struct Context {
        pub module_name: String,
        // TODO: scope (funcs | vars)
    }

    impl Context {
        pub const fn new(module_name: String) -> Self {
            Self { module_name }
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

    #[derive(Debug)]
    pub struct FunctionDispatch {}

    impl FunctionDispatch {
        pub fn new() -> Self {
            Self {}
        }
    }
}
