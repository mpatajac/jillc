use std::collections::HashMap;

use crate::codegen::vm;

// region: Context

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

// endregion

// region: Type Info

#[derive(Debug)]
pub struct TypeInfo {
    pub current_variant: usize,
}

impl TypeInfo {
    pub const fn new() -> Self {
        Self { current_variant: 0 }
    }
}

// endregion

// region: Scope

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

// endregion
