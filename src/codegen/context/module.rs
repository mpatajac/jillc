use std::collections::HashMap;

use crate::codegen::{
    self,
    error::{Error, FallableAction},
    vm,
};

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScopeSearchOutcome {
    NotFound,
    Variable(VariableContext),
    Function(FunctionContext),
}

type Name = String;

impl Scope {
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
    pub fn add_function(&mut self, name: Name, context: FunctionContext) -> FallableAction {
        // check for existing functions/variables with the same name (to prevent shadowing)
        match self.search(&name) {
            ScopeSearchOutcome::Variable(_) => Err(Error::VariableAlreadyInScope(name)),
            ScopeSearchOutcome::Function(_) => Err(Error::FunctionAlreadyInScope(name)),
            ScopeSearchOutcome::NotFound => {
                self.last_frame().functions.insert(name, context);
                Ok(())
            }
        }
    }

    /// Add a new variable to the (latest frame of the) scope.
    /// This is usually performed after a variable definition.
    pub fn add_variable(&mut self, name: Name, context: VariableContext) -> FallableAction {
        // check for existing functions/variables with the same name (to prevent shadowing)
        match self.search(&name) {
            ScopeSearchOutcome::Variable(_) => Err(Error::VariableAlreadyInScope(name)),
            ScopeSearchOutcome::Function(_) => Err(Error::FunctionAlreadyInScope(name)),
            ScopeSearchOutcome::NotFound => {
                self.last_frame().variables.insert(name, context);
                Ok(())
            }
        }
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionContext {
    pub number_of_arguments: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariableContext {
    // TODO?: separate variables by segments?
    pub segment: vm::Segment,
    pub index: usize,
}

// endregion

#[cfg(test)]
mod tests {
    use crate::codegen::{self, vm};

    #[allow(clippy::too_many_lines)]
    #[test]
    fn test_scope() {
        let mut scope = super::Scope::new();

        assert!(scope
            .add_variable(
                "foo".to_string(),
                super::VariableContext {
                    segment: vm::Segment::Static,
                    index: 0,
                },
            )
            .is_ok());

        assert!(scope
            .add_function(
                "f".to_string(),
                super::FunctionContext {
                    number_of_arguments: 2
                },
            )
            .is_ok());

        // fn f a b = _.
        scope.add_frame();

        assert!(scope
            .add_variable(
                "a".to_string(),
                super::VariableContext {
                    segment: vm::Segment::Argument,
                    index: 0,
                },
            )
            .is_ok());

        assert!(scope
            .add_variable(
                "b".to_string(),
                super::VariableContext {
                    segment: vm::Segment::Argument,
                    index: 1,
                },
            )
            .is_ok());

        scope.drop_frame();

        assert!(scope
            .add_function(
                "g".to_string(),
                super::FunctionContext {
                    number_of_arguments: 1
                },
            )
            .is_ok());

        // fn g x = ...
        scope.add_frame();

        assert!(scope
            .add_variable(
                "x".to_string(),
                super::VariableContext {
                    segment: vm::Segment::Argument,
                    index: 0,
                },
            )
            .is_ok());

        // nested function
        assert!(scope
            .add_function(
                "baz".to_string(),
                super::FunctionContext {
                    number_of_arguments: 1
                },
            )
            .is_ok());

        // fn baz a = _.
        scope.add_frame();

        // `a` already existed, but it was defined in previous function,
        // so by now it should have gone out of scope
        assert!(scope
            .add_variable(
                "a".to_string(),
                super::VariableContext {
                    segment: vm::Segment::Local,
                    index: 0,
                },
            )
            .is_ok());

        scope.drop_frame();

        // variable local to the `fn g` scope
        assert!(scope
            .add_variable(
                "bar".to_string(),
                super::VariableContext {
                    segment: vm::Segment::Local,
                    index: 0,
                },
            )
            .is_ok());

        assert!(matches!(
            scope.search(&"f".to_string()),
            super::ScopeSearchOutcome::Function(super::FunctionContext {
                number_of_arguments
            })
        ));

        assert!(matches!(
            scope.search(&"bar".to_string()),
            super::ScopeSearchOutcome::Variable(super::VariableContext { segment, index })
        ));

        assert!(matches!(
            scope.search(&"jill".to_string()),
            super::ScopeSearchOutcome::NotFound
        ));

        // occured twice, but went out of scope both times
        assert!(matches!(
            scope.search(&"a".to_string()),
            super::ScopeSearchOutcome::NotFound
        ));

        // variable with a same name already exists (global)
        assert!(scope
            .add_variable(
                "foo".to_string(),
                super::VariableContext {
                    segment: vm::Segment::Local,
                    index: 1,
                },
            )
            .is_err_and(|err| matches!(err, codegen::error::Error::VariableAlreadyInScope(_))));
    }
}
