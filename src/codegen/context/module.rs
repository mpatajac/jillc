use std::{collections::HashMap, iter};

use strum::VariantArray;

use crate::codegen::{
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
    Variable,
    Function,
}

type Name = String;

impl Scope {
    pub fn new() -> Self {
        Self {
            // initialize with the module-level scope
            frames: vec![ScopeFrame::new(String::new())],
        }
    }

    /// Add a function to the (latest frame of the) scope
    /// and add new frame for this function.
    ///
    /// This is usually performed during a function definition.
    pub fn enter_function(
        &mut self,
        name: Name,
        context_arguments: FunctionContextArguments,
    ) -> Result<FunctionContext, Error> {
        // check for existing functions/variables with the same name (to prevent shadowing)
        match self.search(&name) {
            ScopeSearchOutcome::Variable => Err(Error::VariableAlreadyInScope(name)),
            ScopeSearchOutcome::Function => Err(Error::FunctionAlreadyInScope(name)),
            ScopeSearchOutcome::NotFound => {
                let context = FunctionContext {
                    arity: context_arguments.arity,
                    prefix: self.construct_function_prefix(),
                    captures: context_arguments.captures.unwrap_or_default(),
                };

                // add to list of existing functions
                self.last_mut_frame()
                    .functions
                    .insert(name.clone(), context.clone());

                // create a frame for the new function
                self.frames.push(ScopeFrame::new(name));

                Ok(context)
            }
        }
    }

    /// Denote that a function definition is ending and that
    /// it should not contain further declarations.
    ///
    /// This is usually performed when leaving a function definition.
    pub fn leave_function(&mut self) {
        self.frames.pop();
    }

    /// Add a new variable to the (latest frame of the) scope.
    ///
    /// This is usually performed during a variable definition.
    pub fn add_variable(
        &mut self,
        name: Name,
        context_arguments: VariableContextArguments,
    ) -> Result<VariableContext, Error> {
        // check for existing functions/variables with the same name (to prevent shadowing)
        match self.search(&name) {
            ScopeSearchOutcome::Variable => Err(Error::VariableAlreadyInScope(name)),
            ScopeSearchOutcome::Function => Err(Error::FunctionAlreadyInScope(name)),
            ScopeSearchOutcome::NotFound => {
                let segment_index = self
                    .last_mut_frame()
                    .variable_segment_indices
                    .add_variable(context_arguments.segment);

                let context = VariableContext {
                    segment: context_arguments.segment,
                    index: segment_index,
                };

                self.last_mut_frame()
                    .variables
                    .insert(name, context.clone());

                Ok(context)
            }
        }
    }

    /// Search through the scope frames for a function with
    /// a given identifier, returning its context upon a successful search.
    // TODO!: figure out how (in what form) to store names!!!!!
    pub fn search_function(&self, identifier: &Name) -> Option<FunctionContext> {
        self.frames
            .iter()
            .rev()
            .find_map(|frame| frame.functions.get(identifier))
            .cloned()
    }

    /// Search through the scope frames for an accessible (global or very local) variable
    /// with a given identifier, returning its context upon a successful search.
    pub fn search_variable(&self, identifier: &Name) -> Option<VariableContext> {
        // first check last (current) frame
        self.last_frame()
            .variables
            .get(identifier)
            .or_else(
                // if not, check globals in the first (module) frame
                || self.first_frame().variables.get(identifier),
            )
            .cloned()
    }

    /// Constructs a prefix from previously defined functions
    /// in which (a potential) current function is nested.
    ///
    /// E.g. function `bar` defined inside the function `foo`
    /// would have a prefix of `foo_` and full name of `foo_bar`.
    fn construct_function_prefix(&self) -> String {
        self.frames
            .iter()
            // skip first (module-level) frame, as it does not
            // have an owner, an therefore a valid prefix
            .skip(1)
            .map(|frame| frame.owner.clone() + "_")
            .collect()
    }

    /// Returns the first (module-level) scope frame.
    fn first_frame(&self) -> &ScopeFrame {
        self.frames
            .first()
            .expect("scope shoule at least have the module-level frame")
    }

    /// Returns the last (latest, currently active) scope frame.
    fn last_frame(&self) -> &ScopeFrame {
        self.frames
            .last()
            .expect("scope shoule at least have the module-level frame")
    }
    /// Returns the mutable last (latest, currently active) scope frame.
    fn last_mut_frame(&mut self) -> &mut ScopeFrame {
        self.frames
            .last_mut()
            .expect("scope shoule at least have the module-level frame")
    }

    /// Search through the scope frames for a variable
    /// or a function with a given identifier,
    /// returning its context upon a successful search.
    ///
    /// Used internally to prevent name shadowing.
    fn search(&self, identifier: &Name) -> ScopeSearchOutcome {
        // search from latest (last) to oldest (first)
        for frame in self.frames.iter().rev() {
            // variables have priority
            if frame.variables.contains_key(identifier) {
                return ScopeSearchOutcome::Variable;
            }

            if frame.functions.contains_key(identifier) {
                return ScopeSearchOutcome::Function;
            }
        }

        ScopeSearchOutcome::NotFound
    }
}

#[derive(Debug)]
pub struct ScopeFrame {
    owner: Name,
    variable_segment_indices: VariableSegmentIndices,
    functions: HashMap<Name, FunctionContext>,
    variables: HashMap<Name, VariableContext>,
}

impl ScopeFrame {
    fn new(owner: Name) -> Self {
        Self {
            owner,
            variable_segment_indices: VariableSegmentIndices::new(),
            functions: HashMap::new(),
            variables: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionContextArguments {
    arity: usize,
    captures: Option<Vec<String>>,
}

impl FunctionContextArguments {
    pub const fn new(arity: usize) -> Self {
        Self {
            arity,
            captures: None,
        }
    }

    pub fn with_captures(self, captures: Vec<String>) -> Self {
        Self {
            captures: Some(captures),
            ..self
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionContext {
    pub arity: usize,
    pub prefix: String,
    pub captures: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariableContextArguments {
    segment: vm::Segment,
}

impl VariableContextArguments {
    pub const fn new(segment: vm::Segment) -> Self {
        Self { segment }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariableContext {
    pub segment: vm::Segment,
    pub index: usize,
}

impl VariableContext {
    /// Helper function for performing a `push`
    /// action with the variable's segment and index.
    pub fn push(&self) -> vm::VMInstruction {
        vm::push(self.segment, self.index)
    }

    /// Helper function for performing a `pop`
    /// action with the variable's segment and index.
    pub fn pop(&self) -> vm::VMInstruction {
        vm::pop(self.segment, self.index)
    }
}

#[derive(Debug)]
struct VariableSegmentIndices {
    indices: HashMap<vm::Segment, usize>,
}

impl VariableSegmentIndices {
    fn new() -> Self {
        Self {
            indices: HashMap::new(),
        }
    }

    /// Gets current segment index and increases it for future usage.
    fn add_variable(&mut self, segment: vm::Segment) -> usize {
        *self
            .indices
            .entry(segment)
            .and_modify(|idx| *idx += 1)
            .or_default()
    }
}

// endregion

#[cfg(test)]
mod tests {
    use crate::codegen::{self, vm};

    #[allow(clippy::too_many_lines)]
    #[test]
    fn test_scope() {
        let mut scope = super::Scope::new();

        assert_eq!(scope.construct_function_prefix(), String::new());

        // let foo = _.
        assert!(scope
            .add_variable(
                "foo".to_string(),
                super::VariableContextArguments::new(vm::Segment::Static)
            )
            .is_ok());

        // fn f a b = _.
        assert!(scope
            .enter_function("f".to_string(), super::FunctionContextArguments::new(2))
            .is_ok());

        assert!(scope
            .add_variable(
                "a".to_string(),
                super::VariableContextArguments::new(vm::Segment::Argument)
            )
            .is_ok());

        assert!(scope
            .add_variable(
                "b".to_string(),
                super::VariableContextArguments::new(vm::Segment::Argument)
            )
            .is_ok());

        // check that variable has correct segment index
        assert!(scope
            .search_variable(&"b".to_string())
            .is_some_and(|ctx| ctx.segment == vm::Segment::Argument && ctx.index == 1));

        assert_eq!(scope.construct_function_prefix(), String::from("f_"));

        scope.leave_function();

        // fn g x = ...
        assert!(scope
            .enter_function("g".to_string(), super::FunctionContextArguments::new(1))
            .is_ok());

        assert!(scope
            .add_variable(
                "x".to_string(),
                super::VariableContextArguments::new(vm::Segment::Argument)
            )
            .is_ok());

        // nested function
        // fn baz a [x] = _.
        assert!(scope
            .enter_function(
                "baz".to_string(),
                super::FunctionContextArguments::new(1).with_captures(vec![String::from("x")])
            )
            .is_ok());

        // `a` already existed, but it was defined in previous function,
        // so by now it should have gone out of scope
        assert!(scope
            .add_variable(
                "a".to_string(),
                super::VariableContextArguments::new(vm::Segment::Argument)
            )
            .is_ok());

        // nested scope frame, variable segment indices should reset
        assert!(scope
            .search_variable(&"a".to_string())
            .is_some_and(|ctx| ctx.segment == vm::Segment::Argument && ctx.index == 0));

        assert_eq!(scope.construct_function_prefix(), String::from("g_baz_"));

        // check that capture is part of the context
        assert!(scope
            .search_function(&"baz".to_string())
            .is_some_and(|ctx| ctx.captures == vec!["x"]));

        scope.leave_function();

        // variable local to the `fn g` scope
        assert!(scope
            .add_variable(
                "bar".to_string(),
                super::VariableContextArguments::new(vm::Segment::Local)
            )
            .is_ok());

        assert!(matches!(
            scope.search_function(&"f".to_string()),
            Some(super::FunctionContext {
                arity,
                prefix,
                captures
            })
        ));

        assert!(matches!(
            scope.search_variable(&"bar".to_string()),
            Some(super::VariableContext { segment, index })
        ));

        assert!(scope.search_function(&"jill".to_string()).is_none());

        // occured twice, but went out of scope both times
        assert!(scope.search_variable(&"a".to_string()).is_none());

        // variable with a same name already exists (global)
        assert!(scope
            .add_variable(
                "foo".to_string(),
                super::VariableContextArguments::new(vm::Segment::Local),
            )
            .is_err_and(|err| matches!(err, codegen::error::Error::VariableAlreadyInScope(_))));
    }
}
