use phf::phf_map;

use crate::{codegen::vm, common::ast};

use super::function::JillFunctionReferenceExtensions;

type ModuleName = &'static str;
type FunctionName = &'static str;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FunctionOverrideKind {
    VM(vm::VMCommand),
    JackStd(ModuleName, FunctionName),
}

static OVERRIDES: phf::Map<&'static str, FunctionOverrideKind> = phf_map! {
    // VM
    "Math.add" => FunctionOverrideKind::VM(vm::VMCommand::Add),

    // JackStd
    "Math.mult" => FunctionOverrideKind::JackStd("Math", "multiply"),
};

pub fn find_override(
    function_reference: &ast::JillFunctionReference,
) -> Option<FunctionOverrideKind> {
    let vm_function_name =
        function_reference.to_fully_qualified_hack_name(&String::new(), String::new());

    OVERRIDES.get(&vm_function_name).cloned()
}
