use phf::phf_map;

use crate::codegen::{context::program::JillFunctionMetadata, vm};

static ARITIES: phf::Map<&'static str, usize> = phf_map! {
    // Math
    "Math.multiply" => 2,
    "Math.divide" => 2,
    "Math.min" => 2,
    "Math.max" => 2,
    "Math.sqrt" => 1,

    // String
    "String.new" => 1,
    "String.dispose" => 1,
    "String.length" => 1,
    "String.charAt" => 2,
    "String.setCharAt" => 3,
    "String.appendChar" => 2,
    "String.eraseLastChar" => 1,
    "String.intValue" => 1,
    "String.setInt" => 2,
    "String.backSpace" => 0,
    "String.doubleQuote" => 0,
    "String.newLine" => 0,

    // Array
    "Array.new" => 1,
    "Array.dispose" => 1,

    // Output
    "Output.moveCursor" => 2,
    "Output.printChar" => 1,
    "Output.printString" => 1,
    "Output.printInt" => 1,
    "Output.println" => 0,
    "Output.backSpace" => 0,

    // Screen
    "Screen.clearScreen" => 0,
    "Screen.setColor" => 1,
    "Screen.drawPixel" => 2,
    "Screen.drawLine" => 4,
    "Screen.drawRectangle" => 4,
    "Screen.drawCircle" => 3,

    // Keyboard
    "Keyboard.keyPressed" => 0,
    "Keyboard.readChar" => 0,
    "Keyboard.readLine" => 1,
    "Keyboard.readInt" => 1,

    // Memory
    "Memory.peek" => 1,
    "Memory.poke" => 2,
    "Memory.alloc" => 1,
    "Memory.deAlloc" => 1,

    // Sys
    "Sys.halt" => 0,
    "Sys.error" => 1,
    "Sys.wait" => 1,
};

pub fn function_arity(vm_function_name: &vm::VMFunctionName) -> Option<JillFunctionMetadata> {
    ARITIES
        .get(&vm_function_name.to_string())
        .copied()
        .map(|arity| JillFunctionMetadata {
            arity,
            // Jack API functions have no captures
            has_captures: false,
        })
}
