//! Code regarding file input (reading source `.jill` files)
//! and output (writting generated `.vm` files) actions.
//!
//! ## Input
//! `jillc` expects to be run in the root directory of the Jill project,
//! with all the `.jill` files placed in the `./src` directory.
//!
//! Note that, unlike `JackCompiler`, `jillc` allows source files
//! to be in nested directories (so long as they are all in the `./src` directory).
//!
//! ## Output
//! Resulting `.vm` are generated in a separate `./bin` directory, located
//! in the root directory, alongside the `./src` directory (unlike `JackCompiler`,
//! which outputs `.vm` files alongside the source `.jack` files).
//!
//! Since `Hack` only loads `.vm` files from a single directory,
//! resulting `.vm` files are "flattened" in a single directory, rather then
//! keeping the original (potentially nested) source file structure.
