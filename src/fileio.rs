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

use std::{io, path::Path};

/// Checks that the provided path to the root of the project directory
/// is in fact a valid path to an existing directory.
fn check_root_path(root_path: &Path) -> io::Result<()> {
    if root_path.is_dir() {
        Ok(())
    } else {
        Err(io::Error::other(
            "provided root path is not a valid directory",
        ))
    }
}

pub mod output {
    use std::{
        fs,
        io::{self, Write},
        path::{Path, PathBuf},
    };

    pub struct OutputFile {
        name: String,
        content: String,
    }

    impl OutputFile {
        pub const fn new(name: String, content: String) -> Self {
            Self { name, content }
        }
    }

    pub fn generate(root_path: &Path, output_file: OutputFile) -> io::Result<()> {
        super::check_root_path(root_path)?;

        let bin_path = ensure_bin_dir_exists(root_path)?;
        let file_path = bin_path.join(output_file.name).with_extension("vm");

        fs::File::create(file_path)?.write_all(output_file.content.as_bytes())
    }

    fn ensure_bin_dir_exists(root_path: &Path) -> io::Result<PathBuf> {
        let bin_path = root_path.join("bin");
        if !bin_path.is_dir() {
            fs::create_dir(&bin_path)?;
        }

        Ok(bin_path)
    }
}
