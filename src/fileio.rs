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

pub mod input {
    use std::{
        fs, io,
        path::{Path, PathBuf},
    };

    pub struct SourceFile {
        src_path: PathBuf,
        file_path: PathBuf,
        content: String,
    }

    impl SourceFile {
        pub fn content(&self) -> &str {
            &self.content
        }

        /// Convert file's path to a Hack-compatible module name
        pub fn module_name(&self) -> String {
            // get the path from the ./src dir to the source file
            let module_path = self
                .file_path
                .strip_prefix(&self.src_path)
                .expect("source file path should start with the ./src path");

            module_path
                // remove extension, so we only have the dir/file names
                .with_extension("")
                // split into a series of directory/file names
                .components()
                // convert to ordinary strings
                // TODO: re-check string handling
                .map(|c| c.as_os_str().to_string_lossy().to_string())
                // concat names using "_" as a separator
                .collect::<Vec<_>>()
                .join("_")
        }
    }

    pub struct SourceDir {
        src_path: PathBuf,
        current_file_index: usize,
        source_file_paths: Vec<PathBuf>,
    }

    fn check_src_exists(root_path: &Path) -> io::Result<PathBuf> {
        let src_path = root_path.join("src");
        if src_path.is_dir() {
            Ok(src_path)
        } else {
            Err(io::Error::other(
                "provided root path does not contain a `src` directory",
            ))
        }
    }

    impl SourceDir {
        pub fn setup(root_path: &Path) -> io::Result<Self> {
            super::check_root_path(root_path)?;

            let src_path = check_src_exists(root_path)?;

            let mut source_file_paths = vec![];
            Self::collect_source_files(&src_path, &mut source_file_paths)?;

            Ok(Self {
                src_path,
                current_file_index: 0,
                source_file_paths,
            })
        }

        fn collect_source_files(dir: &PathBuf, collected: &mut Vec<PathBuf>) -> io::Result<()> {
            for entry in fs::read_dir(dir)? {
                let entry = entry?;
                let path = entry.path();

                // recursively visit nested directories
                if path.is_dir() {
                    Self::collect_source_files(&path, collected)?;
                }

                // only add `.jill` files
                if path.extension().map_or(false, |ext| ext == "jill") {
                    collected.push(path);
                }
            }

            Ok(())
        }
    }

    impl Iterator for SourceDir {
        type Item = (PathBuf, io::Result<SourceFile>);

        fn next(&mut self) -> Option<Self::Item> {
            // reached the end of the file list - no more files to load
            if self.current_file_index == self.source_file_paths.len() {
                return None;
            }

            // get the path to the current (first unread) file
            let file_path = self.source_file_paths[self.current_file_index].clone();

            // move index to next file (for next iteration)
            self.current_file_index += 1;

            // (try to) read the contents of the file
            let read_result = fs::read_to_string(&file_path);

            // on successful read, store content (and other metadata) to model
            let source_file_result = read_result.map(|content| SourceFile {
                src_path: self.src_path.clone(),
                file_path: file_path.clone(),
                content,
            });

            // keep as result (as any of the file reads could fail), handle on per-file basis
            // keep `file_path` separate to the action result so we can denote file at fault in case of failure
            Some((file_path, source_file_result))
        }
    }
}

pub mod output {
    use std::{
        fs, io,
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

    pub struct OutputGenerator {
        bin_path: PathBuf,
    }

    impl OutputGenerator {
        pub fn setup(root_path: &Path) -> io::Result<Self> {
            super::check_root_path(root_path)?;

            let bin_path = ensure_bin_dir_exists(root_path)?;

            Ok(Self { bin_path })
        }

        pub fn generate(&self, output_file: OutputFile) -> io::Result<()> {
            let file_path = self.bin_path.join(output_file.name).with_extension("vm");

            fs::write(file_path, output_file.content)
        }
    }

    fn ensure_bin_dir_exists(root_path: &Path) -> io::Result<PathBuf> {
        let bin_path = root_path.join("bin");
        if !bin_path.is_dir() {
            fs::create_dir(&bin_path)?;
        }

        Ok(bin_path)
    }
}
