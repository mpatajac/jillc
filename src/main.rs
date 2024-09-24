use fileio::{input::SourceDir, output::OutputGenerator};
use std::path::Path;

mod codegen;
mod common;
mod fileio;
mod parser;

fn main() {
    let root_path = Path::new("./tmp");
    let source_dir = SourceDir::setup(root_path).unwrap();
    let output_generator = OutputGenerator::setup(root_path).unwrap();

    let mut program_context = codegen::context::ProgramContext::new();

    for (file_path, file) in source_dir {
        match file {
            Err(error) => eprintln!("Unable to load file at `{file_path:#?}`: {error}"),
            Ok(file_info) => match parser::parse_module(&file_info) {
                Ok(ast) => {
                    // dbg!(&ast);
                    match codegen::construct_module(ast, &mut program_context) {
                        Ok(output_file) => output_generator.generate(output_file).unwrap(),
                        Err(error) => eprintln!("{error:#?}"),
                    };
                }
                Err(errors) => error_report::display(
                    file_path.to_string_lossy().as_ref(),
                    file_info.content(),
                    errors,
                ),
            },
        }
    }
}

mod error_report {
    use ariadne::{Label, Report, ReportKind, Source};

    use crate::parser::JillParseError;

    pub fn display(file_path: &str, file_content: &str, errors: Vec<JillParseError>) {
        for error in errors {
            Report::build(ReportKind::Error, file_path, error.span().start)
                .with_message("Parser error")
                .with_label(
                    Label::new((file_path, error.span()))
                        .with_message(error.label().unwrap_or("error occured here")),
                )
                .finish()
                .eprint((file_path, Source::from(file_content)))
                .expect("error report should be valid");
        }
    }
}
