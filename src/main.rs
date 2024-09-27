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

    apply_post_compilation_generation(&output_generator, &mut program_context).unwrap();
}

fn apply_post_compilation_generation(
    output_generator: &OutputGenerator,
    program_context: &mut codegen::context::ProgramContext,
) -> Result<(), Box<dyn std::error::Error>> {
    use codegen::post_compilation;

    // globals initialization
    if let Some(globals_output) = post_compilation::globals::construct(program_context) {
        // output globals
        output_generator.generate(globals_output)?;

        // add a custom Sys.vm (which is modified to include a call to `Globals.init`)
        output_generator.generate(post_compilation::jillstd::sys_output())?;
    }

    // jillstd
    for jillstd_module_output in post_compilation::jillstd::construct(program_context) {
        output_generator.generate(jillstd_module_output)?;
    }

    // fn dispatch
    if let Some(fn_dispatch_output) =
        post_compilation::function_dispatch::construct(program_context)
    {
        output_generator.generate(fn_dispatch_output)?;
    }

    Ok(())
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
