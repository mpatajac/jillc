//! Logic for parsing raw `Jill` code
//! into an AST.

#[allow(clippy::wildcard_imports)]
use crate::common::ast::*;
use chumsky::prelude::*;

use crate::fileio::input::SourceFile;

// TODO?: move to error handling
type JillParseError = Simple<char>;

/// Parse a single module (source file).
pub fn parse_module(source_file: &SourceFile) -> Result<JillModule, Vec<JillParseError>> {
    let module_name = source_file.module_name();
    let module_content = parser().parse(source_file.content())?;

    Ok(JillModule {
        name: module_name,
        content: module_content,
    })
}

/// Construct the parser for a Jill program module (file).
fn parser() -> impl Parser<char, JillModuleContent, Error = JillParseError> {
    let integer = text::int(10)
        .map(|s: String| s.parse().expect("should be a valid integer"))
        .map(JillLiteral::Integer);

    let string = just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .map(|chars| chars.into_iter().collect())
        .map(JillLiteral::String);

    let literal = integer.or(string).padded().map(JillExpression::Literal);

    let expression = literal;

    let identifier = text::ident().padded();

    let variable = text::keyword("let")
        .ignore_then(identifier)
        .map(|name| JillIdentifier { value: name })
        .then_ignore(just('='))
        .then(expression.clone())
        .padded()
        .map(|(name, value)| JillVariable { name, value });

    let function = recursive(|function| {
        let function_body = function
            .separated_by(just('.'))
            .allow_trailing()
            .padded()
            .then(
                variable
                    .clone()
                    .separated_by(just(','))
                    .allow_trailing()
                    .padded(),
            )
            .then(expression)
            .padded()
            .map(
                |((local_functions, local_variables), return_expression)| JillFunctionBody {
                    local_functions,
                    local_variables,
                    return_expression,
                },
            );

        text::keyword("fn")
            .ignore_then(identifier)
            .map(|name| JillIdentifier { value: name })
            .then(identifier.repeated())
            .map(|(name, args)| (name, JillIdentifier::map_vec(args)))
            .then_ignore(just('='))
            .then(function_body)
            .padded()
            .map(|((name, arguments), body)| JillFunction {
                name,
                arguments,
                body,
            })
    });

    let module = variable
        .separated_by(just('.'))
        .allow_trailing()
        .then(function.separated_by(just('.')).allow_trailing())
        .padded()
        .map(|(variables, functions)| JillModuleContent {
            variables,
            functions,
        });

    // TODO: figure out how exactly this needs to be set
    module.then_ignore(end())
}
