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

// TODO: try to split into separate functions
#[allow(clippy::too_many_lines)]
/// Construct the parser for a Jill program module (file).
fn parser() -> impl Parser<char, JillModuleContent, Error = JillParseError> {
    let positive_integer =
        text::int(10).map(|s: String| s.parse::<isize>().expect("should be a valid integer"));

    let negation_sign = just('-').or_not();
    let integer = negation_sign
        .then(positive_integer)
        .map(|(sign, number)| if sign.is_some() { -number } else { number })
        .map(JillLiteral::Integer);

    let string = just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .map(|chars| chars.into_iter().collect())
        .map(JillLiteral::String);

    let identifier = text::ident().padded().map(JillIdentifier);

    let comment = just("--").then(take_until(text::newline()));
    let comments = comment.repeated().padded().ignored();

    let expression = recursive(|expression| {
        let literal = integer.or(string).map(JillExpression::Literal);

        let variable_name = identifier;

        // ( :: | ( mod_name:: )+ )
        let local_module_path = just("::").to(Vec::new());
        let modules_path = identifier
            .then_ignore(just("::"))
            .repeated()
            .at_least(1)
            .or(local_module_path);

        let associated_type = identifier.then_ignore(just(":"));
        let function_name = identifier;
        let function_reference = modules_path
            .then(associated_type.or_not())
            .then(function_name)
            .map(
                |((modules_path, associated_type), function_name)| JillFunctionReference {
                    modules_path,
                    associated_type,
                    function_name,
                },
            );

        let function_call = {
            let function_call_source =
                function_reference
                    .clone()
                    .or(variable_name.map(|name| JillFunctionReference {
                        modules_path: vec![],
                        associated_type: None,
                        function_name: name,
                    }));

            function_call_source
                .then(
                    expression
                        .clone()
                        .separated_by(just(','))
                        .allow_trailing()
                        .delimited_by(just('('), just(')')),
                )
                .map(|(reference, arguments)| JillFunctionCall {
                    reference,
                    arguments,
                })
        };

        // since there is possible ambiguity here, order in which the
        // options are listed is important (most specific => least specific)
        literal
            .or(function_call.map(JillExpression::FunctionCall))
            .or(function_reference.map(JillExpression::FunctionReference))
            .or(variable_name.map(JillExpression::VariableName))
            .padded()
    });

    let variable = text::keyword("let")
        .ignore_then(identifier)
        .then_ignore(just('='))
        .then(expression.clone())
        .padded()
        .padded_by(comments)
        .map(|(name, value)| JillVariable { name, value });

    let function = recursive(|function| {
        let function_body = function
            .then_ignore(just('.'))
            .repeated()
            .then(variable.clone().then_ignore(just(',')).repeated())
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
            .then(identifier.repeated())
            .then_ignore(just('='))
            .then(function_body)
            .padded()
            .padded_by(comments)
            .map(|((name, arguments), body)| JillFunction {
                name,
                arguments,
                body,
            })
    });

    let variant = identifier
        .then(
            identifier
                .separated_by(just(','))
                .allow_trailing()
                .delimited_by(just('('), just(')'))
                .or_not(),
        )
        .map(|(name, arguments)| JillTypeVariant {
            name,
            arguments: arguments.unwrap_or(Vec::new()),
        });

    let r#type = text::keyword("type")
        .ignore_then(identifier)
        .then_ignore(just('='))
        .then(variant.separated_by(just(',')))
        .padded()
        .padded_by(comments)
        .map(|(name, variants)| JillType { name, variants });

    let module = r#type
        .then_ignore(just('.'))
        .repeated()
        .then(variable.then_ignore(just('.')).repeated())
        .then(function.then_ignore(just('.')).repeated())
        .padded()
        .map(|((types, variables), functions)| JillModuleContent {
            types,
            variables,
            functions,
        });

    module.then_ignore(end())
}
