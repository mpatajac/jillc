//! Logic for parsing raw `Jill` code
//! into an AST.

#[allow(clippy::wildcard_imports)]
use crate::common::ast::*;
use chumsky::prelude::*;

use crate::fileio::input::SourceFile;

// TODO?: move to error handling
pub type JillParseError = Simple<char>;

/// Parse a single module (source file).
pub fn parse_module(source_file: &SourceFile) -> Result<JillModule, Vec<JillParseError>> {
    let module_name = source_file.module_name();
    let module_content = module().parse(source_file.content())?;

    Ok(JillModule {
        name: module_name,
        content: module_content,
    })
}

/// Construct the parser for a Jill program module (file).
fn module() -> impl Parser<char, JillModuleContent, Error = JillParseError> {
    let identifier = text::ident().padded().map(JillIdentifier);

    let expression = recursive(|expression| {
        let variable_name = identifier;

        // since there is possible ambiguity here, order in which the
        // options are listed is important (most specific => least specific)
        literal(expression.clone())
            .map(JillExpression::Literal)
            .or(function_call(identifier, expression, variable_name)
                .map(JillExpression::FunctionCall))
            .or(function_reference(identifier).map(JillExpression::FunctionReference))
            .or(variable_name.map(JillExpression::VariableName))
            .padded()
    });

    let function = recursive(|function| {
        text::keyword("fn")
            .ignore_then(identifier)
            .then(identifier.repeated())
            .then_ignore(just('='))
            .then(function_body(identifier, expression.clone(), function))
            .padded()
            .padded_by(comments())
            .map(|((name, arguments), body)| JillFunction {
                name,
                arguments,
                body,
            })
    });

    let module = r#type(identifier)
        .then_ignore(just('.'))
        .repeated()
        .then(
            variable(identifier.clone(), expression.clone())
                .then_ignore(just('.'))
                .repeated(),
        )
        .then(function.then_ignore(just('.')).repeated())
        .padded()
        .map(|((types, variables), functions)| JillModuleContent {
            types,
            variables,
            functions,
        });

    module.then_ignore(end())
}

fn comments() -> impl Parser<char, (), Error = JillParseError> + std::clone::Clone {
    let comment = just("--").then(take_until(text::newline()));
    comment.padded().repeated().ignored()
}

fn nested_expression_list(
    expression: impl Parser<char, JillExpression, Error = JillParseError>,
) -> impl Parser<char, Vec<JillExpression>, Error = JillParseError> {
    expression.separated_by(just(',')).allow_trailing().padded()
}

type JillFunctionReferenceComponents = (
    (Vec<JillIdentifier>, Option<JillIdentifier>),
    JillIdentifier,
);
fn fully_qualified_function_name(
    identifier: impl Parser<char, JillIdentifier, Error = JillParseError> + std::clone::Clone,
) -> impl Parser<char, JillFunctionReferenceComponents, Error = JillParseError> {
    let modules_path = identifier.clone().then_ignore(just("::")).repeated();

    let associated_type = identifier.clone().then_ignore(just(":"));
    let function_name = identifier;

    modules_path
        .then(associated_type.or_not())
        .then(function_name)
}

fn literal(
    expression: impl Parser<char, JillExpression, Error = JillParseError> + std::clone::Clone,
) -> impl Parser<char, JillLiteral, Error = JillParseError> {
    // integer
    let number =
        text::int(10).map(|s: String| s.parse::<isize>().expect("should be a valid number"));

    let negation_sign = just('-').or_not();
    let integer = negation_sign
        .then(number)
        .map(|(sign, number)| if sign.is_some() { -number } else { number })
        .map(JillLiteral::Integer);

    // string
    let string = just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .map(|chars| chars.into_iter().collect())
        .map(JillLiteral::String);

    // bool
    let boolean = just("True")
        .or(just("False"))
        .map(|b| b == "True")
        .map(JillLiteral::Bool);

    // list
    let list = nested_expression_list(expression)
        .delimited_by(just('['), just(']'))
        .map(JillLiteral::List);

    integer.or(string).or(boolean).or(list)
}

fn function_reference(
    identifier: impl Parser<char, JillIdentifier, Error = JillParseError> + std::clone::Clone,
) -> impl Parser<char, JillFunctionReference, Error = JillParseError> {
    just("&")
        .ignore_then(fully_qualified_function_name(identifier))
        .then_ignore(
            none_of("(")
                .rewind()
                .labelled("cannot reference and call a function"),
        )
        .map(
            |((modules_path, associated_type), function_name)| JillFunctionReference {
                modules_path,
                associated_type,
                function_name,
            },
        )
}

fn function_call(
    identifier: impl Parser<char, JillIdentifier, Error = JillParseError> + std::clone::Clone,
    expression: impl Parser<char, JillExpression, Error = JillParseError>,
    variable_name: impl Parser<char, JillIdentifier, Error = JillParseError>,
) -> impl Parser<char, JillFunctionCall, Error = JillParseError> {
    let function_call_source = fully_qualified_function_name(identifier)
        .or(variable_name.map(|name| ((vec![], None), name)))
        .map(
            |((modules_path, associated_type), function_name)| JillFunctionReference {
                modules_path,
                associated_type,
                function_name,
            },
        );

    function_call_source
        .then(nested_expression_list(expression).delimited_by(just('('), just(')')))
        .map(|(reference, arguments)| JillFunctionCall {
            reference,
            arguments,
        })
}

fn function_body(
    identifier: impl Parser<char, JillIdentifier, Error = JillParseError> + std::clone::Clone,
    expression: impl Parser<char, JillExpression, Error = JillParseError> + std::clone::Clone,
    nested_function: impl Parser<char, JillFunction, Error = JillParseError>,
) -> impl Parser<char, JillFunctionBody, Error = JillParseError> {
    nested_function
        .then_ignore(just('.'))
        .repeated()
        .then(
            variable(identifier, expression.clone())
                .clone()
                .then_ignore(just(','))
                .repeated(),
        )
        .then(expression)
        .padded()
        .map(
            |((local_functions, local_variables), return_expression)| JillFunctionBody {
                local_functions,
                local_variables,
                return_expression,
            },
        )
}

fn variable(
    identifier: impl Parser<char, JillIdentifier, Error = JillParseError> + std::clone::Clone,
    expression: impl Parser<char, JillExpression, Error = JillParseError> + std::clone::Clone,
) -> impl Parser<char, JillVariable, Error = JillParseError> + std::clone::Clone {
    text::keyword("let")
        .ignore_then(identifier)
        .then_ignore(just('='))
        .then(expression)
        .padded()
        .padded_by(comments())
        .map(|(name, value)| JillVariable { name, value })
}

fn r#type(
    identifier: impl Parser<char, JillIdentifier, Error = JillParseError> + std::clone::Clone,
) -> impl Parser<char, JillType, Error = JillParseError> {
    let variant = identifier
        .clone()
        .then(
            identifier
                .clone()
                .separated_by(just(','))
                .allow_trailing()
                .delimited_by(just('('), just(')'))
                .or_not(),
        )
        .map(|(name, fields)| JillTypeVariant {
            name,
            fields: fields.unwrap_or(Vec::new()),
        });

    text::keyword("type")
        .ignore_then(identifier)
        .then_ignore(just('='))
        .then(variant.separated_by(just(',')))
        .padded()
        .padded_by(comments())
        .map(|(name, variants)| JillType { name, variants })
}
