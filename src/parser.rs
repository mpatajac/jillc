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
    let module = r#type()
        .then_ignore(just('.'))
        .repeated()
        .then(variable().then_ignore(just('.')).repeated())
        .then(function().then_ignore(just('.')).repeated())
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
    // NOTE: since this is a part of the (recursive) `expression` parser definition,
    // we have to directly pass expression parser
    expression: impl Parser<char, JillExpression, Error = JillParseError>,
) -> impl Parser<char, Vec<JillExpression>, Error = JillParseError> {
    expression.separated_by(just(',')).allow_trailing().padded()
}

type JillFunctionReferenceComponents = (
    (Vec<JillIdentifier>, Option<JillIdentifier>),
    JillIdentifier,
);
fn fully_qualified_function_name(
    // NOTE: passed as this function is used for both function calls and references,
    // where different parsing rules should apply
    function_name_parser: impl Parser<char, JillIdentifier, Error = JillParseError>,
) -> impl Parser<char, JillFunctionReferenceComponents, Error = JillParseError> {
    let modules_path = identifier::module().then_ignore(just("::")).repeated();

    let associated_type = identifier::r#type().then_ignore(just(":"));
    let function_name = function_name_parser;

    modules_path
        .then(associated_type.or_not())
        .then(function_name)
}

fn literal(
    // NOTE: since this is a part of the (recursive) `expression` parser definition,
    // we have to directly pass expression parser
    expression: impl Parser<char, JillExpression, Error = JillParseError>,
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

fn function_reference() -> impl Parser<char, JillFunctionReference, Error = JillParseError> {
    just("&")
        .ignore_then(fully_qualified_function_name(
            identifier::function_reference(),
        ))
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
    // NOTE: since this is a part of the (recursive) `expression` parser definition,
    // we have to directly pass expression parser
    expression: impl Parser<char, JillExpression, Error = JillParseError>,
) -> impl Parser<char, JillFunctionCall, Error = JillParseError> {
    let function_call_source = fully_qualified_function_name(identifier::function_call())
        .or(identifier::function_call().map(|name| ((vec![], None), name)))
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

fn expression() -> impl Parser<char, JillExpression, Error = JillParseError> {
    recursive(|expression| {
        // since there is possible ambiguity here, order in which the
        // options are listed is important (most specific => least specific)
        literal(expression.clone())
            .map(JillExpression::Literal)
            .or(function_call(expression).map(JillExpression::FunctionCall))
            .or(function_reference().map(JillExpression::FunctionReference))
            .or(identifier::variable().map(JillExpression::VariableName))
            .padded()
            .padded_by(comments())
    })
}

fn function_body(
    nested_function: impl Parser<char, JillFunction, Error = JillParseError>,
) -> impl Parser<char, JillFunctionBody, Error = JillParseError> {
    nested_function
        .then_ignore(just('.'))
        .repeated()
        .then(variable().then_ignore(just(',')).repeated())
        .then(expression())
        .padded()
        .map(
            |((local_functions, local_variables), return_expression)| JillFunctionBody {
                local_functions,
                local_variables,
                return_expression,
            },
        )
}

fn variable() -> impl Parser<char, JillVariable, Error = JillParseError> {
    text::keyword("let")
        .ignore_then(identifier::variable_assignment())
        .then_ignore(just('='))
        .then(expression())
        .padded()
        .padded_by(comments())
        .map(|(name, value)| JillVariable { name, value })
}

fn function() -> impl Parser<char, JillFunction, Error = JillParseError> {
    let captured_arguments = identifier::variable()
        .repeated()
        // require at least one captured argument if brackets are present
        .at_least(1)
        .delimited_by(just('['), just(']'))
        .padded();

    recursive(|function| {
        text::keyword("fn")
            .ignore_then(identifier::function_name())
            .then(identifier::variable().repeated())
            .then(captured_arguments.or_not())
            .then_ignore(just('='))
            .then(function_body(function))
            .padded()
            .padded_by(comments())
            .map(|(((name, arguments), captures), body)| JillFunction {
                name,
                arguments,
                captures: captures.unwrap_or(vec![]),
                body,
            })
    })
}

fn r#type() -> impl Parser<char, JillType, Error = JillParseError> {
    let variant = identifier::r#type()
        .then(
            identifier::variable()
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
        .ignore_then(identifier::r#type())
        .then_ignore(just('='))
        .then(variant.separated_by(just(',')))
        .padded()
        .padded_by(comments())
        .map(|(name, variants)| JillType { name, variants })
}

mod identifier {
    // TODO!: figure out how to "forbid" certain identifiers

    use super::*;

    fn keyword() -> impl Parser<char, (), Error = JillParseError> {
        choice([
            text::keyword("let"),
            text::keyword("fn"),
            text::keyword("type"),
        ])
    }

    fn compiler_internal() -> impl Parser<char, (), Error = JillParseError> {
        choice([
            text::keyword("if"),
            text::keyword("ifElse"),
            text::keyword("do"),
            text::keyword("match"),
            text::keyword("todo"),
            text::keyword("free"),
        ])
    }

    fn underscore() -> impl Parser<char, (), Error = JillParseError> {
        text::keyword("_")
    }

    // ({identifier} \ `_`) \ {keyword}
    fn non_keyword_identifier() -> impl Parser<char, String, Error = JillParseError> {
        // forbid all keywords, as they are never a valid identifier
        let not_keyword = keyword().not().rewind();

        // also forbid isolated `_`, as it is not a valid identifier
        let not_underscore = underscore().not().rewind();

        let base_identifier = text::ident().padded();

        not_keyword.ignore_then(not_underscore.ignore_then(base_identifier))
    }

    pub fn function_call() -> impl Parser<char, JillIdentifier, Error = JillParseError> {
        // function call is the only place where we should accept
        // compiler internal (i.e. lazy evaluated) functions
        non_keyword_identifier().map(JillIdentifier)
    }

    // (({identifier} \ `_`) \ {keyword}) \ {compiler_internal}
    fn non_compiler_internal_identifier() -> impl Parser<char, String, Error = JillParseError> {
        let not_compiler_internal = compiler_internal().not().rewind();

        not_compiler_internal.ignore_then(non_keyword_identifier())
    }

    macro_rules! make_non_compiler_internal_identifier {
        ($name: ident) => {
            pub fn $name() -> impl Parser<char, JillIdentifier, Error = JillParseError> {
                non_compiler_internal_identifier().map(JillIdentifier)
            }
        };
    }

    make_non_compiler_internal_identifier!(function_reference);
    make_non_compiler_internal_identifier!(function_name);
    make_non_compiler_internal_identifier!(module);
    make_non_compiler_internal_identifier!(r#type);
    make_non_compiler_internal_identifier!(variable);

    pub fn variable_assignment() -> impl Parser<char, JillIdentifier, Error = JillParseError> {
        // non-internal, but allows pure `_`
        non_compiler_internal_identifier()
            .or(underscore().map(|()| String::new()))
            .map(JillIdentifier)
    }
}
