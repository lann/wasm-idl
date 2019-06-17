use pest::Parser;
use pest_derive;

use super::model;

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
struct IdlParser;

type Pair<'a> = pest::iterators::Pair<'a, Rule>;

type ParseResult<T> = Result<T, pest::error::Error<Rule>>;

pub fn parse<'a>(input: &'a str) -> ParseResult<model::Module<'a>> {
    let main = IdlParser::parse(Rule::main, input)?.next().unwrap();

    let mut m = model::Module {
        types: vec![],
        imports: vec![],
        export: None,
    };

    for pair in main.into_inner() {
        match pair.as_rule() {
            Rule::type_def => m.types.push(parse_labeled_type(pair)?),
            Rule::export_block => m.export = Some(parse_export(pair)?),
            Rule::import_block => m.imports.push(parse_import(pair)?),
            Rule::EOI => (),
            _ => unreachable!(),
        }
    }

    Ok(m)
}

pub(crate) fn parse_builtin_types(
    input: &'static str,
) -> ParseResult<Vec<model::LabeledType<'static>>> {
    let main = IdlParser::parse(Rule::main, input)?.next().unwrap();

    let mut types: Vec<model::LabeledType> = vec![];
    for pair in main.into_inner() {
        match pair.as_rule() {
            Rule::type_def => types.push(parse_labeled_type(pair)?),
            Rule::EOI => (),
            _ => {
                return Err(new_parse_error(
                    pair,
                    "builtins should only contain type defs",
                ))
            }
        }
    }
    Ok(types)
}

fn parse_attrs<'a>(maybe_pair: Option<Pair<'a>>) -> ParseResult<Option<model::Attrs>> {
    if let Some(pair) = maybe_pair {
        let mut entries: Vec<(&str, model::AttrValue)> = vec![];
        for attr_pair in pair.into_inner() {
            let mut inner_pairs = attr_pair.into_inner();
            let name = inner_pairs.next().unwrap().as_str();
            let value = parse_attr_value(inner_pairs.next().unwrap())?;
            entries.push((name, value));
        }
        Ok(Some(entries.into_iter().collect()))
    } else {
        Ok(None)
    }
}

fn parse_attr_value(pair: Pair) -> ParseResult<model::AttrValue> {
    let value = match pair.as_rule() {
        Rule::boolean => model::AttrValue::Bool(pair.as_str() == "true"),
        Rule::integer => {
            let val: i64 = parse_integer(pair)?;
            model::AttrValue::Int(val)
        }
        Rule::string => {
            let val = parse_string(pair)?;
            model::AttrValue::Str(val)
        }
        _ => unreachable!(),
    };
    Ok(value)
}

fn parse_integer<F, E>(pair: Pair) -> ParseResult<F>
where
    F: std::str::FromStr<Err = E>,
    E: std::fmt::Display,
{
    pair.as_str()
        .parse()
        .map_err(|err| new_parse_error(pair, format!("failed to parse int: {}", err)))
}

fn parse_string(pair: Pair) -> ParseResult<String> {
    let mut pieces: Vec<String> = vec![];
    for inner_pair in pair.into_inner() {
        let s = inner_pair.as_str();
        match inner_pair.as_rule() {
            Rule::string_chars => pieces.push(s.to_string()),
            Rule::string_unicode_escape => {
                let code = u32::from_str_radix(s, 16).unwrap();
                if let Some(ch) = std::char::from_u32(code) {
                    pieces.push(ch.to_string());
                } else {
                    return Err(new_parse_error(inner_pair, "invalid unicode char"));
                }
            }
            Rule::string_special_escape => {
                pieces.push(
                    match s {
                        // These are the JSON escapes plus `'`.
                        "'" => "'",
                        "\"" => "\"",
                        "\\" => "\\",
                        "/" => "/",
                        "b" => "\x08",
                        "f" => "\x0c",
                        "n" => "\n",
                        "r" => "\r",
                        "t" => "\t",
                        _ => return Err(new_parse_error(inner_pair, "bad escape char")),
                    }
                    .to_string(),
                );
            }
            _ => unreachable!(),
        }
    }
    Ok(pieces.into_iter().collect())
}

fn parse_type<'a>(pair: Pair<'a>) -> ParseResult<model::Type<'a>> {
    Ok(match pair.as_rule() {
        Rule::ident => model::Type::Named(pair.as_str()),
        Rule::pointer_type => {
            let inner_type = parse_type(pair.into_inner().next().unwrap())?;
            model::Type::Pointer(Box::new(inner_type))
        }
        Rule::vector_type => {
            let inner_type = parse_type(pair.into_inner().next().unwrap())?;
            model::Type::Vector(Box::new(inner_type))
        }
        Rule::array_type => {
            let mut inner_pairs = pair.into_inner();
            let size: u32 = parse_integer(inner_pairs.next().unwrap())?;
            let inner_type = parse_type(inner_pairs.next().unwrap())?;
            model::Type::Array(size, Box::new(inner_type))
        }
        Rule::struct_type => {
            let mut fields: Vec<model::LabeledType> = vec![];
            for field_pair in pair.into_inner().next().unwrap().into_inner() {
                fields.push(parse_labeled_type(field_pair)?);
            }
            model::Type::Struct(fields)
        }
        _ => unreachable!(),
    })
}

fn parse_labeled_type<'a>(pair: Pair<'a>) -> ParseResult<model::LabeledType<'a>> {
    let mut inner_pairs: Vec<_> = pair.into_inner().collect();
    let mut last_pair = inner_pairs.pop().unwrap();
    let mut attrs: Option<model::Attrs> = None;
    if last_pair.as_rule() == Rule::attrs {
        attrs = parse_attrs(Some(last_pair))?;
        last_pair = inner_pairs.pop().unwrap();
    }
    let typ = parse_type(last_pair)?;
    let label = inner_pairs.pop().map(|pair| pair.as_str());
    Ok(model::LabeledType {
        label: label,
        typ: typ,
        attrs: attrs,
    })
}

fn parse_global<'a>(pair: Pair<'a>) -> ParseResult<model::GlobalDecl<'a>> {
    let mut inner_pairs = pair.into_inner();
    let name = inner_pairs.next().unwrap().as_str();
    let typ = parse_type(inner_pairs.next().unwrap())?;
    let attrs = parse_attrs(inner_pairs.next())?;
    Ok(model::GlobalDecl {
        name: name,
        typ: typ,
        attrs: attrs,
    })
}

fn parse_func<'a>(pair: Pair<'a>) -> ParseResult<model::FuncDecl<'a>> {
    let mut inner_pairs = pair.into_inner();
    let name = inner_pairs.next().unwrap().as_str();

    let mut args: Vec<model::LabeledType> = vec![];
    let mut result: Option<model::Type> = None;
    let mut attrs: Option<model::Attrs> = None;
    for inner_pair in inner_pairs {
        match inner_pair.as_rule() {
            Rule::func_args => {
                for arg_pair in inner_pair.into_inner() {
                    args.push(parse_labeled_type(arg_pair)?);
                }
            }
            Rule::attrs => {
                attrs = parse_attrs(Some(inner_pair))?;
            }
            // If it isn't func_args or attrs, it must be the result type.
            _ => {
                result = Some(parse_type(inner_pair)?);
            }
        }
    }
    Ok(model::FuncDecl {
        name: name,
        args: args,
        result: result,
        attrs: attrs,
    })
}

fn parse_block_body<'a>(
    pair: Pair<'a>,
) -> ParseResult<(Vec<model::GlobalDecl<'a>>, Vec<model::FuncDecl<'a>>)> {
    let mut globals: Vec<model::GlobalDecl> = vec![];
    let mut funcs: Vec<model::FuncDecl> = vec![];
    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::global_decl => globals.push(parse_global(inner_pair)?),
            Rule::func_decl => funcs.push(parse_func(inner_pair)?),
            _ => unreachable!(),
        }
    }
    Ok((globals, funcs))
}

fn parse_export<'a>(pair: Pair<'a>) -> ParseResult<model::Export<'a>> {
    let (globals, funcs) = parse_block_body(pair.into_inner().next().unwrap())?;
    Ok(model::Export {
        globals: globals,
        funcs: funcs,
    })
}

fn parse_import<'a>(pair: Pair<'a>) -> ParseResult<model::Import<'a>> {
    let mut inner_pairs = pair.into_inner();
    let name = inner_pairs.next().unwrap().as_str();
    let (globals, funcs) = parse_block_body(inner_pairs.next().unwrap())?;
    let attrs = parse_attrs(inner_pairs.next())?;
    Ok(model::Import {
        name: name,
        globals: globals,
        funcs: funcs,
        attrs: attrs,
    })
}

fn new_parse_error<M: Into<String>>(pair: Pair, message: M) -> pest::error::Error<Rule> {
    pest::error::Error::new_from_span(
        pest::error::ErrorVariant::CustomError {
            message: message.into(),
        },
        pair.as_span(),
    )
}
