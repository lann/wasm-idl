use pest::Parser;
use pest_derive;

use self::model::*;

pub mod model {
    use std::collections::HashMap;

    #[derive(Debug)]
    pub struct Module<'a> {
        pub types: Vec<LabeledType<'a>>,
        pub imports: Vec<Import<'a>>,
        pub export: Option<Export<'a>>,
    }

    pub type Attrs<'a> = Option<HashMap<&'a str, AttrValue<'a>>>;

    #[derive(Debug)]
    pub enum AttrValue<'a> {
        Bool(bool),
        Int(i64),
        Str(&'a str),
    }

    #[derive(Debug)]
    pub struct LabeledType<'a> {
        pub label: Option<&'a str>,
        pub typ: Type<'a>,
        pub attrs: Attrs<'a>,
    }

    #[derive(Debug)]
    pub enum Type<'a> {
        Named(&'a str),
        Pointer(Box<Type<'a>>),
        Vector(Box<Type<'a>>),
        Array(u32, Box<Type<'a>>),
        Struct(Vec<LabeledType<'a>>),
    }

    #[derive(Debug)]
    pub struct Export<'a> {
        pub globals: Vec<GlobalDecl<'a>>,
        pub funcs: Vec<FuncDecl<'a>>,
    }

    #[derive(Debug)]
    pub struct Import<'a> {
        pub name: &'a str,
        pub globals: Vec<GlobalDecl<'a>>,
        pub funcs: Vec<FuncDecl<'a>>,
        pub attrs: Attrs<'a>,
    }

    #[derive(Debug)]
    pub struct GlobalDecl<'a> {
        pub name: &'a str,
        pub typ: Type<'a>,
        pub attrs: Attrs<'a>,
    }

    #[derive(Debug)]
    pub struct FuncDecl<'a> {
        pub name: &'a str,
        pub args: Vec<LabeledType<'a>>,
        pub result: Option<Type<'a>>,
        pub attrs: Attrs<'a>,
    }
}

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
struct IdlParser;

type Pair<'a> = pest::iterators::Pair<'a, Rule>;

type ParseResult<'a> = Result<Module<'a>, pest::error::Error<Rule>>;

pub fn parse<'a>(input: &'a str) -> ParseResult<'a> {
    let main = IdlParser::parse(Rule::main, input)?
        .next().unwrap();

    let mut m = Module {
        types: vec!(),
        imports: vec!(),
        export: None,
    };

    for pair in main.into_inner() {
        match pair.as_rule() {
            Rule::type_def => m.types.push(parse_labeled_type(pair)),
            Rule::export_block => m.export = Some(parse_export(pair)),
            Rule::import_block => m.imports.push(parse_import(pair)),
            Rule::EOI => (),
            _ => unreachable!(),
        }
    }

    Ok(m)
}

pub(crate) fn parse_builtin_types(input: &'static str) -> Vec<LabeledType<'static>> {
    let inner_pairs = IdlParser::parse(Rule::main, input)
        .expect("failed parsing builtins") // TODO: print errors
        .next().unwrap().into_inner();
    inner_pairs.filter_map(|pair| {
        match pair.as_rule() {
            Rule::type_def => Some(parse_labeled_type(pair)),
            Rule::EOI => None,
            _ => panic!("builtins should only contain type defs"),
        }
    }).collect()
}

fn parse_attrs<'a>(maybe_pair: Option<Pair<'a>>) -> Attrs {
    maybe_pair.map(|pair| {
        pair.into_inner().map(|attr_pair| {
            let mut inner_pairs = attr_pair.into_inner();
            let name = inner_pairs.next().unwrap().as_str();
            let value_pair = inner_pairs.next().unwrap();
            let value = match value_pair.as_rule() {
                Rule::boolean => AttrValue::Bool(value_pair.as_str() == "true"),
                Rule::integer => AttrValue::Int(value_pair.as_str().parse().expect("error parsing int")),
                Rule::string => AttrValue::Str(parse_string(value_pair)),
                _ => unreachable!(),
            };
            (name, value)
        }).collect()
    })
}

fn parse_string<'a>(pair: Pair<'a>) -> &'a str {
    let raw = pair.as_str();
    // TODO: escapes
    &raw[1..raw.len()-1]
}

fn parse_type<'a>(pair: Pair<'a>) -> Type<'a> {
    match pair.as_rule() {
        Rule::ident => Type::Named(pair.as_str()),
        Rule::pointer_type => {
            let inner_type = parse_type(pair.into_inner().next().unwrap());
            Type::Pointer(Box::new(inner_type))
        }
        Rule::vector_type => {
            let inner_type = parse_type(pair.into_inner().next().unwrap());
            Type::Vector(Box::new(inner_type))
        }
        Rule::array_type => {
            let mut inner_pairs = pair.into_inner();
            let size: u32 = inner_pairs.next().unwrap().as_str().parse().expect("failed parsing int");
            let inner_type = parse_type(inner_pairs.next().unwrap());
            Type::Array(size, Box::new(inner_type))
        }
        Rule::struct_type => {
            let field_pairs = pair.into_inner().next().unwrap().into_inner();
            Type::Struct(field_pairs.map(parse_labeled_type).collect())
        }
        _ => unreachable!(),
    }
}

fn parse_labeled_type<'a>(pair: Pair<'a>) -> LabeledType<'a> {
    let mut inner_pairs: Vec<_> = pair.into_inner().collect();
    let mut last_pair = inner_pairs.pop().unwrap();
    let mut attrs: Attrs = None;
    if last_pair.as_rule() == Rule::attrs {

        attrs = parse_attrs(Some(last_pair));
        last_pair = inner_pairs.pop().unwrap();
    }
    let typ = parse_type(last_pair);
    let label = inner_pairs.pop().map(|pair| pair.as_str());
    LabeledType { label: label, typ: typ, attrs: attrs }
}

fn parse_global<'a>(pair: Pair<'a>) -> GlobalDecl<'a> {
    let mut inner_pairs = pair.into_inner();
    let name = inner_pairs.next().unwrap().as_str();
    let typ = parse_type(inner_pairs.next().unwrap());
    let attrs = parse_attrs(inner_pairs.next());
    GlobalDecl { name: name, typ: typ, attrs: attrs }
}

fn parse_func<'a>(pair: Pair<'a>) -> FuncDecl<'a> {
    let mut inner_pairs = pair.into_inner();
    let name = inner_pairs.next().unwrap().as_str();
    
    let mut args: Vec<LabeledType> = vec!();
    let mut result: Option<Type> = None;
    let mut attrs: Attrs = None;
    for inner_pair in inner_pairs {
        match inner_pair.as_rule() {
            Rule::func_args => {
                args = inner_pair.into_inner().map(parse_labeled_type).collect();
            }
            Rule::attrs => {
                attrs = parse_attrs(Some(inner_pair));
            }
            // If it isn't func_args or attrs, it must be the result type.
            _ => {
                result = Some(parse_type(inner_pair));
            }
        }
    }
    FuncDecl { name: name, args: args, result: result, attrs: attrs }
}

fn parse_block_body<'a>(pair: Pair<'a>) -> (Vec<GlobalDecl<'a>>, Vec<FuncDecl<'a>>) {
    let mut globals: Vec<GlobalDecl> = vec!();
    let mut funcs: Vec<FuncDecl> = vec!();
    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::global_decl => globals.push(parse_global(inner_pair)),
            Rule::func_decl => funcs.push(parse_func(inner_pair)),
            _ => unreachable!(),
        }
    }
    (globals, funcs)
}

fn parse_export<'a>(pair: Pair<'a>) -> Export<'a> {
    let (globals, funcs) = parse_block_body(pair.into_inner().next().unwrap());
    Export { globals: globals, funcs: funcs }
}

fn parse_import<'a>(pair: Pair<'a>) -> Import<'a> {
    let mut inner_pairs = pair.into_inner();
    let name = inner_pairs.next().unwrap().as_str();
    let (globals, funcs) = parse_block_body(inner_pairs.next().unwrap());
    let attrs = parse_attrs(inner_pairs.next());
    Import { name: name, globals: globals, funcs: funcs, attrs: attrs }
}

