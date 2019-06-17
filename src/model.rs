use std::collections::HashMap;

#[derive(Debug)]
pub struct Module<'a> {
    pub types: Vec<LabeledType<'a>>,
    pub imports: Vec<Import<'a>>,
    pub export: Option<Export<'a>>,
}

pub type Attrs<'a> = HashMap<&'a str, AttrValue>;

#[derive(Debug)]
pub enum AttrValue {
    Bool(bool),
    Int(i64),
    Str(String),
}

#[derive(Debug)]
pub struct LabeledType<'a> {
    pub label: Option<&'a str>,
    pub typ: Type<'a>,
    pub attrs: Option<Attrs<'a>>,
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
    pub attrs: Option<Attrs<'a>>,
}

#[derive(Debug)]
pub struct GlobalDecl<'a> {
    pub name: &'a str,
    pub typ: Type<'a>,
    pub attrs: Option<Attrs<'a>>,
}

#[derive(Debug)]
pub struct FuncDecl<'a> {
    pub name: &'a str,
    pub args: Vec<LabeledType<'a>>,
    pub result: Option<Type<'a>>,
    pub attrs: Option<Attrs<'a>>,
}
