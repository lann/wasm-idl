use super::parser;

const IDL: &str = r#"

// Integer types

type int8   i32 [ kind = 'int', int = 8,  signed = true ]
type int16  i32 [ kind = 'int', int = 16, signed = true ]
type int32  i32 [ kind = 'int', int = 32, signed = true ]
type int64  i64 [ kind = 'int', int = 64, signed = true ]
type uint8  i32 [ kind = 'int', int = 8,  signed = false ]
type uint16 i32 [ kind = 'int', int = 16, signed = false ]
type uint32 i32 [ kind = 'int', int = 32, signed = false ]
type uint64 i64 [ kind = 'int', int = 64, signed = false ]

// Float types

type float32 f32 [ kind = 'float', float = 32 ]
type float64 f64 [ kind = 'float', float = 64 ]

// Void type

// Void is a zero-sized type used a placeholder for e.g. Pointers.
type void [0]i32 [ kind = 'void' ]

// Memory types

// Memory address-sized integer (should become i64 for wasm64)
type size isize [ kind = 'size', signed = false ]

// Generic pointer; `*<type>` desugars into this with [ ..., pointer = "<type>" ]
type pointer isize [ kind = 'pointer', pointer = 'void' ]

// Vector types

// Generic Vector; `[]<type>` desugars into this with [ ..., vector = "<type>" ]
type vector struct {
  ptr pointer
  len size
} [ kind = 'vector', vector = 'void' ]

// Common vector types
type bytes  vector [ vector = 'uint8', encoding = 'none' ]
type string vector [ vector = 'uint8', encoding = 'utf8' ]
type json   vector [ vector = 'uint8', encoding = 'json' ]

"#;

pub fn parse_types() -> Vec<parser::model::LabeledType<'static>> {
    parser::parse_builtin_types(IDL)
}
