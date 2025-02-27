pub mod base;
pub mod error;
pub mod position;
pub mod string;
pub mod syntax;

use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;
