#![feature(unchecked_math)]
#![allow(clippy::single_match, clippy::type_complexity)]

extern crate core;

#[allow(unused_imports)]
#[macro_use]
extern crate alloc;

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[allow(unused_imports)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

// pub mod circuit;
pub mod eval;
// pub mod proof;
// pub mod repl;
pub mod error;
pub mod expr;
pub mod num;
pub mod ptr;
pub mod store;
pub mod uint;
// pub mod sym;
pub mod writer;

// pub use num::Num;
// pub use sym::{Sym, Symbol};
// pub use uint::UInt;

pub const TEST_SEED: [u8; 16] = [
  0x62, 0x59, 0x5d, 0xbe, 0x3d, 0x76, 0x3d, 0x8d, 0xdb, 0x17, 0x32, 0x37, 0x06,
  0x54, 0xe5, 0xbc,
];
