use std::io;

use lurk_ff::LurkField;

use crate::{
  expr::Expr,
  ptr::Ptr,
  store::Store,
};

pub trait Write<F: LurkField> {
  fn fmt<W: io::Write>(&self, store: &Store<F>, w: &mut W) -> io::Result<()>;
  fn fmt_to_string(&self, store: &Store<F>) -> String {
    let mut out = Vec::new();
    self.fmt(store, &mut out).expect("preallocated");
    String::from_utf8(out).expect("I know it")
  }
}

// fn write_symbol<F: LurkField, W: io::Write>(
//  w: &mut W,
//  store: &Store<F>,
//  sym: &Sym,
//) -> io::Result<()> {
//  let package = &store.lurk_package;
//  let maybe_abbr = package.relative_abbreviation(sym);
//  let symbol_name = maybe_abbr.full_name();
//  write!(w, "{}", symbol_name)
//}
