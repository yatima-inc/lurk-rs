use std::collections::HashSet;

use crate::{
  lurksym,
  sym,
  sym::Symbol,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Package {
  pub symbols: HashSet<Symbol>,
}

pub const LURK_EXTERNAL_SYMBOL_NAMES: &[&str] = &[
  "atom",
  "begin",
  "car",
  "cdr",
  "char",
  "comm",
  "commit",
  "cons",
  "current-env",
  "emit",
  "eval",
  "eq",
  "hide",
  "if",
  "lambda",
  "let",
  "letrec",
  "nil",
  "num",
  "open",
  "quote",
  "secret",
  "strcons",
  "t",
  "_",
  "+",
  "-",
  "*",
  "/",
  "%",
  "=",
  "<",
  ">",
  "<=",
  ">=",
];

impl Package {
  pub fn empty() -> Self { Package { symbols: HashSet::default() } }

  pub fn new(new_symbols: Vec<Symbol>) -> Self {
    let mut symbols = HashSet::new();
    for sym in new_symbols {
      symbols.insert(sym);
    }
    // symbols.insert(sym!["lurk"]);
    for sym in LURK_EXTERNAL_SYMBOL_NAMES {
      symbols.insert(sym!["lurk", sym]);
    }
    Self { symbols }
  }

  pub fn add_symbol(&mut self, sym: Symbol) { self.symbols.insert(sym); }

  pub fn remove_symbol(&mut self, sym: &Symbol) { self.symbols.remove(sym); }

  pub fn get_symbol(&self, sym: &Symbol) -> Option<&Symbol> {
    self.symbols.get(sym)
  }

  pub fn common_prefix(parent: &Symbol, child: &Symbol) -> Option<Symbol> {
    match (parent, child) {
      (Symbol::Sym(parent_path), Symbol::Sym(child_path))
        if child_path.len() >= parent_path.len()
          && child_path.iter().zip(parent_path).all(|(a, b)| a == b) =>
      {
        Some(Symbol::Sym(child_path[parent_path.len()..].to_vec()))
      },
      (Symbol::Key(parent_path), Symbol::Key(child_path))
        if child_path.len() >= parent_path.len()
          && child_path.iter().zip(parent_path).all(|(a, b)| a == b) =>
      {
        Some(Symbol::Key(child_path[parent_path.len()..].to_vec()))
      },
      _ => return None,
    }
  }
}

impl Default for Package {
  fn default() -> Self { Self::new(vec![]) }
}
