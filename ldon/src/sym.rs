pub const SYM_MARKER: char = '.';
pub const KEYWORD_MARKER: char = ':';
pub const SYM_SEPARATOR: char = '.';
pub const ESCAPE_CHARS: &'static str = "(){}[]=,.:";

use std::fmt;

use lurk_ff::LurkField;

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Hash)]
pub enum Symbol {
  Sym(Vec<String>),
  Key(Vec<String>),
}

impl Symbol {
  pub fn path(&self) -> &Vec<String> {
    match self {
      Self::Sym(x) => x,
      Self::Key(x) => x,
    }
  }

  pub fn root_sym() -> Self { Self::Sym(vec![]) }

  pub fn root_key() -> Self { Self::Key(vec![]) }

  pub fn parse_unchecked<F: LurkField>(xs: &str) -> Self {
    Symbol::parse::<F>(xs).expect("invalid symbol")
  }

  pub fn parse<F: LurkField>(
    xs: &str,
  ) -> Result<Self, crate::parser::error::ParseError<crate::parser::Span, F>>
  {
    let res =
      crate::parser::syntax::parse_symbol()(crate::parser::Span::new(xs));
    use nom::Finish;
    res.finish().map(|(_, s)| s)
  }

  pub fn is_key(&self) -> bool { matches!(self, Self::Key(_)) }

  pub fn marker(&self) -> char {
    match self {
      Self::Sym(_) => SYM_MARKER,
      Self::Key(_) => KEYWORD_MARKER,
    }
  }

  pub fn is_root(&self) -> bool { self.path().len() == 0 }

  pub fn name(&self) -> String {
    let path = self.path();
    let l = path.len();
    path[l - 1].clone()
  }

  pub fn is_keyword(&self) -> bool { self.path().len() == 2 }

  // see https://github.com/sg16-unicode/sg16/issues/69
  pub fn whitespace() -> Vec<char> {
    vec![
      '\u{0009}', '\u{000A}', '\u{000B}', '\u{000C}', '\u{000D}', '\u{0020}',
      '\u{0085}', '\u{200E}', '\u{200F}', '\u{2028}', '\u{2029}', '\u{20A0}',
      '\u{1680}', '\u{2000}', '\u{2001}', '\u{2002}', '\u{2003}', '\u{2004}',
      '\u{2005}', '\u{2006}', '\u{2007}', '\u{2008}', '\u{2009}', '\u{200A}',
      '\u{202F}', '\u{205F}', '\u{3000}',
    ]
  }

  pub fn is_whitespace(c: char) -> bool {
    Self::whitespace().iter().any(|x| *x == c)
  }

  pub fn escape_symbol_element(xs: &str) -> String {
    let mut res = String::new();
    for x in xs.chars() {
      if ESCAPE_CHARS.chars().any(|c| c == x) {
        res.push_str(&format!("\\{}", x));
      }
      else if Self::is_whitespace(x) {
        res.push_str(&format!("{}", x.escape_unicode()));
      }
      else {
        res.push(x)
      }
    }
    res
  }

  pub fn print_root(&self) -> String { format!("_{}", self.marker()) }

  pub fn print_escape(&self) -> String {
    if self.is_root() {
      return self.print_root();
    }
    let mut res = String::new();
    let xs = self.path();
    if Self::sym_needs_marker(self) {
      res.push(self.marker())
    }
    res.push_str(&Self::escape_symbol_element(&xs[0]));
    for x in xs[1..].iter() {
      res.push(SYM_SEPARATOR);
      res.push_str(&Self::escape_symbol_element(&x));
    }
    res
  }

  pub fn sym_needs_marker(&self) -> bool {
    let xs = self.path();
    if self.is_root()
      || self.is_key()
      || xs[0].is_empty()
      || xs[0] == "_"
      || xs[0] == self.print_root()
    {
      return true;
    };
    let c = xs[0].chars().next().unwrap();
    "1234567890.:'[](){}=,\"\\".chars().any(|x| x == c)
      || char::is_whitespace(c)
      || char::is_control(c)
  }
}

#[cfg(feature = "test-utils")]
pub mod test_utils {

  use quickcheck::{
    Arbitrary,
    Gen,
  };

  use super::*;

  fn arbitrary_limb(g: &mut Gen) -> String {
    let num_chars = usize::arbitrary(g) % 5;
    let mut s = String::new();
    for _ in 0..num_chars {
      let c = char::arbitrary(g);
      if !char::is_whitespace(c) && c != '\\' {
        s.push(c);
      }
    }
    s
  }

  impl Arbitrary for Symbol {
    fn arbitrary(g: &mut Gen) -> Self {
      let num_syms = usize::arbitrary(g) % 3;
      let mut sym = Vec::new();
      for _ in 0..num_syms {
        let s = arbitrary_limb(g);
        sym.push(s);
      }
      if bool::arbitrary(g) {
        Self::Key(sym)
      }
      else {
        Self::Sym(sym)
      }
    }
  }
}

impl fmt::Display for Symbol {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.is_root() {
      write!(f, "_{}", self.marker())
    }
    else {
      write!(f, "{}", self.print_escape())
    }
  }
}
