use std::fmt;

use lurk_ff::field::LurkField;

use crate::{
  hash::PoseidonCache,
  lurksym,
  parser::position::Pos,
  store::Store,
  sym::Symbol,
};

// LDON syntax
#[derive(Clone, Debug)]
pub enum Syn<F: LurkField> {
  // A field element: 1, 0xff
  Num(Pos, F),
  // A u64 integer: 1u64, 0xffu64
  U64(Pos, u64),
  // A hierarchical symbol: foo, foo.bar.baz
  Symbol(Pos, Symbol),
  // A string literal: "foobar", "foo\nbar"
  String(Pos, String),
  // A character literal: 'a', 'b', '\n'
  Char(Pos, char),
  // A cons-list of expressions, which can be terminated by nil: (1 2 3)
  // or can be terminated with the right-most expression (1, 2, 3)
  List(Pos, Vec<Syn<F>>, Box<Syn<F>>),
  // A map of expressions to expressions: { foo = 1, blue = true, 3 = 4 }
  Map(Pos, Vec<(Syn<F>, Syn<F>)>),
  // A contextual link or descriptor of some piece of foreign data:
  // [sha256 0xffff_ffff_ffff_ffff 0xffff_ffff_ffff_ffff 0xffff_ffff_ffff_ffff
  // 0xffff_ffff_ffff_ffff]
  Link(Pos, Box<Syn<F>>, Vec<u64>),
}

impl<F: LurkField> Syn<F> {
  // Syn's Ord impl has bad asymptotics since it generates a fresh poseidon
  // cache. In those cases, `cached_cmp` allows for cache preserving
  // comparisons
  pub fn cached_cmp(
    &self,
    other: &Self,
    cache: &PoseidonCache<F>,
  ) -> core::cmp::Ordering {
    let mut store = Store::default();
    let self_ptr = store.insert_syn(cache, &self);
    let other_ptr = store.insert_syn(cache, &other);
    self_ptr.cmp(&other_ptr)
  }

  pub fn parse(
    xs: &str,
  ) -> Result<Self, crate::parser::error::ParseError<crate::parser::Span, F>>
  {
    let res = crate::parser::syntax::parse_syn()(crate::parser::Span::new(xs));
    use nom::Finish;
    res.finish().map(|(_, s)| s)
  }
}

impl<F: LurkField> PartialOrd for Syn<F> {
  fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
    Some(self.cached_cmp(other, &PoseidonCache::default()))
  }
}

impl<F: LurkField> Ord for Syn<F> {
  fn cmp(&self, other: &Self) -> core::cmp::Ordering {
    self.cached_cmp(other, &PoseidonCache::default())
  }
}

impl<F: LurkField> fmt::Display for Syn<F> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Num(_, x) => {
        write!(f, "0x{}", x.hex_digits())?;
        Ok(())
      },
      Self::U64(_, x) => write!(f, "{}u64", x),
      Self::Symbol(_, sym) => write!(f, "{}", sym.print_escape()),
      Self::String(_, x) => write!(f, "\"{}\"", x.escape_default()),
      Self::Char(_, x) => write!(f, "'{}'", x.escape_default()),
      Self::List(_, xs, end)
        if *end == Box::new(Syn::Symbol(Pos::No, lurksym!["nil"])) =>
      {
        let mut iter = xs.iter().peekable();
        write!(f, "(")?;
        while let Some(x) = iter.next() {
          match iter.peek() {
            Some(_) => write!(f, "{} ", x)?,
            None => write!(f, "{}", x)?,
          }
        }
        write!(f, ")")
      },
      Self::List(_, xs, end) => {
        let mut iter = xs.iter().peekable();
        write!(f, "(")?;
        while let Some(x) = iter.next() {
          match iter.peek() {
            Some(_) => write!(f, "{}, ", x)?,
            None => write!(f, "{}, {}", x, end)?,
          }
        }
        write!(f, ")")
      },
      Self::Map(_, xs) => {
        let mut iter = xs.iter().peekable();
        write!(f, "{{")?;
        while let Some((key, val)) = iter.next() {
          match iter.peek() {
            Some(_) => write!(f, "{} = {}, ", key, val)?,
            None => write!(f, "{} = {}", key, val)?,
          }
        }
        write!(f, "}}")
      },
      Self::Link(_, ctx, xs) => {
        let mut iter = xs.iter().peekable();
        write!(f, "[{} ", ctx)?;
        while let Some(x) = iter.next() {
          match iter.peek() {
            Some(_) => write!(f, "{} ", Self::U64(Pos::No, *x))?,
            None => write!(f, "{}", Self::U64(Pos::No, *x))?,
          }
        }
        write!(f, "]")
      },
    }
  }
}

// Redefine Equality for Syn to ignore the Pos arguments, which only matter for
// parser errors
impl<F: LurkField> PartialEq for Syn<F> {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::Num(_, x), Self::Num(_, y)) => x == y,
      (Self::U64(_, x), Self::U64(_, y)) => x == y,
      (Self::Symbol(_, x), Self::Symbol(_, y)) => x == y,
      (Self::String(_, x), Self::String(_, y)) => x == y,
      (Self::Char(_, x), Self::Char(_, y)) => x == y,
      (Self::List(_, x, x1), Self::List(_, y, y1)) => x == y && x1 == y1,
      (Self::Map(_, x), Self::Map(_, y)) => x == y,
      (Self::Link(_, x, x1), Self::Link(_, y, y1)) => x == y && x1 == y1,
      _ => false,
    }
  }
}

impl<F: LurkField> Eq for Syn<F> {}

#[cfg(feature = "test-utils")]
pub mod test_utils {
  use std::collections::BTreeMap;

  use blstrs::Scalar as Fr;
  use lurk_ff::{
    field::FWrap,
    test_utils::frequency,
  };
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  use super::*;
  use crate::sym::{
    test_utils,
    Symbol,
  };

  impl Syn<Fr> {
    fn arbitrary_syn(g: &mut Gen) -> Self {
      #[allow(clippy::type_complexity)]
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> Syn<Fr>>)> = vec![
        (100, Box::new(|g| Self::Num(Pos::No, FWrap::arbitrary(g).0))),
        (100, Box::new(|g| Self::U64(Pos::No, u64::arbitrary(g)))),
        (100, Box::new(|g| Self::Char(Pos::No, char::arbitrary(g)))),
        (100, Box::new(|g| Self::String(Pos::No, Self::arbitrary_string(g)))),
        (50, Box::new(|g| Self::Symbol(Pos::No, Symbol::arbitrary(g)))),
        (50, Box::new(Self::arbitrary_list)),
        (50, Box::new(Self::arbitrary_map)),
        (50, Box::new(Self::arbitrary_link)),
      ];
      frequency(g, input)
    }

    fn arbitrary_string(g: &mut Gen) -> String {
      let num_chars = usize::arbitrary(g) % 5;
      let mut s = String::new();
      for _ in 0..num_chars {
        let c = char::arbitrary(g);
        s.push(c);
      }
      s
    }

    fn arbitrary_list(g: &mut Gen) -> Self {
      let num_exprs: usize = Arbitrary::arbitrary(g);
      let num_exprs = num_exprs % 4;
      let mut exprs = Vec::new();
      for _ in 0..num_exprs {
        let expr = Syn::arbitrary_syn(g);
        exprs.push(expr);
      }
      let improper: bool = Arbitrary::arbitrary(g);
      let end = exprs.pop();
      if improper && num_exprs >= 2 && !matches!(end, Some(Syn::List(_, _, _)))
      {
        Syn::List(Pos::No, exprs, Box::new(end.unwrap()))
      }
      else {
        let nil = Syn::Symbol(Pos::No, lurksym!["nil"]);
        if exprs.is_empty() {
          nil
        }
        else {
          Syn::List(Pos::No, exprs, Box::new(nil))
        }
      }
    }

    fn arbitrary_map(g: &mut Gen) -> Self {
      let num_exprs: usize = Arbitrary::arbitrary(g);
      let num_exprs = num_exprs % 3;
      // we use a BTreeMap to get the right ordering
      let mut map = BTreeMap::new();
      for _ in 0..num_exprs {
        let key = Syn::arbitrary_syn(g);
        let val = Syn::arbitrary_syn(g);
        map.insert(key, val);
      }
      Syn::Map(Pos::No, map.into_iter().collect())
    }

    fn arbitrary_link(g: &mut Gen) -> Self {
      let num_xs: usize = Arbitrary::arbitrary(g);
      let num_xs = num_xs % 4;
      let mut xs = Vec::new();
      for _ in 0..num_xs {
        let x = u64::arbitrary(g);
        xs.push(x);
      }
      Syn::Link(Pos::No, Box::new(Syn::arbitrary_syn(g)), xs)
    }
  }

  impl Arbitrary for Syn<Fr> {
    fn arbitrary(g: &mut Gen) -> Self { Syn::arbitrary_syn(g) }
  }
}

#[cfg(all(test, feature = "test-utils"))]
mod test {
  use blstrs::Scalar as Fr;

  use super::*;
  #[allow(unused_imports)]
  use crate::{
    char,
    keyword,
    list,
    map,
    num,
    str,
    symbol,
    u64,
  };

  fn test_print(syn: Syn<Fr>, expected: &'static str) -> bool {
    let syn_print = format!("{}", syn);
    let res = syn_print == expected;
    if !res {
      println!("syntax: {:?}", syn);
      println!("expected: {}", expected);
      println!("detected: {}", syn_print);
    }
    res
  }

  #[test]
  fn unit_syn_print() {
    assert!(test_print(symbol!([]), "_."));
    assert!(test_print(symbol!(Fr, []), "_."));
    assert!(test_print(keyword!([]), "_:"));
    assert!(test_print(keyword!(Fr, []), "_:"));
    assert!(test_print(symbol!([""]), "."));
    assert!(test_print(keyword!([""]), ":"));
    assert!(test_print(symbol!(["foo"]), "foo"));
    assert!(test_print(symbol!(["fλoo"]), "fλoo"));
    assert!(test_print(symbol!(["foo", ""]), "foo."));
    assert!(test_print(symbol!(["foo", "", ""]), "foo.."));
    assert!(test_print(symbol!(["", "foo"]), "..foo"));
    assert!(test_print(symbol!(["", "", "foo"]), "...foo"));
    assert!(test_print(keyword!(["foo"]), ":foo"));
    assert!(test_print(keyword!(["foo", ""]), ":foo."));
    assert!(test_print(keyword!(["foo", "", ""]), ":foo.."));
    assert!(test_print(keyword!(["", "foo"]), ":.foo"));
    assert!(test_print(keyword!(["", "", "foo"]), ":..foo"));
    assert!(test_print(list!([]), "()"));
    assert!(test_print(list!(Fr, []), "()"));
    assert!(test_print(list!([u64!(1), u64!(2), u64!(3)]), "(1u64 2u64 3u64)"));
    assert!(test_print(
      list!([u64!(1), u64!(2), u64!(3)], u64!(4)),
      "(1u64, 2u64, 3u64, 4u64)"
    ));
    assert!(test_print(
      map!([
        (symbol!(["a"]), u64!(1)),
        (symbol!(["b"]), u64!(2)),
        (symbol!(["c"]), u64!(3))
      ]),
      "{a = 1u64, b = 2u64, c = 3u64}"
    ));
  }

  #[quickcheck]
  fn prop_syn_generates(syn: Syn<Fr>) -> bool {
    // println!("-------------");
    let mut store1 = Store::<Fr>::default();
    let _ptr1 = store1.insert_syn(&PoseidonCache::default(), &syn);
    true
  }
}
