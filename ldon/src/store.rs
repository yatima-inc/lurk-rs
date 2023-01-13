use std::{
  collections::BTreeMap,
  fmt,
};

use lurk_ff::{
  field::LurkField,
  tag::ExprTag,
};

use crate::{
  cid::Cid,
  expr::Expr,
  hash::PoseidonCache,
  parser::position::Pos,
  serde_f::{
    SerdeF,
    SerdeFError,
  },
  sym::Symbol,
  syntax::Syn,
};

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Store<F: LurkField> {
  pub store: BTreeMap<Cid<F>, Entry<F>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Entry<F: LurkField> {
  Expr(Expr<F>),
  Opaque,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StoreError<F: LurkField> {
  UnknownCid(Cid<F>),
  UnexpectedEntry(Cid<F>, Entry<F>, &'static str),
  ExpectedChar(Cid<F>),
  ExpectedU64(Cid<F>),
  ExpectedString(Cid<F>),
  ExpectedSymbol(Cid<F>),
  ExpectedMap(Cid<F>),
  ExpectedLink(Cid<F>),
  Custom(&'static str),
}

impl<F: LurkField> Store<F> {
  pub fn new() -> Self { Self::default() }

  pub fn insert_opaque(&mut self, cid: Cid<F>) -> Cid<F> {
    if !cid.is_immediate() {
      self.store.insert(cid, Entry::Opaque);
    }
    cid
  }

  pub fn insert_expr(
    &mut self,
    cache: &PoseidonCache<F>,
    expr: Expr<F>,
  ) -> Cid<F> {
    let cid = expr.cid(cache);
    if !cid.is_immediate() {
      self.store.insert(cid, Entry::Expr(expr));
    }
    cid
  }

  pub fn insert_symbol(
    &mut self,
    cache: &PoseidonCache<F>,
    sym: &Symbol,
  ) -> Cid<F> {
    let path = sym.path();
    let mut cid = self.insert_expr(cache, Expr::SymNil);
    for s in path {
      let str_cid = self.insert_syn(cache, &Syn::String(Pos::No, s.clone()));
      cid = self.insert_expr(cache, Expr::SymCons(str_cid, cid));
    }
    match sym {
      Symbol::Sym(_) => cid,
      Symbol::Key(_) => self.insert_expr(cache, Expr::Keyword(cid)),
    }
  }

  pub fn insert_syn(
    &mut self,
    cache: &PoseidonCache<F>,
    syn: &Syn<F>,
  ) -> Cid<F> {
    match syn {
      Syn::Num(_, f) => self.insert_expr(cache, Expr::Num(*f)),
      Syn::Char(_, c) => self.insert_expr(cache, Expr::Char(F::from_char(*c))),
      Syn::U64(_, x) => self.insert_expr(cache, Expr::U64((*x).into())),
      Syn::String(_, string) => {
        let mut cid = self.insert_expr(cache, Expr::StrNil);
        for c in string.chars().rev() {
          let char_cid =
            Cid { tag: F::expr_tag(ExprTag::Char), val: F::from_char(c) };
          cid = self.insert_expr(cache, Expr::StrCons(char_cid, cid));
        }
        cid
      },
      Syn::Symbol(_, sym) => self.insert_symbol(cache, sym),
      Syn::List(_, xs, end) => {
        if let (Some(end), true) = (end, xs.is_empty()) {
          let nil_cid = self.insert_expr(cache, Expr::ConsNil);
          let end_cid = self.insert_syn(cache, end);
          return self.insert_expr(cache, Expr::Cons(end_cid, nil_cid));
        }
        let mut cid = match end {
          Some(end) => self.insert_syn(cache, end),
          None => self.insert_expr(cache, Expr::ConsNil),
        };
        for x in xs.iter().rev() {
          let head_cid = self.insert_syn(cache, x);
          cid = self.insert_expr(cache, Expr::Cons(head_cid, cid));
        }
        cid
      },
      Syn::Map(_, map) => {
        // We need to sort the entries by Cid value first
        let mut sorted: BTreeMap<Cid<F>, Cid<F>> = BTreeMap::new();
        for (k, v) in map {
          let key_cid = self.insert_syn(cache, k);
          let val_cid = self.insert_syn(cache, v);
          sorted.insert(key_cid, val_cid);
        }
        // Then construct the cons-list of pairs
        let mut cid = self.insert_expr(cache, Expr::ConsNil);
        for (key_cid, val_cid) in sorted.iter().rev() {
          let head_cid =
            self.insert_expr(cache, Expr::Cons(*key_cid, *val_cid));
          cid = self.insert_expr(cache, Expr::Cons(head_cid, cid));
        }
        self.insert_expr(cache, Expr::Map(cid))
      },
      Syn::Link(_, ctx, val) => {
        let ctx_cid = self.insert_syn(cache, ctx);
        let val_cid = self.insert_syn(
          cache,
          &Syn::List(
            Pos::No,
            val.iter().map(|x| Syn::U64(Pos::No, *x)).collect(),
            None,
          ),
        );
        self.insert_expr(cache, Expr::Link(ctx_cid, val_cid))
      },
    }
  }

  pub fn get_entry(&self, cid: Cid<F>) -> Result<Entry<F>, StoreError<F>> {
    if let Some(expr) = cid.immediate() {
      Ok(Entry::Expr(expr))
    }
    else {
      let entry = self.store.get(&cid).ok_or(StoreError::UnknownCid(cid))?;
      Ok(entry.clone())
    }
  }

  pub fn get_expr(&self, cid: Cid<F>) -> Result<Expr<F>, StoreError<F>> {
    match self.get_entry(cid)? {
      Entry::Expr(x) => Ok(x),
      Entry::Opaque => {
        Err(StoreError::UnexpectedEntry(cid, Entry::Opaque, "Expr"))
      },
    }
  }

  pub fn get_opaque(&self, cid: Cid<F>) -> Result<(), StoreError<F>> {
    match self.get_entry(cid)? {
      Entry::Expr(x) => {
        Err(StoreError::UnexpectedEntry(cid, Entry::Expr(x), "Opaque"))
      },
      Entry::Opaque => Ok(()),
    }
  }

  pub fn get_syn_list(&self, cid: Cid<F>) -> Result<Syn<F>, StoreError<F>> {
    let mut list = vec![];
    let mut cid = cid;

    while let Expr::Cons(car, cdr) = self.get_expr(cid)? {
      list.push(self.get_syn(car)?);
      cid = cdr;
    }
    if let Expr::ConsNil = self.get_expr(cid)? {
      Ok(Syn::List(Pos::No, list, None))
    }
    else {
      Ok(Syn::List(Pos::No, list, Some(Box::new(self.get_syn(cid)?))))
    }
  }

  pub fn get_syn_link(
    &self,
    ctx: Cid<F>,
    data: Cid<F>,
  ) -> Result<Syn<F>, StoreError<F>> {
    let mut list = vec![];
    let ctx = self.get_syn(ctx)?;
    let mut cid = data;

    while let Expr::Cons(car, cdr) = self.get_expr(cid)? {
      list.push(self.get_u64(car)?);
      cid = cdr;
    }
    if let Expr::ConsNil = self.get_expr(cid)? {
      Ok(Syn::Link(Pos::No, Box::new(ctx), list))
    }
    else {
      Err(StoreError::ExpectedLink(cid))
    }
  }

  pub fn get_u64(&self, cid: Cid<F>) -> Result<u64, StoreError<F>> {
    if let Expr::U64(f) = self.get_expr(cid)? {
      let x = F::to_u64(&f).ok_or(StoreError::ExpectedU64(cid))?;
      Ok(x)
    }
    else {
      Err(StoreError::ExpectedU64(cid))
    }
  }

  pub fn get_char(&self, cid: Cid<F>) -> Result<char, StoreError<F>> {
    if let Expr::Char(f) = self.get_expr(cid)? {
      let c = F::to_char(&f).ok_or(StoreError::ExpectedChar(cid))?;
      Ok(c)
    }
    else {
      Err(StoreError::ExpectedChar(cid))
    }
  }

  pub fn get_string(&self, cid: Cid<F>) -> Result<String, StoreError<F>> {
    let mut s = String::new();
    let mut next = cid;

    while let Expr::StrCons(car, cdr) = self.get_expr(next)? {
      s.push(self.get_char(car)?);
      next = cdr;
    }
    if let Expr::StrNil = self.get_expr(next)? {
      Ok(s)
    }
    else {
      Err(StoreError::ExpectedString(cid))
    }
  }

  pub fn get_symbol_path(
    &self,
    cid: Cid<F>,
  ) -> Result<Vec<String>, StoreError<F>> {
    let mut list = vec![];
    let mut next = cid;

    while let Expr::SymCons(car, cdr) = self.get_expr(next)? {
      list.push(self.get_string(car)?);
      next = cdr;
    }
    if let Expr::SymNil = self.get_expr(next)? {
      Ok(list.into_iter().rev().collect())
    }
    else {
      Err(StoreError::ExpectedSymbol(cid))
    }
  }

  pub fn get_syn_map(&self, cid: Cid<F>) -> Result<Syn<F>, StoreError<F>> {
    let mut assoc = vec![];
    let mut next = cid;

    while let Expr::Cons(entry, cdr) = self.get_expr(next)? {
      if let Expr::Cons(key, val) = self.get_expr(entry)? {
        assoc.push((self.get_syn(key)?, self.get_syn(val)?));
        next = cdr;
      }
      else {
        return Err(StoreError::ExpectedMap(cid));
      }
    }
    // could test for correctness here
    Ok(Syn::Map(Pos::No, assoc))
  }

  pub fn get_syn(&self, cid: Cid<F>) -> Result<Syn<F>, StoreError<F>> {
    let expr = self.get_expr(cid)?;
    match expr {
      Expr::ConsNil => Ok(Syn::List(Pos::No, vec![], None)),
      Expr::SymNil => Ok(Syn::Symbol(Pos::No, Symbol::root_sym())),
      Expr::StrNil => Ok(Syn::String(Pos::No, "".to_string())),
      Expr::Num(f) => Ok(Syn::Num(Pos::No, f)),
      Expr::Char(_) => Ok(Syn::Char(Pos::No, self.get_char(cid)?)),
      Expr::U64(_) => Ok(Syn::U64(Pos::No, self.get_u64(cid)?)),
      Expr::Cons(..) => self.get_syn_list(cid),
      Expr::StrCons(..) => Ok(Syn::String(Pos::No, self.get_string(cid)?)),
      Expr::SymCons(..) => {
        Ok(Syn::Symbol(Pos::No, Symbol::Sym(self.get_symbol_path(cid)?)))
      },
      Expr::Keyword(sym) => {
        Ok(Syn::Symbol(Pos::No, Symbol::Key(self.get_symbol_path(sym)?)))
      },
      Expr::Map(map) => self.get_syn_map(map),
      Expr::Link(ctx, data) => self.get_syn_link(ctx, data),
      _ => Err(StoreError::Custom("no syntax for Comm, Thunk, Fun")),
      // Expr::Comm(F, Cid<F>),             // secret, val
      // Expr::Thunk(Cid<F>, Cid<F>),       // val, cont
      // Expr::Fun(Cid<F>, Cid<F>, Cid<F>), // arg, body, env
    }
  }
}

impl<F: LurkField> fmt::Display for Store<F> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, "{{")?;
    for (k, v) in self.store.iter() {
      match v {
        Entry::Expr(x) => {
          writeln!(f, "  {}: {},", k, x)?;
        },
        Entry::Opaque => {
          writeln!(f, "  {}: _,", k)?;
        },
      }
    }
    writeln!(f, "}}")?;
    Ok(())
  }
}

impl<F: LurkField> SerdeF<F> for Store<F> {
  fn ser_f(&self) -> Vec<F> {
    let mut exprs = Vec::new();
    let mut opaqs = Vec::new();
    for (cid, entry) in self.store.iter() {
      match entry {
        Entry::Expr(x) => exprs.extend(x.ser_f().into_iter()),
        Entry::Opaque => opaqs.extend(cid.ser_f()),
      }
    }
    let mut res = vec![(opaqs.len() as u64).into()];
    res.extend(opaqs);
    res.extend(exprs);
    res
  }

  fn de_f(fs: &[F]) -> Result<Store<F>, SerdeFError<F>> {
    let mut map: BTreeMap<Cid<F>, Entry<F>> = BTreeMap::new();
    if fs.is_empty() {
      return Err(SerdeFError::UnexpectedEnd);
    }
    let opaqs: u64 =
      fs[0].to_u64().ok_or_else(|| SerdeFError::ExpectedU64(fs[0]))?;
    // This cast will break on 32-bit targets if there are more the 2^32
    // opaque pointers in the store, but maybe we don't care about that.
    // TODO: Harden for wasm32-unknown-unknown compilation
    let opaqs: usize = opaqs as usize;
    if fs.len() < opaqs {
      return Err(SerdeFError::UnexpectedEnd);
    }
    let mut i = 1;
    while i <= opaqs {
      map.insert(Cid::de_f(&fs[i..])?, Entry::Opaque);
      i += 2;
    }
    while i < fs.len() {
      let cid = Cid::de_f(&fs[i..])?;
      let expr = Expr::de_f(&fs[i..])?;
      map.insert(cid, Entry::Expr(expr));
      i += 2 + expr.child_cids().len() * 2;
    }
    Ok(Store { store: map })
  }
}
#[cfg(feature = "test-utils")]
pub mod test_utils {
  use blstrs::Scalar as Fr;
  use lurk_ff::test_utils::frequency;
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  use super::*;
  impl Arbitrary for Entry<Fr> {
    fn arbitrary(g: &mut Gen) -> Self {
      #[allow(clippy::type_complexity)]
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> Entry<Fr>>)> = vec![
        (100, Box::new(|_| Self::Opaque)),
        (100, Box::new(|g| Self::Expr(Expr::arbitrary(g)))),
      ];
      frequency(g, input)
    }
  }

  impl Arbitrary for Store<Fr> {
    fn arbitrary(g: &mut Gen) -> Self {
      let mut store: BTreeMap<Cid<Fr>, Entry<Fr>> = BTreeMap::new();
      let n: usize = usize::arbitrary(g) % 5;
      let cache = PoseidonCache::default();
      for _ in 0..n {
        let entry = Entry::arbitrary(g);
        match entry {
          Entry::Opaque => store.insert(Cid::arbitrary(g), entry),
          Entry::Expr(x) => store.insert(x.cid(&cache), entry),
        };
      }
      Store { store }
    }
  }
}

#[cfg(all(test, feature = "test-utils"))]
mod test {
  use blstrs::Scalar as Fr;

  use super::*;
  #[allow(unused_imports)]
  use crate::{
    char,
    list,
    map,
    num,
    str,
    sym,
    u64,
  };

  #[test]
  fn unit_expr_store_get() {
    let mut store = Store::<Fr>::default();
    let cache = PoseidonCache::default();

    let mut test = |expr1| {
      let cid = store.insert_expr(&cache, expr1);
      let expr2 = store.get_expr(cid).unwrap();
      assert!(expr1 == expr2);
      cid
    };

    test(Expr::Num(0u64.into()));
    test(Expr::U64(0u64.into()));
    let a = test(Expr::Char(97u64.into()));
    test(Expr::SymNil);
    let nil = test(Expr::ConsNil);
    let str_nil = test(Expr::StrNil);
    test(Expr::Cons(nil, nil));
    test(Expr::StrCons(a, str_nil));
  }

  #[test]
  fn unit_syn_store_get() {
    let mut store = Store::<Fr>::default();
    let cache = PoseidonCache::default();

    let mut test = |syn1| {
      let cid = store.insert_syn(&cache, &syn1);
      if let Ok(syn2) = store.get_syn(cid) {
        println!("{:?}", syn1);
        println!("{:?}", syn2);
        assert!(syn1 == syn2);
        cid
      }
      else {
        println!("{:?}", store.get_syn(cid));
        println!("{}", store);
        assert!(false);
        cid
      }
    };

    test(Syn::Num(Pos::No, 0u64.into()));
    test(Syn::U64(Pos::No, 0u64.into()));
    test(Syn::Char(Pos::No, 'a'));
    test(Syn::String(Pos::No, "foo".to_string()));
    test(Syn::List(Pos::No, vec![Syn::Num(Pos::No, 0u64.into())], None));
    test(Syn::List(
      Pos::No,
      vec![Syn::Num(Pos::No, 0u64.into())],
      Some(Box::new(Syn::Num(Pos::No, 0u64.into()))),
    ));
    test(Syn::Symbol(Pos::No, Symbol::Sym(vec![])));
    test(Syn::Symbol(Pos::No, Symbol::Sym(vec!["foo".to_string()])));
    test(Syn::Symbol(
      Pos::No,
      Symbol::Sym(vec!["foo".to_string(), "bar".to_string()]),
    ));
  }
  #[test]
  fn unit_syn_store_demo() {
    // (lambda (x) x)
    let syn_from_macro =
      list!(Fr, [sym!(["lambda"]), list!([sym!(["x"])]), sym!(["x"])]);
    println!("syntax from macro {}", syn_from_macro);
    let syn_from_text = Syn::<Fr>::parse("(lambda (x) x)").unwrap();
    println!("syntax from text  {}", syn_from_text);
    assert_eq!(syn_from_macro, syn_from_text);
    let syn = syn_from_text;

    let mut store = Store::default();
    let cache = PoseidonCache::default();

    let cid = store.insert_syn(&cache, &syn);
    println!("cid: {}", cid);
    println!("store: {}", store);
    let syn2 = store.get_syn(cid).unwrap();
    assert_eq!(syn, syn2);
    let bytes = store.ser();
    println!("bytes: {:?}", bytes);
    let store2 = Store::de(&bytes).unwrap();

    assert_eq!(store, store2);
  }

  #[quickcheck]
  fn prop_syn_store_get(syn1: Syn<Fr>) -> bool {
    let mut store1 = Store::<Fr>::default();
    let cache = PoseidonCache::default();
    let cid1 = store1.insert_syn(&cache, &syn1);
    let syn2 = store1.get_syn(cid1);
    println!("{:?}", syn1);
    println!("{:?}", syn2);
    syn1 == syn2.unwrap()
  }

  #[test]
  fn unit_syn_store_serdef() {
    let mut store1 = Store::<Fr>::default();
    let cache = PoseidonCache::default();

    let mut test = |syn1| {
      let _cid = store1.insert_syn(&cache, &syn1);
      let vec = &store1.ser_f();
      println!("syn: {:?}", syn1);
      println!("store: {}", store1);
      println!("vec: {:?} {:?}", vec.len(), vec);
      match Store::de_f(&vec) {
        Ok(store2) => {
          println!("store1: {}", store1);
          println!("store2: {}", store2);
          assert!(store1 == store2)
        },
        Err(e) => {
          println!("{:?}", e);
          assert!(false)
        },
      }
    };

    test(Syn::Num(Pos::No, 0u64.into()));
    test(Syn::U64(Pos::No, 0u64.into()));
    test(Syn::Char(Pos::No, 'a'));
    test(Syn::String(Pos::No, "".to_string()));
    test(Syn::String(Pos::No, "a".to_string()));
    test(Syn::String(Pos::No, "ab".to_string()));
    test(Syn::List(Pos::No, vec![Syn::Num(Pos::No, 0u64.into())], None));
    test(Syn::List(
      Pos::No,
      vec![Syn::Num(Pos::No, 0u64.into())],
      Some(Box::new(Syn::Num(Pos::No, 0u64.into()))),
    ));
    test(Syn::Symbol(Pos::No, Symbol::Sym(vec![])));
    test(Syn::Symbol(Pos::No, Symbol::Sym(vec!["foo".to_string()])));
    test(Syn::Symbol(
      Pos::No,
      Symbol::Sym(vec!["foo".to_string(), "bar".to_string()]),
    ));
  }

  #[quickcheck]
  fn prop_syn_store_serdef(syn1: Syn<Fr>) -> bool {
    println!("==================");
    let mut store1 = Store::<Fr>::default();
    let cache = PoseidonCache::default();
    store1.insert_syn(&cache, &syn1);
    let vec = &store1.ser_f();
    match Store::de_f(&vec) {
      Ok(store2) => {
        println!("store1: {}", store1);
        println!("store2: {}", store2);
        store1 == store2
      },
      Err(e) => {
        println!("{:?}", e);
        false
      },
    }
  }
  #[quickcheck]
  fn prop_store_serdef(store1: Store<Fr>) -> bool {
    println!("==================");
    let vec = &store1.ser_f();
    // println!("store1: {}", store1);
    match Store::de_f(&vec) {
      Ok(store2) => {
        println!("store1: {}", store1);
        println!("store2: {}", store2);
        store1 == store2
      },
      Err(e) => {
        println!("{:?}", e);
        false
      },
    }
  }
}
