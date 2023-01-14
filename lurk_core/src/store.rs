use std::{
  cell::RefCell,
  rc::Rc,
};

use ahash::RandomState;
use dashmap::DashMap;
use ldon::{
  cid::Cid,
  hash::HashConstants,
  op::{
    Op1,
    Op2,
  },
  store::Entry,
  syntax::Syn,
  PoseidonCache,
};
use lurk_ff::{
  tag::ExprTag,
  LurkField,
};
// use std::fmt;
// use once_cell::sync::OnceCell;
use rayon::prelude::*;

use crate::{
  error::LurkError,
  expr::Expr,
  num::Num,
  ptr::{
    Ptr,
    RawPtr,
  },
  uint::UInt,
};

type IndexSet<K> = indexmap::IndexSet<K, RandomState>;

#[derive(Debug)]
pub struct Store<F: LurkField> {
  pub conses: IndexSet<(Ptr<F>, Ptr<F>)>,
  pub comms: IndexSet<(Ptr<F>, Ptr<F>)>,
  pub funs: IndexSet<(Ptr<F>, Ptr<F>, Ptr<F>)>,
  pub syms: IndexSet<(Ptr<F>, Ptr<F>)>,
  pub keys: IndexSet<Ptr<F>>,
  // Other sparse storage format without hashing is likely more efficient
  pub nums: IndexSet<Num<F>>,
  pub strs: IndexSet<(Ptr<F>, Ptr<F>)>,
  pub links: IndexSet<(Ptr<F>, Ptr<F>)>,
  pub maps: IndexSet<Ptr<F>>,
  pub thunks: IndexSet<(Ptr<F>, Ptr<F>)>,
  pub call0s: IndexSet<(Ptr<F>, Ptr<F>)>,
  pub calls: IndexSet<(Ptr<F>, Ptr<F>, Ptr<F>)>,
  pub call2s: IndexSet<(Ptr<F>, Ptr<F>, Ptr<F>)>,
  pub tails: IndexSet<(Ptr<F>, Ptr<F>)>,
  pub lookups: IndexSet<(Ptr<F>, Ptr<F>)>,
  pub unops: IndexSet<(Ptr<F>, Ptr<F>)>,
  pub binops: IndexSet<(Ptr<F>, Ptr<F>, Ptr<F>, Ptr<F>)>,
  pub binop2s: IndexSet<(Ptr<F>, Ptr<F>, Ptr<F>)>,

  pub ifs: IndexSet<(Ptr<F>, Ptr<F>)>,
  pub lets: IndexSet<(Ptr<F>, Ptr<F>, Ptr<F>, Ptr<F>)>,
  pub let_recs: IndexSet<(Ptr<F>, Ptr<F>, Ptr<F>, Ptr<F>)>,
  pub emits: IndexSet<Ptr<F>>,

  // Holds the opaque Cids which RawPtr::Opaque indexes
  pub opaques: IndexSet<Cid<F>>,
  /// Holds a mapping of Cid -> Ptr for reverse lookups
  pub cids: DashMap<Cid<F>, Ptr<F>, RandomState>,

  /// Caches poseidon hashes
  pub poseidon_cache: PoseidonCache<F>,

  // Contains Ptrs which have not yet been hydrated.
  pub dehydrated: Vec<Ptr<F>>,
  // pub lurk_package: Package,
}

impl<F: LurkField> Default for Store<F> {
  fn default() -> Self {
    let store = Store {
      conses: Default::default(),
      comms: Default::default(),
      syms: Default::default(),
      keys: Default::default(),
      nums: Default::default(),
      funs: Default::default(),
      strs: Default::default(),
      links: Default::default(),
      thunks: Default::default(),
      maps: Default::default(),
      call0s: Default::default(),
      calls: Default::default(),
      call2s: Default::default(),
      tails: Default::default(),
      lookups: Default::default(),
      unops: Default::default(),
      binops: Default::default(),
      binop2s: Default::default(),
      ifs: Default::default(),
      lets: Default::default(),
      let_recs: Default::default(),
      emits: Default::default(),
      opaques: Default::default(),
      cids: Default::default(),
      poseidon_cache: Default::default(),
      dehydrated: Default::default(),
      // lurk_package: Package::lurk(),
    };

    store
  }
}

#[derive(Clone, Copy, Debug)]
pub enum HashMode {
  Put,
  Get,
}

impl<F: LurkField> Store<F> {
  pub fn new() -> Self { Store::default() }

  pub fn poseidon_constants(&self) -> &HashConstants<F> {
    &self.poseidon_cache.constants
  }

  pub fn intern_cid(
    &mut self,
    cid: Cid<F>,
    ldon_store: &ldon::Store<F>,
  ) -> Result<Ptr<F>, LurkError<F>> {
    use ldon::store::Entry;
    let entry = ldon_store.get_entry(cid).map_err(LurkError::LdonStore)?;
    let Cid { tag, val } = cid;
    match (tag.expr, entry) {
      (_, Entry::Opaque) => self.intern_opaque_cid(cid, false),
      (ExprTag::Cons, Entry::Expr(ldon::Expr::Cons(car, cdr))) => {
        let car = self.intern_cid(car, ldon_store)?;
        let cdr = self.intern_cid(cdr, ldon_store)?;
        self.intern_expr(Expr::Cons(car, cdr))
      },
      (ExprTag::Comm, Entry::Expr(ldon::Expr::Comm(secret, payload))) => {
        let secret = self.intern_cid(secret, ldon_store)?;
        let payload = self.intern_cid(payload, ldon_store)?;
        self.intern_expr(Expr::Comm(secret, payload))
      },
      (ExprTag::Sym, Entry::Expr(ldon::Expr::SymNil)) if val == F::zero() => {
        self.intern_expr(Expr::SymNil)
      },
      (ExprTag::Sym, Entry::Expr(ldon::Expr::SymCons(car, cdr))) => {
        let car = self.intern_cid(car, ldon_store)?;
        let cdr = self.intern_cid(cdr, ldon_store)?;
        self.intern_expr(Expr::SymCons(car, cdr))
      },
      (ExprTag::Key, Entry::Expr(ldon::Expr::Keyword(sym))) => {
        let sym = self.intern_cid(sym, ldon_store)?;
        self.intern_expr(Expr::Keyword(sym))
      },
      (ExprTag::Str, Entry::Expr(ldon::Expr::StrNil)) if val == F::zero() => {
        self.intern_expr(Expr::StrNil)
      },
      (ExprTag::Str, Entry::Expr(ldon::Expr::StrCons(car, cdr))) => {
        let car = self.intern_cid(car, ldon_store)?;
        let cdr = self.intern_cid(cdr, ldon_store)?;
        self.intern_expr(Expr::StrCons(car, cdr))
      },
      (ExprTag::Num, Entry::Expr(ldon::Expr::Num(f))) => {
        self.intern_expr(Expr::Num(Num::Scalar(f)))
      },
      (ExprTag::U64, Entry::Expr(ldon::Expr::U64(..))) => {
        let x = ldon_store.get_u64(cid).map_err(LurkError::LdonStore)?;
        self.intern_expr(Expr::UInt(UInt::U64(x)))
      },
      (ExprTag::Char, Entry::Expr(ldon::Expr::Char(..))) => {
        let c = ldon_store.get_char(cid).map_err(LurkError::LdonStore)?;
        self.intern_expr(Expr::Char(c))
      },
      (ExprTag::Thunk, Entry::Expr(ldon::Expr::Thunk(val, cont))) => {
        let val = self.intern_cid(val, ldon_store)?;
        let cont = self.intern_cid(cont, ldon_store)?;
        self.intern_expr(Expr::Thunk(val, cont))
      },
      (ExprTag::Map, Entry::Expr(ldon::Expr::Map(map))) => {
        let map = self.intern_cid(map, ldon_store)?;
        self.intern_expr(Expr::Map(map))
      },
      (ExprTag::Link, Entry::Expr(ldon::Expr::Link(ctx, data))) => {
        let ctx = self.intern_cid(ctx, ldon_store)?;
        let data = self.intern_cid(data, ldon_store)?;
        self.intern_expr(Expr::Link(ctx, data))
      },
      (ExprTag::Op1, Entry::Expr(ldon::Expr::Op1(op1))) => {
        self.intern_expr(Expr::Op1(op1))
      },
      (ExprTag::Op2, Entry::Expr(ldon::Expr::Op1(op1))) => {
        self.intern_expr(Expr::Op1(op1))
      },
      (ExprTag::Outermost, Entry::Expr(ldon::Expr::Outermost)) => {
        self.intern_expr(Expr::Outermost)
      },
      (ExprTag::Call, Entry::Expr(ldon::Expr::Call(arg, env, cont))) => {
        let arg = self.intern_cid(arg, ldon_store)?;
        let env = self.intern_cid(env, ldon_store)?;
        let cont = self.intern_cid(cont, ldon_store)?;
        self.intern_expr(Expr::Call(arg, env, cont))
      },
      (ExprTag::Call0, Entry::Expr(ldon::Expr::Call0(env, cont))) => {
        let env = self.intern_cid(env, ldon_store)?;
        let cont = self.intern_cid(cont, ldon_store)?;
        self.intern_expr(Expr::Call0(env, cont))
      },
      (ExprTag::Call2, Entry::Expr(ldon::Expr::Call2(fun, env, cont))) => {
        let fun = self.intern_cid(fun, ldon_store)?;
        let env = self.intern_cid(env, ldon_store)?;
        let cont = self.intern_cid(cont, ldon_store)?;
        self.intern_expr(Expr::Call2(fun, env, cont))
      },
      (ExprTag::Tail, Entry::Expr(ldon::Expr::Tail(env, cont))) => {
        let env = self.intern_cid(env, ldon_store)?;
        let cont = self.intern_cid(cont, ldon_store)?;
        self.intern_expr(Expr::Tail(env, cont))
      },
      (ExprTag::Error, Entry::Expr(ldon::Expr::Error)) => {
        self.intern_expr(Expr::Error)
      },
      (ExprTag::Unop, Entry::Expr(ldon::Expr::Unop(op1, cont))) => {
        let op1 = self.intern_cid(op1, ldon_store)?;
        let cont = self.intern_cid(cont, ldon_store)?;
        self.intern_expr(Expr::Unop(op1, cont))
      },
      (
        ExprTag::Binop,
        Entry::Expr(ldon::Expr::Binop(op2, env, args, cont)),
      ) => {
        let op2 = self.intern_cid(op2, ldon_store)?;
        let env = self.intern_cid(env, ldon_store)?;
        let args = self.intern_cid(args, ldon_store)?;
        let cont = self.intern_cid(cont, ldon_store)?;
        self.intern_expr(Expr::Binop(op2, env, args, cont))
      },
      (ExprTag::Binop2, Entry::Expr(ldon::Expr::Binop2(op2, env, cont))) => {
        let op2 = self.intern_cid(op2, ldon_store)?;
        let env = self.intern_cid(env, ldon_store)?;
        let cont = self.intern_cid(cont, ldon_store)?;
        self.intern_expr(Expr::Binop2(op2, env, cont))
      },
      (ExprTag::If, Entry::Expr(ldon::Expr::If(args, cont))) => {
        let args = self.intern_cid(args, ldon_store)?;
        let cont = self.intern_cid(cont, ldon_store)?;
        self.intern_expr(Expr::If(args, cont))
      },
      (ExprTag::Let, Entry::Expr(ldon::Expr::Let(var, body, env, cont))) => {
        let var = self.intern_cid(var, ldon_store)?;
        let body = self.intern_cid(body, ldon_store)?;
        let env = self.intern_cid(env, ldon_store)?;
        let cont = self.intern_cid(cont, ldon_store)?;
        self.intern_expr(Expr::Let(var, body, env, cont))
      },
      (
        ExprTag::LetRec,
        Entry::Expr(ldon::Expr::LetRec(var, body, env, cont)),
      ) => {
        let var = self.intern_cid(var, ldon_store)?;
        let body = self.intern_cid(body, ldon_store)?;
        let env = self.intern_cid(env, ldon_store)?;
        let cont = self.intern_cid(cont, ldon_store)?;
        self.intern_expr(Expr::LetRec(var, body, env, cont))
      },
      (ExprTag::Emit, Entry::Expr(ldon::Expr::Emit(cont))) => {
        let cont = self.intern_cid(cont, ldon_store)?;
        self.intern_expr(Expr::Emit(cont))
      },
      (ExprTag::Dummy, Entry::Expr(ldon::Expr::Dummy)) => {
        self.intern_expr(Expr::Dummy)
      },
      (ExprTag::Terminal, Entry::Expr(ldon::Expr::Terminal)) => {
        self.intern_expr(Expr::Terminal)
      },
      _ => Err(LurkError::MalformedLdonStore(cid, ldon_store.clone())),
    }
  }

  // Intern a potentially-opaque value. If the corresponding non-opaque value is
  // already known to the store, return the known value. If we set `force`
  // we ensure an opaque Ptr, even when the corresponding value is present in
  // the store,
  fn intern_opaque_cid(
    &mut self,
    cid: Cid<F>,
    force: bool,
  ) -> Result<Ptr<F>, LurkError<F>> {
    self.hydrate_cid_cache()?;
    match self.cids.get(&cid) {
      Some(p) if p.is_opaque() || !force => Ok(*p),
      _ => {
        let (idx, _) = self.opaques.insert_full(cid);
        Ok(Ptr::opaque(cid.tag.expr, idx))
      },
    }
  }

  fn get_opaque_cid(&self, cid: Cid<F>) -> Result<Ptr<F>, LurkError<F>> {
    match self.cids.get(&cid) {
      Some(p) => Ok(*p),
      None => Err(LurkError::UnknownCid(cid)),
    }
  }

  // TODO: Figure out if we need the cache_mode flag
  pub fn cache_cid(
    &self,
    ptr: Ptr<F>,
    cid: Cid<F>,
    cache_mode: HashMode,
  ) -> Result<(), LurkError<F>> {
    match (cache_mode, self.cids.try_entry(cid)) {
      (HashMode::Put, Some(entry)) => {
        entry.or_insert(ptr);
        Ok(())
      },
      (HashMode::Put, None) => {
        Err(LurkError::Custom("encountered lock when trying to cache a Cid"))
      },
      (HashMode::Get, _) => Ok(()),
    }
  }

  pub fn get_expr(&self, ptr: Ptr<F>) -> Result<Expr<F>, LurkError<F>> {
    let tag = ptr.tag.expr;
    match (tag, ptr.raw) {
      (_, RawPtr::Opaque(idx)) => {
        let cid =
          self.opaques.get_index(idx).ok_or(LurkError::GetOpaque(tag, idx))?;
        let ptr = self.get_opaque_cid(*cid)?;
        self.get_expr(ptr)
      },
      (ExprTag::Cons, RawPtr::Index(idx)) => {
        let (car, cdr) =
          self.conses.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::Cons(*car, *cdr))
      },
      (ExprTag::Comm, RawPtr::Index(idx)) => {
        let (secret, payload) =
          self.comms.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::Comm(*secret, *payload))
      },
      (ExprTag::Sym, RawPtr::Null) => Ok(Expr::SymNil),
      (ExprTag::Sym, RawPtr::Index(idx)) => {
        let (car, cdr) =
          self.syms.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::SymCons(*car, *cdr))
      },
      (ExprTag::Key, RawPtr::Index(idx)) => {
        let sym =
          self.keys.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::Keyword(*sym))
      },
      (ExprTag::Fun, RawPtr::Index(idx)) => {
        let (arg, body, env) =
          self.funs.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::Fun(*arg, *body, *env))
      },
      (ExprTag::Num, RawPtr::Index(idx)) => {
        let num =
          self.nums.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::Num(*num))
      },
      (ExprTag::Str, RawPtr::Null) => Ok(Expr::StrNil),
      (ExprTag::Str, RawPtr::Index(idx)) => {
        let (car, cdr) =
          self.strs.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::StrCons(*car, *cdr))
      },
      (ExprTag::Thunk, RawPtr::Index(idx)) => {
        let (val, cont) =
          self.thunks.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::Thunk(*val, *cont))
      },
      (ExprTag::Map, RawPtr::Index(idx)) => {
        let map =
          self.maps.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::Map(*map))
      },
      (ExprTag::Link, RawPtr::Index(idx)) => {
        let (ctx, data) =
          self.links.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::Link(*ctx, *data))
      },
      (ExprTag::Char, RawPtr::Index(idx)) => {
        let c =
          char::from_u32(idx as u32).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::Char(c))
      },
      (ExprTag::U64, RawPtr::Index(idx)) => Ok(Expr::UInt((idx as u64).into())),
      (ExprTag::Op1, RawPtr::Index(idx)) => {
        let x = u16::try_from(idx)
          .map_err(|e| LurkError::InvalidOp1Ptr(ptr, e.to_string()))?;
        Ok(Expr::Op1(
          Op1::try_from(x)
            .map_err(|e| LurkError::InvalidOp1Ptr(ptr, e.to_string()))?,
        ))
      },
      (ExprTag::Op2, RawPtr::Index(idx)) => {
        let x = u16::try_from(idx)
          .map_err(|e| LurkError::InvalidOp2Ptr(ptr, e.to_string()))?;
        Ok(Expr::Op2(
          Op2::try_from(x)
            .map_err(|e| LurkError::InvalidOp2Ptr(ptr, e.to_string()))?,
        ))
      },
      (ExprTag::Outermost, RawPtr::Null) => Ok(Expr::Outermost),
      (ExprTag::Call, RawPtr::Index(idx)) => {
        let (arg, env, cont) =
          self.calls.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::Call(*arg, *env, *cont))
      },
      (ExprTag::Call0, RawPtr::Index(idx)) => {
        let (env, cont) =
          self.call0s.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::Call0(*env, *cont))
      },
      (ExprTag::Call2, RawPtr::Index(idx)) => {
        let (fun, env, cont) =
          self.call2s.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::Call2(*fun, *env, *cont))
      },
      (ExprTag::Tail, RawPtr::Index(idx)) => {
        let (env, cont) =
          self.tails.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::Tail(*env, *cont))
      },
      (ExprTag::Lookup, RawPtr::Index(idx)) => {
        let (env, cont) =
          self.lookups.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::Lookup(*env, *cont))
      },
      (ExprTag::Unop, RawPtr::Index(idx)) => {
        let (op, cont) =
          self.unops.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::Unop(*op, *cont))
      },
      (ExprTag::Binop, RawPtr::Index(idx)) => {
        let (op, env, args, cont) =
          self.binops.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::Binop(*op, *env, *args, *cont))
      },
      (ExprTag::Binop2, RawPtr::Index(idx)) => {
        let (op, arg, cont) =
          self.binop2s.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::Binop2(*op, *arg, *cont))
      },
      (ExprTag::If, RawPtr::Index(idx)) => {
        let (args, cont) =
          self.ifs.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::If(*args, *cont))
      },
      (ExprTag::Let, RawPtr::Index(idx)) => {
        let (var, body, env, cont) =
          self.lets.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::Let(*var, *body, *env, *cont))
      },
      (ExprTag::LetRec, RawPtr::Index(idx)) => {
        let (var, body, env, cont) =
          self.let_recs.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::LetRec(*var, *body, *env, *cont))
      },
      (ExprTag::Emit, RawPtr::Index(idx)) => {
        let cont =
          self.emits.get_index(idx).ok_or(LurkError::GetIndex(tag, idx))?;
        Ok(Expr::Emit(*cont))
      },
      (ExprTag::Error, RawPtr::Null) => Ok(Expr::Error),
      (ExprTag::Terminal, RawPtr::Null) => Ok(Expr::Terminal),
      (ExprTag::Dummy, RawPtr::Null) => Ok(Expr::Dummy),
      _ => Err(LurkError::GetUnknownPtr(ptr)),
    }
  }

  pub fn hash_expr(&self, ptr: Ptr<F>) -> Result<Cid<F>, LurkError<F>> {
    self.get_entry(ptr, None, HashMode::Put).map(|x| x.0)
  }

  // Get hash for expr, but only if it already exists. This should never cause
  // create_scalar_ptr to be called. Use this after the cache has been
  // hydrated. NOTE: because dashmap::entry can deadlock, it is important not to
  // call hash_expr in nested call graphs which might trigger that behavior.
  // This discovery is what led to get_expr_hash
  // TODO: investigate whether dashmap::try_entry fixes this
  pub fn get_expr_hash(&self, ptr: Ptr<F>) -> Result<Cid<F>, LurkError<F>> {
    self.get_entry(ptr, None, HashMode::Get).map(|x| x.0)
  }

  pub fn get_entry(
    &self,
    ptr: Ptr<F>,
    ls: Option<Rc<RefCell<ldon::Store<F>>>>,
    cache_mode: HashMode,
  ) -> Result<(Cid<F>, Entry<F>), LurkError<F>> {
    match ptr.raw {
      RawPtr::Opaque(idx) => {
        let cid = self
          .opaques
          .get_index(idx)
          .ok_or(LurkError::GetOpaque(ptr.tag.expr, idx))?;
        match self.cids.get(cid) {
          // TODO: Double check if this is safe to do. If someone puts a
          // cid in opaques and then maps the cid to the resulting opaque in
          // cids this could create cycles
          Some(ptr) => self.get_entry(*ptr, ls.clone(), cache_mode),
          None => {
            self.cache_cid(ptr, *cid, cache_mode)?;
            if let Some(ls) = ls {
              ls.borrow_mut().insert_opaque(*cid);
            };
            Ok((*cid, Entry::Opaque))
          },
        }
      },
      _ => {
        let (cid, expr) = match self.get_expr(ptr)? {
          Expr::Cons(car, cdr) => {
            let (car, _) = self.get_entry(car, ls.clone(), cache_mode)?;
            let (cdr, _) = self.get_entry(cdr, ls.clone(), cache_mode)?;
            let expr = ldon::Expr::Cons(car, cdr);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Comm(secret, payload) => {
            let (secret, _) = self.get_entry(secret, ls.clone(), cache_mode)?;
            let (payload, _) =
              self.get_entry(payload, ls.clone(), cache_mode)?;
            let expr = ldon::Expr::Comm(secret, payload);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::SymNil => {
            let expr = ldon::Expr::SymNil;
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::SymCons(car, cdr) => {
            let (car, _) = self.get_entry(car, ls.clone(), cache_mode)?;
            let (cdr, _) = self.get_entry(cdr, ls.clone(), cache_mode)?;
            let expr = ldon::Expr::SymCons(car, cdr);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Keyword(sym) => {
            let (sym, _) = self.get_entry(sym, ls.clone(), cache_mode)?;
            let expr = ldon::Expr::Keyword(sym);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Fun(arg, body, env) => {
            let (arg, _) = self.get_entry(arg, ls.clone(), cache_mode)?;
            let (body, _) = self.get_entry(body, ls.clone(), cache_mode)?;
            let (env, _) = self.get_entry(env, ls.clone(), cache_mode)?;
            let expr = ldon::Expr::Fun(arg, body, env);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Num(num) => {
            let expr = ldon::Expr::Num(num.into_scalar());
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::StrNil => {
            let expr = ldon::Expr::StrNil;
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::StrCons(car, cdr) => {
            let (car, _) = self.get_entry(car, ls.clone(), cache_mode)?;
            let (cdr, _) = self.get_entry(cdr, ls.clone(), cache_mode)?;
            let expr = ldon::Expr::StrCons(car, cdr);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Map(map) => {
            let (map, _) = self.get_entry(map, ls.clone(), cache_mode)?;
            let expr = ldon::Expr::Map(map);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Link(ctx, data) => {
            let (ctx, _) = self.get_entry(ctx, ls.clone(), cache_mode)?;
            let (data, _) = self.get_entry(data, ls.clone(), cache_mode)?;
            let expr = ldon::Expr::Link(ctx, data);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Thunk(val, cont) => {
            let (val, _) = self.get_entry(val, ls.clone(), cache_mode)?;
            let (cont, _) = self.get_entry(cont, ls.clone(), cache_mode)?;
            let expr = ldon::Expr::Thunk(val, cont);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Char(c) => {
            let expr = ldon::Expr::Char(F::from_char(c));
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::UInt(uint) => {
            let expr = ldon::Expr::U64(F::from_u64(uint.into()));
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Op1(op1) => {
            let expr = ldon::Expr::Op1(op1);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Op2(op2) => {
            let expr = ldon::Expr::Op2(op2);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Call0(env, cont) => {
            let (env, _) = self.get_entry(env, ls.clone(), cache_mode)?;
            let (cont, _) = self.get_entry(cont, ls.clone(), cache_mode)?;
            let expr = ldon::Expr::Call0(env, cont);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Call2(fun, env, cont) => {
            let (fun, _) = self.get_entry(fun, ls.clone(), cache_mode)?;
            let (env, _) = self.get_entry(env, ls.clone(), cache_mode)?;
            let (cont, _) = self.get_entry(cont, ls.clone(), cache_mode)?;
            let expr = ldon::Expr::Call2(fun, env, cont);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Tail(env, cont) => {
            let (env, _) = self.get_entry(env, ls.clone(), cache_mode)?;
            let (cont, _) = self.get_entry(cont, ls.clone(), cache_mode)?;
            let expr = ldon::Expr::Tail(env, cont);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Lookup(env, cont) => {
            let (env, _) = self.get_entry(env, ls.clone(), cache_mode)?;
            let (cont, _) = self.get_entry(cont, ls.clone(), cache_mode)?;
            let expr = ldon::Expr::Lookup(env, cont);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Unop(op, cont) => {
            let (op, _) = self.get_entry(op, ls.clone(), cache_mode)?;
            let (cont, _) = self.get_entry(cont, ls.clone(), cache_mode)?;
            let expr = ldon::Expr::Unop(op, cont);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Binop(op, env, args, cont) => {
            let (op, _) = self.get_entry(op, ls.clone(), cache_mode)?;
            let (env, _) = self.get_entry(env, ls.clone(), cache_mode)?;
            let (args, _) = self.get_entry(args, ls.clone(), cache_mode)?;
            let (cont, _) = self.get_entry(cont, ls.clone(), cache_mode)?;
            let expr = ldon::Expr::Binop(op, env, args, cont);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Binop2(op, arg, cont) => {
            let (op, _) = self.get_entry(op, ls.clone(), cache_mode)?;
            let (arg, _) = self.get_entry(arg, ls.clone(), cache_mode)?;
            let (cont, _) = self.get_entry(cont, ls.clone(), cache_mode)?;
            let expr = ldon::Expr::Binop2(op, arg, cont);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::If(args, cont) => {
            let (args, _) = self.get_entry(args, ls.clone(), cache_mode)?;
            let (cont, _) = self.get_entry(cont, ls.clone(), cache_mode)?;
            let expr = ldon::Expr::If(args, cont);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Let(var, body, env, cont) => {
            let (var, _) = self.get_entry(var, ls.clone(), cache_mode)?;
            let (body, _) = self.get_entry(body, ls.clone(), cache_mode)?;
            let (env, _) = self.get_entry(env, ls.clone(), cache_mode)?;
            let (cont, _) = self.get_entry(cont, ls.clone(), cache_mode)?;
            let expr = ldon::Expr::LetRec(var, body, env, cont);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::LetRec(var, body, env, cont) => {
            let (var, _) = self.get_entry(var, ls.clone(), cache_mode)?;
            let (body, _) = self.get_entry(body, ls.clone(), cache_mode)?;
            let (env, _) = self.get_entry(env, ls.clone(), cache_mode)?;
            let (cont, _) = self.get_entry(cont, ls.clone(), cache_mode)?;
            let expr = ldon::Expr::LetRec(var, body, env, cont);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Emit(cont) => {
            let (cont, _) = self.get_entry(cont, ls.clone(), cache_mode)?;
            let expr = ldon::Expr::Emit(cont);
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Error => {
            let expr = ldon::Expr::Error;
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Dummy => {
            let expr = ldon::Expr::Dummy;
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          Expr::Terminal => {
            let expr = ldon::Expr::Terminal;
            let cid = expr.cid(&self.poseidon_cache);
            Ok((cid, expr))
          },
          _ => Err(LurkError::MalformedStore(ptr)),
        }?;
        self.cache_cid(ptr, cid, cache_mode)?;
        if let Some(ls) = ls {
          ls.borrow_mut().insert_expr(&self.poseidon_cache, expr);
        };
        Ok((cid, Entry::Expr(expr)))
      },
    }
  }

  pub fn cache_if_opaque(&mut self, ptr: Ptr<F>) -> Result<(), LurkError<F>> {
    if ptr.is_opaque() {
      self.hash_expr(ptr)?;
    }
    Ok(())
  }

  pub fn intern_string(
    &mut self,
    string: String,
  ) -> Result<Ptr<F>, LurkError<F>> {
    let mut ptr = self.intern_expr(Expr::StrNil)?;
    for c in string.chars().rev() {
      let char_ptr = self.intern_expr(Expr::Char(c))?;
      ptr = self.intern_expr(Expr::StrCons(char_ptr, ptr))?;
    }
    Ok(ptr)
  }

  pub fn intern_symbol(
    &mut self,
    sym: Vec<String>,
  ) -> Result<Ptr<F>, LurkError<F>> {
    let mut ptr = self.intern_expr(Expr::SymNil)?;
    for s in sym {
      let str_ptr = self.intern_string(s)?;
      ptr = self.intern_expr(Expr::SymCons(str_ptr, ptr))?;
    }
    Ok(ptr)
  }

  pub fn intern_syn(&mut self, syn: &Syn<F>) -> Result<Ptr<F>, LurkError<F>> {
    let mut ldon_store = ldon::Store::default();
    let cid = ldon_store.insert_syn(&self.poseidon_cache, syn);
    self.intern_cid(cid, &ldon_store)
  }

  // pub fn intern_strcons(
  //  &mut self,
  //  car: Ptr<F>,
  //  cdr: Ptr<F>,
  //) -> Result<Ptr<F>, LurkError<F>> {
  //  self.cache_if_opaque(&car)?;
  //  self.cache_if_opaque(&cdr)?;
  //  if let (Expr::Char(c), Expr::Str(s)) =
  //    (self.get_expr(&car)?, self.get_expr(&cdr)?)
  //  {
  //    let new_str = format!("{}{}", c, s);
  //    self.intern_str(&new_str)
  //  }
  //  else {
  //    Err(LurkError::Custom("strcons args must be Char and Str"))
  //  }
  //}

  // pub fn intern_str<T: AsRef<str>>(
  //  &mut self,
  //  str: T,
  //) -> Result<Ptr<F>, LurkError<F>> {
  //  // Hash string for side effect. This will cause all tails to be interned.
  //  self.hash_string_mut(str.as_ref())?;
  //  self.intern_str_aux(str)
  //}

  // pub fn intern_str_aux<T: AsRef<str>>(
  //  &mut self,
  //  str: T,
  //) -> Result<Ptr<F>, LurkError<F>> {
  //  if let Some(ptr) = self.strs.0.get(&str) {
  //    Ok(Ptr::index(ExprTag::Str, ptr.to_usize()))
  //  }
  //  else {
  //    let ptr = self.strs.0.get_or_intern(str);
  //    let ptr = Ptr::index(ExprTag::Str, ptr.to_usize());
  //    self.dehydrated.push(ptr);
  //    Ok(ptr)
  //  }
  //}

  // pub fn hash_string(&self, s: &str) -> Cid<F> {
  //  let mut cid = ldon::Expr::StrNil.cid(&self.poseidon_cache);
  //  for c in s.chars().rev() {
  //    let char_cid =
  //      Cid { tag: F::expr_tag(ExprTag::Char), val: F::from_char(c) };
  //    cid = ldon::Expr::StrCons(char_cid, cid).cid(&self.poseidon_cache);
  //  }
  //  cid
  //}

  // pub fn hash_string_mut<T: AsRef<str>>(
  //  &mut self,
  //  s: T,
  //) -> Result<Vec<(Ptr<F>, Cid<F>)>, LurkError<F>> {
  //  let mut res = Vec::new();
  //  let chars: Vec<char> = s.as_ref().chars().rev().collect();
  //  let mut i = 0;
  //  let mut cid = ldon::Expr::StrNil.cid(&self.poseidon_cache);
  //  let ptr = self.intern_str_aux("")?;
  //  res.push((ptr, cid));
  //  for c in &chars {
  //    i += 1;
  //    let char_cid =
  //      Cid { tag: F::expr_tag(ExprTag::Char), val: F::from_char(*c) };
  //    let substring = (&chars)[0..i].iter().collect::<String>();
  //    let ptr = self.intern_str_aux(&substring)?;
  //    cid = ldon::Expr::StrCons(char_cid, cid).cid(&self.poseidon_cache);
  //    self.cache_cid(ptr, cid, HashMode::Put)?;
  //    res.push((ptr, cid));
  //  }
  //  Ok(res)
  //}

  pub fn intern_expr(&mut self, expr: Expr<F>) -> Result<Ptr<F>, LurkError<F>> {
    let (ptr, inserted) = match expr {
      Expr::Cons(car, cdr) => {
        self.cache_if_opaque(car)?;
        self.cache_if_opaque(cdr)?;
        let (p, inserted) = self.conses.insert_full((car, cdr));
        Ok((Ptr::index(ExprTag::Cons, p), inserted))
      },
      Expr::Comm(secret, payload) => {
        self.cache_if_opaque(secret)?;
        self.cache_if_opaque(payload)?;
        let (p, inserted) = self.comms.insert_full((secret, payload));
        Ok((Ptr::index(ExprTag::Comm, p), inserted))
      },
      Expr::StrNil => Ok((Ptr::null(ExprTag::Str), false)),
      Expr::StrCons(car, cdr) => {
        self.cache_if_opaque(car)?;
        self.cache_if_opaque(cdr)?;
        let (p, inserted) = self.strs.insert_full((car, cdr));
        Ok((Ptr::index(ExprTag::Str, p), inserted))
      },
      Expr::SymNil => Ok((Ptr::null(ExprTag::Sym), false)),
      Expr::SymCons(car, cdr) => {
        self.cache_if_opaque(car)?;
        self.cache_if_opaque(cdr)?;
        let (p, inserted) = self.syms.insert_full((car, cdr));
        Ok((Ptr::index(ExprTag::Sym, p), inserted))
      },
      Expr::Keyword(sym) => {
        self.cache_if_opaque(sym)?;
        let (p, inserted) = self.keys.insert_full(sym);
        Ok((Ptr::index(ExprTag::Key, p), inserted))
      },
      Expr::Fun(arg, body, env) => {
        self.cache_if_opaque(arg)?;
        self.cache_if_opaque(body)?;
        self.cache_if_opaque(env)?;
        let (p, inserted) = self.funs.insert_full((arg, body, env));
        Ok((Ptr::index(ExprTag::Fun, p), inserted))
      },
      Expr::Num(num) => {
        let num = match num {
          Num::Scalar(s) => s.to_u64().map_or(num, Num::U64),
          Num::U64(_) => num,
        };
        Ok((Ptr::index(ExprTag::Num, self.nums.insert_full(num).0), false))
      },
      Expr::Char(c) => Ok((Ptr::index(ExprTag::Char, c as usize), false)),
      // FIXME: This breaks on 32-bit targets
      Expr::UInt(x) => {
        let x: usize = u64::from(x)
          .try_into()
          .map_err(|_| LurkError::Custom("uint to usize conversion error"))?;
        Ok((Ptr::index(ExprTag::U64, x), false))
      },
      Expr::Thunk(val, cont) => {
        self.cache_if_opaque(val)?;
        self.cache_if_opaque(cont)?;
        let (p, inserted) = self.thunks.insert_full((val, cont));
        Ok((Ptr::index(ExprTag::Fun, p), inserted))
      },
      Expr::Map(map) => {
        self.cache_if_opaque(map)?;
        let (p, inserted) = self.maps.insert_full(map);
        Ok((Ptr::index(ExprTag::Map, p), inserted))
      },
      Expr::Link(ctx, data) => {
        self.cache_if_opaque(ctx)?;
        self.cache_if_opaque(data)?;
        let (p, inserted) = self.links.insert_full((ctx, data));
        Ok((Ptr::index(ExprTag::Link, p), inserted))
      },
      Expr::Op1(op1) => Ok((Ptr::index(ExprTag::Op1, op1 as usize), false)),
      Expr::Op2(op2) => Ok((Ptr::index(ExprTag::Op1, op2 as usize), false)),
      Expr::Outermost => Ok((Ptr::null(ExprTag::Outermost), false)),
      Expr::Call(arg, env, cont) => {
        self.cache_if_opaque(arg)?;
        self.cache_if_opaque(env)?;
        self.cache_if_opaque(cont)?;
        let (p, inserted) = self.calls.insert_full((arg, env, cont));
        Ok((Ptr::index(ExprTag::Call, p), inserted))
      },
      Expr::Call0(env, cont) => {
        self.cache_if_opaque(env)?;
        self.cache_if_opaque(cont)?;
        let (p, inserted) = self.call0s.insert_full((env, cont));
        Ok((Ptr::index(ExprTag::Call0, p), inserted))
      },
      Expr::Call2(fun, env, cont) => {
        self.cache_if_opaque(fun)?;
        self.cache_if_opaque(env)?;
        self.cache_if_opaque(cont)?;
        let (p, inserted) = self.call2s.insert_full((fun, env, cont));
        Ok((Ptr::index(ExprTag::Call2, p), inserted))
      },
      Expr::Tail(env, cont) => {
        self.cache_if_opaque(env)?;
        self.cache_if_opaque(cont)?;
        let (p, inserted) = self.tails.insert_full((env, cont));
        Ok((Ptr::index(ExprTag::Tail, p), inserted))
      },
      Expr::Error => Ok((Ptr::null(ExprTag::Error), false)),
      Expr::Lookup(env, cont) => {
        self.cache_if_opaque(env)?;
        self.cache_if_opaque(cont)?;
        let (p, inserted) = self.lookups.insert_full((env, cont));
        Ok((Ptr::index(ExprTag::Lookup, p), inserted))
      },
      Expr::Unop(op, cont) => {
        self.cache_if_opaque(op)?;
        self.cache_if_opaque(cont)?;
        let (p, inserted) = self.unops.insert_full((op, cont));
        Ok((Ptr::index(ExprTag::Unop, p), inserted))
      },
      Expr::Binop(op, env, args, cont) => {
        self.cache_if_opaque(op)?;
        self.cache_if_opaque(env)?;
        self.cache_if_opaque(args)?;
        self.cache_if_opaque(cont)?;
        let (p, inserted) = self.binops.insert_full((op, env, args, cont));
        Ok((Ptr::index(ExprTag::Binop, p), inserted))
      },
      Expr::Binop2(op, arg, cont) => {
        self.cache_if_opaque(op)?;
        self.cache_if_opaque(arg)?;
        self.cache_if_opaque(cont)?;
        let (p, inserted) = self.binop2s.insert_full((op, arg, cont));
        Ok((Ptr::index(ExprTag::Binop2, p), inserted))
      },
      Expr::If(args, cont) => {
        self.cache_if_opaque(args)?;
        self.cache_if_opaque(cont)?;
        let (p, inserted) = self.ifs.insert_full((args, cont));
        Ok((Ptr::index(ExprTag::If, p), inserted))
      },
      Expr::Let(var, body, env, cont) => {
        self.cache_if_opaque(var)?;
        self.cache_if_opaque(body)?;
        self.cache_if_opaque(env)?;
        self.cache_if_opaque(cont)?;
        let (p, inserted) = self.lets.insert_full((var, body, env, cont));
        Ok((Ptr::index(ExprTag::Let, p), inserted))
      },
      Expr::LetRec(var, body, env, cont) => {
        self.cache_if_opaque(var)?;
        self.cache_if_opaque(body)?;
        self.cache_if_opaque(env)?;
        self.cache_if_opaque(cont)?;
        let (p, inserted) = self.let_recs.insert_full((var, body, env, cont));
        Ok((Ptr::index(ExprTag::LetRec, p), inserted))
      },
      Expr::Emit(cont) => {
        self.cache_if_opaque(cont)?;
        let (p, inserted) = self.emits.insert_full(cont);
        Ok((Ptr::index(ExprTag::Emit, p), inserted))
      },
      Expr::Dummy => Ok((Ptr::null(ExprTag::Dummy), false)),
      Expr::Terminal => Ok((Ptr::null(ExprTag::Terminal), false)),
    }?;
    if inserted {
      self.dehydrated.push(ptr);
    }
    Ok(ptr)
  }

  // pub fn nil(&mut self) -> Result<Ptr<F>, LurkError<F>> {
  //  self.intern_expr(Expr::ConsNil)
  //}

  pub fn cons(
    &mut self,
    car: Ptr<F>,
    cdr: Ptr<F>,
  ) -> Result<Ptr<F>, LurkError<F>> {
    self.intern_expr(Expr::Cons(car, cdr))
  }

  pub fn strnil(&mut self) -> Result<Ptr<F>, LurkError<F>> { todo!() }

  pub fn strcons(
    &mut self,
    car: Ptr<F>,
    cdr: Ptr<F>,
  ) -> Result<Ptr<F>, LurkError<F>> {
    todo!()
  }

  pub fn symnil(&mut self) -> Result<Ptr<F>, LurkError<F>> { todo!() }

  pub fn symcons(
    &mut self,
    car: Ptr<F>,
    cdr: Ptr<F>,
  ) -> Result<Ptr<F>, LurkError<F>> {
    todo!()
  }

  pub fn car_cdr(
    &mut self,
    ptr: Ptr<F>,
  ) -> Result<(Ptr<F>, Ptr<F>), LurkError<F>> {
    match self.get_expr(ptr)? {
      // Expr::ConsNil => Ok((self.nil()?, self.nil()?)),
      Expr::Cons(car, cdr) => Ok((car, cdr)),
      // TODO:
      // Expr::StrNil => Ok((self.strnil()?, self.strnil()?)),
      // Expr::StrCons(car, cdr) => Ok((car, cdr)),
      // Expr::SymNil => Ok((self.strnil()?, self.strnil()?)),
      // Expr::SymCons(car, cdr) => Ok((car, cdr)),
      _ => Err(LurkError::CantCarCdr(ptr)),
    }
  }

  pub fn to_ldon_store(
    &self,
    root: Ptr<F>,
  ) -> Result<ldon::Store<F>, LurkError<F>> {
    let ldon_store = Rc::new(RefCell::new(ldon::Store::default()));
    self.get_entry(root, Some(ldon_store.clone()), HashMode::Get)?;
    Ok(ldon_store.as_ref().clone().into_inner())
  }

  pub fn intern_ldon_store(
    ldon_store: &ldon::Store<F>,
  ) -> Result<Store<F>, LurkError<F>> {
    let mut store = Store::new();
    for cid in ldon_store.store.keys() {
      store.intern_cid(*cid, ldon_store)?;
    }
    Ok(store)
  }

  /// Fill the cache for Cid. Only Ptrs which have been inserted since
  /// last hydration will be hashed, so it is safe to call this incrementally.
  /// However, for best proving performance, we should call exactly once so all
  /// hashing can be batched, e.g. on the GPU.
  pub fn hydrate_cid_cache(&mut self) -> Result<(), LurkError<F>> {
    self.dehydrated.par_iter().try_for_each(|ptr| {
      self.hash_expr(*ptr)?;
      Ok(())
    })?;

    self.dehydrated.truncate(0);
    Ok(())
  }
  // fn as_lurk_boolean(&mut self, x: bool) -> Ptr<F> {
  //  if x {
  //    self.t()
  //  }
  //  else {
  //    self.nil()
  //  }
  //}
}

#[cfg(test)]
pub mod test {

  use blstrs::Scalar as Fr;
  use ldon::{
    parser::position::Pos,
    syntax::Syn,
  };

  use super::*;

  #[test]
  fn unit_intern_syn() {
    let mut store = Store::<Fr>::default();

    let syn = Syn::<Fr>::U64(Pos::No, 1u64);

    let ptr = store.intern_syn(&syn);
    println!("ptr {:?}", ptr);
    assert!(ptr.is_ok())
  }

  #[quickcheck]
  fn prop_intern_syn(syn1: Syn<Fr>) -> bool {
    let mut store1 = Store::<Fr>::default();
    store1.intern_syn(&syn1).expect("failed to intern syn");
    true
  }

  #[test]
  fn unit_intern_equality() {
    let mut store = Store::<Fr>::default();

    let u64_expr = Expr::Num(Num::U64(123));

    let u64_ptr1 = store.intern_expr(u64_expr.clone()).unwrap();
    let str_ptr1 = store.intern_string("pumpkin".to_string()).unwrap();

    let cons_expr = Expr::Cons(u64_ptr1, str_ptr1);

    let cons_ptr1 = store.intern_expr(cons_expr.clone()).unwrap();
    let u64_expr1 = store.get_expr(u64_ptr1).unwrap();
    let str_expr1 = store.get_expr(str_ptr1).unwrap();
    let cons_expr1 = store.get_expr(cons_ptr1).unwrap();

    let u64_ptr2 = store.intern_expr(u64_expr.clone()).unwrap();
    let str_ptr2 = store.intern_string("pumpkin".to_string()).unwrap();
    let cons_ptr2 = store.intern_expr(cons_expr.clone()).unwrap();
    let u64_expr2 = store.get_expr(u64_ptr2).unwrap();
    let str_expr2 = store.get_expr(str_ptr2).unwrap();
    let cons_expr2 = store.get_expr(cons_ptr2).unwrap();

    assert_eq!(u64_ptr1, u64_ptr2);
    assert_eq!(str_ptr1, str_ptr2);
    assert_eq!(cons_ptr1, cons_ptr2);

    assert_eq!(u64_expr, u64_expr1);
    assert_eq!(u64_expr1, u64_expr2);
    assert_eq!(str_expr1, str_expr2);
    assert_eq!(cons_expr1, cons_expr2);

    assert_eq!(cons_expr, cons_expr1);
    assert_eq!(cons_expr1, cons_expr2);
  }

  #[test]
  fn unit_intern_inequality() {
    let mut store = Store::<Fr>::default();

    let u64_ptr1 = store.intern_expr(Expr::Num(Num::U64(123))).unwrap();
    let str_ptr1 = store.intern_string("pumpkin".to_string()).unwrap();

    let u64_ptr2 = store.intern_expr(Expr::Num(Num::U64(1234))).unwrap();
    let str_ptr2 = store.intern_string("pumpkins".to_string()).unwrap();

    assert_ne!(u64_ptr1, u64_ptr2);
    assert_ne!(str_ptr1, str_ptr2);
  }

  #[quickcheck]
  fn prop_intern_syn_ptr_equality(syn1: Syn<Fr>) -> bool {
    let mut store1 = Store::<Fr>::default();
    let ptr1 = store1.intern_syn(&syn1).expect("failed to intern syn");
    let ptr2 = store1.intern_syn(&syn1).expect("failed to intern syn");
    ptr1 == ptr2
  }

  #[quickcheck]
  fn prop_intern_syn_ptr_equality2(syns: (Syn<Fr>, Syn<Fr>)) -> bool {
    let (syn1, syn2) = syns;
    let mut store1 = Store::<Fr>::default();
    let syn1_ptr1 = store1.intern_syn(&syn1).expect("failed to intern syn");
    let syn2_ptr1 = store1.intern_syn(&syn2).expect("failed to intern syn");
    let syn1_ptr2 = store1.intern_syn(&syn1).expect("failed to intern syn");
    let syn2_ptr2 = store1.intern_syn(&syn2).expect("failed to intern syn");
    syn1_ptr1 == syn1_ptr2 && syn2_ptr1 == syn2_ptr2
  }

  #[quickcheck]
  fn prop_intern_syn_ptr_inequality(syns: (Syn<Fr>, Syn<Fr>)) -> bool {
    let (syn1, syn2) = syns;
    let mut store1 = Store::<Fr>::default();
    let syn1_ptr = store1.intern_syn(&syn1).expect("failed to intern syn");
    let syn2_ptr = store1.intern_syn(&syn2).expect("failed to intern syn");
    if syn1 == syn2 {
      syn1_ptr == syn2_ptr
    }
    else {
      syn1_ptr != syn2_ptr
    }
  }

  #[quickcheck]
  fn prop_intern_ldon_store_consistency(syn1: Syn<Fr>) -> bool {
    let mut store1 = Store::new();
    let mut ldon_store1 = ldon::Store::<Fr>::default();
    let cid1 = ldon_store1.insert_syn(&store1.poseidon_cache, &syn1);
    let mut store1 = Store::<Fr>::intern_ldon_store(&ldon_store1)
      .expect("failed to intern ldon store");
    let ptr1 =
      store1.intern_cid(cid1, &ldon_store1).expect("failed to intern cid");
    let ldon_store2 =
      store1.to_ldon_store(ptr1).expect("failed to make ldon_store");
    let cid2 = store1.hash_expr(ptr1).expect("failed to hash ptr");

    ldon_store1 == ldon_store2 && cid1 == cid2
  }

  #[quickcheck]
  fn prop_cid_consistency(syn1: Syn<Fr>) -> bool {
    let mut ldon_store = ldon::Store::<Fr>::default();
    let mut store = Store::<Fr>::default();
    println!("syn1 {}", syn1);
    let cid1 = ldon_store.insert_syn(&store.poseidon_cache, &syn1);
    println!("cid1 {}", cid1);
    let ptr1 =
      store.intern_cid(cid1, &ldon_store).expect("failed to intern cid");
    println!("ptr1 {:?}", ptr1);
    let cid2 = store.hash_expr(ptr1).expect("failed to hash expr");
    cid1 == cid2
  }
}
