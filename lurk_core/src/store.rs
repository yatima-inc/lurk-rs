use ahash::RandomState;
use dashmap::DashMap;
use ldon::{
  cid::Cid,
  hash::HashConstants,
  op::{
    Op1,
    Op2,
  },
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
};

type IndexSet<K> = indexmap::IndexSet<K, RandomState>;

#[derive(Debug)]
pub struct Store<F: LurkField> {
  pub conses: IndexSet<(Ptr<F>, Ptr<F>)>,
  pub comms: IndexSet<(Ptr<F>, Ptr<F>)>,
  pub funs: IndexSet<(Ptr<F>, Ptr<F>, Ptr<F>)>,
  pub syms: IndexSet<(Ptr<F>, Ptr<F>)>,
  // Other sparse storage format without hashing is likely more efficient
  pub nums: IndexSet<Num<F>>,
  pub strs: IndexSet<(Ptr<F>, Ptr<F>)>,
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
      nums: Default::default(),
      funs: Default::default(),
      strs: Default::default(),
      thunks: Default::default(),
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

  pub fn insert_cid(
    &mut self,
    cid: Cid<F>,
    ldon_store: &ldon::Store<F>,
  ) -> Result<Ptr<F>, LurkError<F>> {
    use ldon::store::Entry;
    let entry = ldon_store.get_entry(cid).map_err(LurkError::LdonStore)?;
    let Cid { tag, val } = cid;
    match (tag.expr, entry) {
      (_, Entry::Opaque) => self.insert_opaque_cid(cid, false),
      (ExprTag::Cons, Entry::Expr(ldon::Expr::ConsNil)) if val == F::zero() => {
        self.insert_expr(Expr::ConsNil)
      },
      (ExprTag::Cons, Entry::Expr(ldon::Expr::Cons(car, cdr))) => {
        let car = self.insert_cid(car, ldon_store)?;
        let cdr = self.insert_cid(cdr, ldon_store)?;
        self.insert_expr(Expr::Cons(car, cdr))
      },
      (ExprTag::Comm, Entry::Expr(ldon::Expr::Comm(secret, payload))) => {
        let secret = self.insert_cid(secret, ldon_store)?;
        let payload = self.insert_cid(payload, ldon_store)?;
        self.insert_expr(Expr::Comm(secret, payload))
      },
      (ExprTag::Sym, Entry::Expr(ldon::Expr::SymNil)) if val == F::zero() => {
        self.insert_expr(Expr::SymNil)
      },
      (ExprTag::Sym, Entry::Expr(ldon::Expr::SymCons(head, tail))) => {
        let head = self.insert_cid(head, ldon_store)?;
        let tail = self.insert_cid(tail, ldon_store)?;
        self.insert_expr(Expr::SymCons(head, tail))
      },
      (ExprTag::Str, Entry::Expr(ldon::Expr::StrNil)) if val == F::zero() => {
        self.insert_expr(Expr::StrNil)
      },
      (ExprTag::Str, Entry::Expr(ldon::Expr::StrCons(head, tail))) => {
        let head = self.insert_cid(head, ldon_store)?;
        let tail = self.insert_cid(tail, ldon_store)?;
        self.insert_expr(Expr::StrCons(head, tail))
      },
      (ExprTag::Num, Entry::Expr(ldon::Expr::Num(f))) => {
        self.insert_expr(Expr::Num(Num::Scalar(f)))
      },
      (ExprTag::Char, Entry::Expr(ldon::Expr::Char(..))) => {
        let c = ldon_store.get_char(cid).map_err(LurkError::LdonStore)?;
        self.insert_expr(Expr::Char(c))
      },
      (ExprTag::Thunk, Entry::Expr(ldon::Expr::Thunk(val, cont))) => {
        let val = self.insert_cid(val, ldon_store)?;
        let cont = self.insert_cid(cont, ldon_store)?;
        self.insert_expr(Expr::Thunk(val, cont))
      },
      (ExprTag::Op1, Entry::Expr(ldon::Expr::Op1(op1))) => {
        self.insert_expr(Expr::Op1(op1))
      },
      (ExprTag::Op2, Entry::Expr(ldon::Expr::Op1(op1))) => {
        self.insert_expr(Expr::Op1(op1))
      },
      (ExprTag::Outermost, Entry::Expr(ldon::Expr::Outermost)) => {
        self.insert_expr(Expr::Outermost)
      },
      (ExprTag::Call, Entry::Expr(ldon::Expr::Call(arg, env, cont))) => {
        let arg = self.insert_cid(arg, ldon_store)?;
        let env = self.insert_cid(env, ldon_store)?;
        let cont = self.insert_cid(cont, ldon_store)?;
        self.insert_expr(Expr::Call(arg, env, cont))
      },
      (ExprTag::Call0, Entry::Expr(ldon::Expr::Call0(env, cont))) => {
        let env = self.insert_cid(env, ldon_store)?;
        let cont = self.insert_cid(cont, ldon_store)?;
        self.insert_expr(Expr::Call0(env, cont))
      },
      (ExprTag::Call2, Entry::Expr(ldon::Expr::Call2(fun, env, cont))) => {
        let fun = self.insert_cid(fun, ldon_store)?;
        let env = self.insert_cid(env, ldon_store)?;
        let cont = self.insert_cid(cont, ldon_store)?;
        self.insert_expr(Expr::Call2(fun, env, cont))
      },
      (ExprTag::Tail, Entry::Expr(ldon::Expr::Tail(env, cont))) => {
        let env = self.insert_cid(env, ldon_store)?;
        let cont = self.insert_cid(cont, ldon_store)?;
        self.insert_expr(Expr::Tail(env, cont))
      },
      (ExprTag::Error, Entry::Expr(ldon::Expr::Error)) => {
        self.insert_expr(Expr::Error)
      },
      (ExprTag::Unop, Entry::Expr(ldon::Expr::Unop(op1, cont))) => {
        let op1 = self.insert_cid(op1, ldon_store)?;
        let cont = self.insert_cid(cont, ldon_store)?;
        self.insert_expr(Expr::Unop(op1, cont))
      },
      (
        ExprTag::Binop,
        Entry::Expr(ldon::Expr::Binop(op2, env, args, cont)),
      ) => {
        let op2 = self.insert_cid(op2, ldon_store)?;
        let env = self.insert_cid(env, ldon_store)?;
        let args = self.insert_cid(args, ldon_store)?;
        let cont = self.insert_cid(cont, ldon_store)?;
        self.insert_expr(Expr::Binop(op2, env, args, cont))
      },
      (ExprTag::Binop2, Entry::Expr(ldon::Expr::Binop2(op2, env, cont))) => {
        let op2 = self.insert_cid(op2, ldon_store)?;
        let env = self.insert_cid(env, ldon_store)?;
        let cont = self.insert_cid(cont, ldon_store)?;
        self.insert_expr(Expr::Binop2(op2, env, cont))
      },
      (ExprTag::If, Entry::Expr(ldon::Expr::If(args, cont))) => {
        let args = self.insert_cid(args, ldon_store)?;
        let cont = self.insert_cid(cont, ldon_store)?;
        self.insert_expr(Expr::If(args, cont))
      },
      (ExprTag::Let, Entry::Expr(ldon::Expr::Let(var, body, env, cont))) => {
        let var = self.insert_cid(var, ldon_store)?;
        let body = self.insert_cid(body, ldon_store)?;
        let env = self.insert_cid(env, ldon_store)?;
        let cont = self.insert_cid(cont, ldon_store)?;
        self.insert_expr(Expr::Let(var, body, env, cont))
      },
      (
        ExprTag::LetRec,
        Entry::Expr(ldon::Expr::LetRec(var, body, env, cont)),
      ) => {
        let var = self.insert_cid(var, ldon_store)?;
        let body = self.insert_cid(body, ldon_store)?;
        let env = self.insert_cid(env, ldon_store)?;
        let cont = self.insert_cid(cont, ldon_store)?;
        self.insert_expr(Expr::LetRec(var, body, env, cont))
      },
      (ExprTag::Emit, Entry::Expr(ldon::Expr::Emit(cont))) => {
        let cont = self.insert_cid(cont, ldon_store)?;
        self.insert_expr(Expr::Emit(cont))
      },
      (ExprTag::Dummy, Entry::Expr(ldon::Expr::Dummy)) => {
        self.insert_expr(Expr::Dummy)
      },
      (ExprTag::Terminal, Entry::Expr(ldon::Expr::Terminal)) => {
        self.insert_expr(Expr::Terminal)
      },
      _ => Err(LurkError::MalformedLdonStore(cid, ldon_store.clone())),
    }
  }

  // Intern a potentially-opaque value. If the corresponding non-opaque value is
  // already known to the store, return the known value. If we set `force`
  // we ensure an opaque Ptr, even when the corresponding value is present in
  // the store,
  fn insert_opaque_cid(
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

  pub fn get_expr(&self, ptr: &Ptr<F>) -> Result<Expr<F>, LurkError<F>> {
    match (ptr.tag.expr, ptr.raw) {
      (_, RawPtr::Opaque(idx)) => {
        let cid =
          self.opaques.get_index(idx).ok_or(LurkError::UnknownPtr(*ptr))?;
        let ptr = self.get_opaque_cid(*cid)?;
        self.get_expr(&ptr)
      },
      (ExprTag::Cons, RawPtr::Null) => Ok(Expr::ConsNil),
      (ExprTag::Cons, RawPtr::Index(idx)) => {
        let (car, cdr) =
          self.conses.get_index(idx).ok_or(LurkError::UnknownPtr(*ptr))?;
        Ok(Expr::Cons(*car, *cdr))
      },
      (ExprTag::Comm, RawPtr::Index(idx)) => {
        let (secret, payload) =
          self.comms.get_index(idx).ok_or(LurkError::UnknownPtr(*ptr))?;
        Ok(Expr::Comm(*secret, *payload))
      },
      (ExprTag::Sym, RawPtr::Null) => Ok(Expr::SymNil),
      (ExprTag::Sym, RawPtr::Index(idx)) => {
        let (head, tail) =
          self.syms.get_index(idx).ok_or(LurkError::UnknownPtr(*ptr))?;
        Ok(Expr::SymCons(*head, *tail))
      },
      (ExprTag::Fun, RawPtr::Index(idx)) => {
        let (arg, body, env) =
          self.funs.get_index(idx).ok_or(LurkError::UnknownPtr(*ptr))?;
        Ok(Expr::Fun(*arg, *body, *env))
      },
      (ExprTag::Num, RawPtr::Index(idx)) => {
        let num =
          self.nums.get_index(idx).ok_or(LurkError::UnknownPtr(*ptr))?;
        Ok(Expr::Num(*num))
      },
      (ExprTag::Str, RawPtr::Null) => Ok(Expr::StrNil),
      (ExprTag::Str, RawPtr::Index(idx)) => {
        let (head, tail) =
          self.strs.get_index(idx).ok_or(LurkError::UnknownPtr(*ptr))?;
        Ok(Expr::StrCons(*head, *tail))
      },
      (ExprTag::Thunk, RawPtr::Index(idx)) => {
        let (val, cont) =
          self.thunks.get_index(idx).ok_or(LurkError::UnknownPtr(*ptr))?;
        Ok(Expr::Thunk(*val, *cont))
      },
      (ExprTag::Char, RawPtr::Index(idx)) => {
        let c =
          char::from_u32(idx as u32).ok_or(LurkError::UnknownPtr(*ptr))?;
        Ok(Expr::Char(c))
      },
      (ExprTag::U64, RawPtr::Index(idx)) => Ok(Expr::UInt((idx as u64).into())),
      (ExprTag::Op1, RawPtr::Index(idx)) => {
        let x = u16::try_from(idx)
          .map_err(|e| LurkError::InvalidOp1Ptr(*ptr, e.to_string()))?;
        Ok(Expr::Op1(
          Op1::try_from(x)
            .map_err(|e| LurkError::InvalidOp1Ptr(*ptr, e.to_string()))?,
        ))
      },
      (ExprTag::Op2, RawPtr::Index(idx)) => {
        let x = u16::try_from(idx)
          .map_err(|e| LurkError::InvalidOp2Ptr(*ptr, e.to_string()))?;
        Ok(Expr::Op2(
          Op2::try_from(x)
            .map_err(|e| LurkError::InvalidOp2Ptr(*ptr, e.to_string()))?,
        ))
      },
      (ExprTag::Outermost, RawPtr::Null) => Ok(Expr::Outermost),
      (ExprTag::Call, RawPtr::Index(idx)) => {
        let (arg, env, cont) =
          self.calls.get_index(idx).ok_or(LurkError::UnknownPtr(*ptr))?;
        Ok(Expr::Call(*arg, *env, *cont))
      },
      (ExprTag::Call0, RawPtr::Index(idx)) => {
        let (env, cont) =
          self.call0s.get_index(idx).ok_or(LurkError::UnknownPtr(*ptr))?;
        Ok(Expr::Call0(*env, *cont))
      },
      (ExprTag::Call2, RawPtr::Index(idx)) => {
        let (fun, env, cont) =
          self.call2s.get_index(idx).ok_or(LurkError::UnknownPtr(*ptr))?;
        Ok(Expr::Call2(*fun, *env, *cont))
      },
      (ExprTag::Tail, RawPtr::Index(idx)) => {
        let (env, cont) =
          self.tails.get_index(idx).ok_or(LurkError::UnknownPtr(*ptr))?;
        Ok(Expr::Tail(*env, *cont))
      },
      (ExprTag::Lookup, RawPtr::Index(idx)) => {
        let (env, cont) =
          self.lookups.get_index(idx).ok_or(LurkError::UnknownPtr(*ptr))?;
        Ok(Expr::Lookup(*env, *cont))
      },
      (ExprTag::Unop, RawPtr::Index(idx)) => {
        let (op, cont) =
          self.unops.get_index(idx).ok_or(LurkError::UnknownPtr(*ptr))?;
        Ok(Expr::Unop(*op, *cont))
      },
      (ExprTag::Binop, RawPtr::Index(idx)) => {
        let (op, env, args, cont) =
          self.binops.get_index(idx).ok_or(LurkError::UnknownPtr(*ptr))?;
        Ok(Expr::Binop(*op, *env, *args, *cont))
      },
      (ExprTag::Binop2, RawPtr::Index(idx)) => {
        let (op, arg, cont) =
          self.binop2s.get_index(idx).ok_or(LurkError::UnknownPtr(*ptr))?;
        Ok(Expr::Binop2(*op, *arg, *cont))
      },
      (ExprTag::If, RawPtr::Index(idx)) => {
        let (args, cont) =
          self.ifs.get_index(idx).ok_or(LurkError::UnknownPtr(*ptr))?;
        Ok(Expr::If(*args, *cont))
      },
      (ExprTag::Let, RawPtr::Index(idx)) => {
        let (var, body, env, cont) =
          self.lets.get_index(idx).ok_or(LurkError::UnknownPtr(*ptr))?;
        Ok(Expr::Let(*var, *body, *env, *cont))
      },
      (ExprTag::LetRec, RawPtr::Index(idx)) => {
        let (var, body, env, cont) =
          self.let_recs.get_index(idx).ok_or(LurkError::UnknownPtr(*ptr))?;
        Ok(Expr::LetRec(*var, *body, *env, *cont))
      },
      (ExprTag::Emit, RawPtr::Index(idx)) => {
        let cont =
          self.emits.get_index(idx).ok_or(LurkError::UnknownPtr(*ptr))?;
        Ok(Expr::Emit(*cont))
      },
      (ExprTag::Error, RawPtr::Null) => Ok(Expr::Error),
      (ExprTag::Terminal, RawPtr::Null) => Ok(Expr::Terminal),
      (ExprTag::Dummy, RawPtr::Null) => Ok(Expr::Dummy),
      _ => Err(LurkError::UnknownPtr(*ptr)),
    }
  }

  pub fn hash_expr(&self, ptr: &Ptr<F>) -> Result<Cid<F>, LurkError<F>> {
    self.hash_expr_aux(ptr, HashMode::Put)
  }

  // Get hash for expr, but only if it already exists. This should never cause
  // create_scalar_ptr to be called. Use this after the cache has been
  // hydrated. NOTE: because dashmap::entry can deadlock, it is important not to
  // call hash_expr in nested call graphs which might trigger that behavior.
  // This discovery is what led to get_expr_hash
  // TODO: investigate whether dashmap::try_entry fixes this
  pub fn get_expr_hash(&self, ptr: &Ptr<F>) -> Result<Cid<F>, LurkError<F>> {
    self.hash_expr_aux(ptr, HashMode::Get)
  }

  pub fn hash_expr_aux(
    &self,
    ptr: &Ptr<F>,
    cache_mode: HashMode,
  ) -> Result<Cid<F>, LurkError<F>> {
    let cid = match ptr.raw {
      RawPtr::Opaque(idx) => {
        Ok(*(self.opaques.get_index(idx).ok_or(LurkError::UnknownPtr(*ptr))?))
      },
      _ => match self.get_expr(ptr)? {
        Expr::ConsNil => Ok(ldon::Expr::ConsNil.cid(&self.poseidon_cache)),
        Expr::Cons(car, cdr) => {
          let car = self.hash_expr_aux(&car, cache_mode)?;
          let cdr = self.hash_expr_aux(&cdr, cache_mode)?;
          Ok(ldon::Expr::Cons(car, cdr).cid(&self.poseidon_cache))
        },
        Expr::Comm(secret, payload) => {
          let secret = self.hash_expr_aux(&secret, cache_mode)?;
          let payload = self.hash_expr_aux(&payload, cache_mode)?;
          Ok(ldon::Expr::Comm(secret, payload).cid(&self.poseidon_cache))
        },
        Expr::SymNil => Ok(ldon::Expr::SymNil.cid(&self.poseidon_cache)),
        Expr::SymCons(head, tail) => {
          let head = self.hash_expr_aux(&head, cache_mode)?;
          let tail = self.hash_expr_aux(&tail, cache_mode)?;
          Ok(ldon::Expr::SymCons(head, tail).cid(&self.poseidon_cache))
        },
        Expr::Fun(arg, body, env) => {
          let arg = self.hash_expr_aux(&arg, cache_mode)?;
          let body = self.hash_expr_aux(&body, cache_mode)?;
          let env = self.hash_expr_aux(&env, cache_mode)?;
          Ok(ldon::Expr::Fun(arg, body, env).cid(&self.poseidon_cache))
        },
        Expr::Num(num) => {
          Ok(ldon::Expr::Num(num.into_scalar()).cid(&self.poseidon_cache))
        },

        Expr::StrNil => Ok(ldon::Expr::StrNil.cid(&self.poseidon_cache)),
        Expr::StrCons(head, tail) => {
          let head = self.hash_expr_aux(&head, cache_mode)?;
          let tail = self.hash_expr_aux(&tail, cache_mode)?;
          Ok(ldon::Expr::StrCons(head, tail).cid(&self.poseidon_cache))
        },
        Expr::Thunk(val, cont) => {
          let val = self.hash_expr_aux(&val, cache_mode)?;
          let cont = self.hash_expr_aux(&cont, cache_mode)?;
          Ok(ldon::Expr::Thunk(val, cont).cid(&self.poseidon_cache))
        },
        Expr::Char(c) => {
          Ok(ldon::Expr::Char(F::from_char(c)).cid(&self.poseidon_cache))
        },
        Expr::UInt(uint) => Ok(
          ldon::Expr::U64(F::from_u64(uint.into())).cid(&self.poseidon_cache),
        ),
        Expr::Op1(op1) => Ok(ldon::Expr::Op1(op1).cid(&self.poseidon_cache)),
        Expr::Op2(op2) => Ok(ldon::Expr::Op2(op2).cid(&self.poseidon_cache)),
        Expr::Call0(env, cont) => {
          let env = self.hash_expr_aux(&env, cache_mode)?;
          let cont = self.hash_expr_aux(&cont, cache_mode)?;
          Ok(ldon::Expr::Call0(env, cont).cid(&self.poseidon_cache))
        },
        Expr::Call2(fun, env, cont) => {
          let fun = self.hash_expr_aux(&fun, cache_mode)?;
          let env = self.hash_expr_aux(&env, cache_mode)?;
          let cont = self.hash_expr_aux(&cont, cache_mode)?;
          Ok(ldon::Expr::Call2(fun, env, cont).cid(&self.poseidon_cache))
        },
        Expr::Tail(env, cont) => {
          let env = self.hash_expr_aux(&env, cache_mode)?;
          let cont = self.hash_expr_aux(&cont, cache_mode)?;
          Ok(ldon::Expr::Tail(env, cont).cid(&self.poseidon_cache))
        },
        Expr::Lookup(env, cont) => {
          let env = self.hash_expr_aux(&env, cache_mode)?;
          let cont = self.hash_expr_aux(&cont, cache_mode)?;
          Ok(ldon::Expr::Lookup(env, cont).cid(&self.poseidon_cache))
        },
        Expr::Unop(op, cont) => {
          let op = self.hash_expr_aux(&op, cache_mode)?;
          let cont = self.hash_expr_aux(&cont, cache_mode)?;
          Ok(ldon::Expr::Unop(op, cont).cid(&self.poseidon_cache))
        },
        Expr::Binop(op, env, args, cont) => {
          let op = self.hash_expr_aux(&op, cache_mode)?;
          let env = self.hash_expr_aux(&env, cache_mode)?;
          let args = self.hash_expr_aux(&args, cache_mode)?;
          let cont = self.hash_expr_aux(&cont, cache_mode)?;
          Ok(ldon::Expr::Binop(op, env, args, cont).cid(&self.poseidon_cache))
        },
        Expr::Binop2(op, arg, cont) => {
          let op = self.hash_expr_aux(&op, cache_mode)?;
          let arg = self.hash_expr_aux(&arg, cache_mode)?;
          let cont = self.hash_expr_aux(&cont, cache_mode)?;
          Ok(ldon::Expr::Binop2(op, arg, cont).cid(&self.poseidon_cache))
        },
        Expr::If(args, cont) => {
          let args = self.hash_expr_aux(&args, cache_mode)?;
          let cont = self.hash_expr_aux(&cont, cache_mode)?;
          Ok(ldon::Expr::If(args, cont).cid(&self.poseidon_cache))
        },
        Expr::Let(var, body, env, cont) => {
          let var = self.hash_expr_aux(&var, cache_mode)?;
          let body = self.hash_expr_aux(&body, cache_mode)?;
          let env = self.hash_expr_aux(&env, cache_mode)?;
          let cont = self.hash_expr_aux(&cont, cache_mode)?;
          Ok(ldon::Expr::LetRec(var, body, env, cont).cid(&self.poseidon_cache))
        },
        Expr::LetRec(var, body, env, cont) => {
          let var = self.hash_expr_aux(&var, cache_mode)?;
          let body = self.hash_expr_aux(&body, cache_mode)?;
          let env = self.hash_expr_aux(&env, cache_mode)?;
          let cont = self.hash_expr_aux(&cont, cache_mode)?;
          Ok(ldon::Expr::LetRec(var, body, env, cont).cid(&self.poseidon_cache))
        },
        Expr::Emit(cont) => {
          let cont = self.hash_expr_aux(&cont, cache_mode)?;
          Ok(ldon::Expr::Emit(cont).cid(&self.poseidon_cache))
        },
        Expr::Error => Ok(ldon::Expr::Error.cid(&self.poseidon_cache)),
        Expr::Dummy => Ok(ldon::Expr::Dummy.cid(&self.poseidon_cache)),
        Expr::Terminal => Ok(ldon::Expr::Terminal.cid(&self.poseidon_cache)),
        _ => Err(LurkError::MalformedStore(*ptr)),
      },
    }?;
    self.cache_cid(*ptr, cid, cache_mode)?;
    Ok(cid)
  }

  pub fn cache_if_opaque(&mut self, ptr: &Ptr<F>) -> Result<(), LurkError<F>> {
    if ptr.is_opaque() {
      self.hash_expr(ptr)?;
    }
    Ok(())
  }

  pub fn insert_expr(&mut self, expr: Expr<F>) -> Result<Ptr<F>, LurkError<F>> {
    let (ptr, inserted) = match expr {
      Expr::ConsNil => Ok((Ptr::null(ExprTag::Cons), false)),
      Expr::Cons(car, cdr) => {
        self.cache_if_opaque(&car)?;
        self.cache_if_opaque(&cdr)?;
        let (p, inserted) = self.conses.insert_full((car, cdr));
        Ok((Ptr::index(ExprTag::Cons, p), inserted))
      },
      Expr::Comm(secret, payload) => {
        self.cache_if_opaque(&secret)?;
        self.cache_if_opaque(&payload)?;
        let (p, inserted) = self.conses.insert_full((secret, payload));
        Ok((Ptr::index(ExprTag::Comm, p), inserted))
      },
      Expr::StrNil => Ok((Ptr::null(ExprTag::Str), false)),
      Expr::StrCons(head, tail) => {
        self.cache_if_opaque(&head)?;
        self.cache_if_opaque(&tail)?;
        let (p, inserted) = self.conses.insert_full((head, tail));
        Ok((Ptr::index(ExprTag::Str, p), inserted))
      },
      Expr::SymNil => Ok((Ptr::null(ExprTag::Sym), false)),
      Expr::SymCons(head, tail) => {
        self.cache_if_opaque(&head)?;
        self.cache_if_opaque(&tail)?;
        let (p, inserted) = self.conses.insert_full((head, tail));
        Ok((Ptr::index(ExprTag::Sym, p), inserted))
      },
      Expr::Fun(arg, body, env) => {
        self.cache_if_opaque(&arg)?;
        self.cache_if_opaque(&body)?;
        self.cache_if_opaque(&env)?;
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
        self.cache_if_opaque(&val)?;
        self.cache_if_opaque(&cont)?;
        let (p, inserted) = self.thunks.insert_full((val, cont));
        Ok((Ptr::index(ExprTag::Fun, p), inserted))
      },
      Expr::Op1(op1) => Ok((Ptr::index(ExprTag::Op1, op1 as usize), false)),
      Expr::Op2(op2) => Ok((Ptr::index(ExprTag::Op1, op2 as usize), false)),
      Expr::Outermost => Ok((Ptr::null(ExprTag::Outermost), false)),
      Expr::Call(arg, env, cont) => {
        self.cache_if_opaque(&arg)?;
        self.cache_if_opaque(&env)?;
        self.cache_if_opaque(&cont)?;
        let (p, inserted) = self.calls.insert_full((arg, env, cont));
        Ok((Ptr::index(ExprTag::Call, p), inserted))
      },
      Expr::Call0(env, cont) => {
        self.cache_if_opaque(&env)?;
        self.cache_if_opaque(&cont)?;
        let (p, inserted) = self.call0s.insert_full((env, cont));
        Ok((Ptr::index(ExprTag::Call0, p), inserted))
      },
      Expr::Call2(fun, env, cont) => {
        self.cache_if_opaque(&fun)?;
        self.cache_if_opaque(&env)?;
        self.cache_if_opaque(&cont)?;
        let (p, inserted) = self.call2s.insert_full((fun, env, cont));
        Ok((Ptr::index(ExprTag::Call2, p), inserted))
      },
      Expr::Tail(env, cont) => {
        self.cache_if_opaque(&env)?;
        self.cache_if_opaque(&cont)?;
        let (p, inserted) = self.tails.insert_full((env, cont));
        Ok((Ptr::index(ExprTag::Tail, p), inserted))
      },
      Expr::Error => Ok((Ptr::null(ExprTag::Error), false)),
      Expr::Lookup(env, cont) => {
        self.cache_if_opaque(&env)?;
        self.cache_if_opaque(&cont)?;
        let (p, inserted) = self.lookups.insert_full((env, cont));
        Ok((Ptr::index(ExprTag::Lookup, p), inserted))
      },
      Expr::Unop(op, cont) => {
        self.cache_if_opaque(&op)?;
        self.cache_if_opaque(&cont)?;
        let (p, inserted) = self.unops.insert_full((op, cont));
        Ok((Ptr::index(ExprTag::Unop, p), inserted))
      },
      Expr::Binop(op, env, args, cont) => {
        self.cache_if_opaque(&op)?;
        self.cache_if_opaque(&env)?;
        self.cache_if_opaque(&args)?;
        self.cache_if_opaque(&cont)?;
        let (p, inserted) = self.binops.insert_full((op, env, args, cont));
        Ok((Ptr::index(ExprTag::Binop, p), inserted))
      },
      Expr::Binop2(op, arg, cont) => {
        self.cache_if_opaque(&op)?;
        self.cache_if_opaque(&arg)?;
        self.cache_if_opaque(&cont)?;
        let (p, inserted) = self.binop2s.insert_full((op, arg, cont));
        Ok((Ptr::index(ExprTag::Binop2, p), inserted))
      },
      Expr::If(args, cont) => {
        self.cache_if_opaque(&args)?;
        self.cache_if_opaque(&cont)?;
        let (p, inserted) = self.ifs.insert_full((args, cont));
        Ok((Ptr::index(ExprTag::If, p), inserted))
      },
      Expr::Let(var, body, env, cont) => {
        self.cache_if_opaque(&var)?;
        self.cache_if_opaque(&body)?;
        self.cache_if_opaque(&env)?;
        self.cache_if_opaque(&cont)?;
        let (p, inserted) = self.lets.insert_full((var, body, env, cont));
        Ok((Ptr::index(ExprTag::Let, p), inserted))
      },
      Expr::LetRec(var, body, env, cont) => {
        self.cache_if_opaque(&var)?;
        self.cache_if_opaque(&body)?;
        self.cache_if_opaque(&env)?;
        self.cache_if_opaque(&cont)?;
        let (p, inserted) = self.lets.insert_full((var, body, env, cont));
        Ok((Ptr::index(ExprTag::LetRec, p), inserted))
      },
      Expr::Emit(cont) => {
        self.cache_if_opaque(&cont)?;
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

  pub fn insert_string(
    &mut self,
    string: String,
  ) -> Result<Ptr<F>, LurkError<F>> {
    let mut ptr = self.insert_expr(Expr::StrNil)?;
    for c in string.chars().rev() {
      let char_ptr = self.insert_expr(Expr::Char(c))?;
      ptr = self.insert_expr(Expr::StrCons(char_ptr, ptr))?;
    }
    Ok(ptr)
  }

  pub fn insert_symbol(
    &mut self,
    sym: Vec<String>,
  ) -> Result<Ptr<F>, LurkError<F>> {
    let mut ptr = self.insert_expr(Expr::SymNil)?;
    for s in sym {
      let str_ptr = self.insert_string(s)?;
      ptr = self.insert_expr(Expr::SymCons(str_ptr, ptr))?;
    }
    Ok(ptr)
  }

  pub fn nil(&mut self) -> Result<Ptr<F>, LurkError<F>> {
    self.insert_expr(Expr::ConsNil)
  }

  pub fn cons(
    &mut self,
    car: Ptr<F>,
    cdr: Ptr<F>,
  ) -> Result<Ptr<F>, LurkError<F>> {
    self.insert_expr(Expr::Cons(car, cdr))
  }

  pub fn strnil(&mut self) -> Result<Ptr<F>, LurkError<F>> {
    self.insert_expr(Expr::StrNil)
  }

  pub fn strcons(
    &mut self,
    car: Ptr<F>,
    cdr: Ptr<F>,
  ) -> Result<Ptr<F>, LurkError<F>> {
    self.insert_expr(Expr::StrCons(car, cdr))
  }

  pub fn symnil(&mut self) -> Result<Ptr<F>, LurkError<F>> {
    self.insert_expr(Expr::SymNil)
  }

  pub fn symcons(
    &mut self,
    car: Ptr<F>,
    cdr: Ptr<F>,
  ) -> Result<Ptr<F>, LurkError<F>> {
    self.insert_expr(Expr::SymCons(car, cdr))
  }

  pub fn car_cdr(
    &mut self,
    ptr: &Ptr<F>,
  ) -> Result<(Ptr<F>, Ptr<F>), LurkError<F>> {
    match self.get_expr(ptr)? {
      Expr::ConsNil => Ok((self.nil()?, self.nil()?)),
      Expr::Cons(car, cdr) => Ok((car, cdr)),
      Expr::StrNil => Ok((self.strnil()?, self.strnil()?)),
      Expr::StrCons(car, cdr) => Ok((car, cdr)),
      Expr::SymNil => Ok((self.strnil()?, self.strnil()?)),
      Expr::SymCons(car, cdr) => Ok((car, cdr)),
      _ => Err(LurkError::CantCarCdr(*ptr)),
    }
  }

  /// Fill the cache for Cid. Only Ptrs which have been inserted since
  /// last hydration will be hashed, so it is safe to call this incrementally.
  /// However, for best proving performance, we should call exactly once so all
  /// hashing can be batched, e.g. on the GPU.
  pub fn hydrate_cid_cache(&mut self) -> Result<(), LurkError<F>> {
    self.dehydrated.par_iter().try_for_each(|ptr| {
      self.hash_expr(ptr)?;
      Ok(())
    })?;

    self.dehydrated.truncate(0);
    Ok(())
  }
}

#[cfg(test)]
pub mod test {

  use super::*;
  use blstrs::Scalar as Fr;

  #[test]
  fn equality() {
    let store = Store::<Fr>::default();

    let u64_expr = Expr::Num(Num::U64(123));

    let u64_ptr1 = store.insert_expr(u64_expr).expect("failed to insert u64");
    let str_ptr1 = store.insert_string("pumpkin".to_string()).expect("failed to insert str");

    let cons_expr = Expr::Cons(u64_ptr1, str_ptr1);

    let cons_ptr1 = store.insert_expr(cons_expr).expect("failed to insert cons");
    let u64_expr1 = store.get_expr(&u64_ptr1).expect("failed to get u64");
    let str_expr1 = store.get_expr(&str_ptr1).expect("failed to get str");
    let cons_expr1 = store.get_expr(&cons_ptr1).expect("failed to get cons");

    let u64_ptr2 = store.insert_expr(u64_expr).expect("failed to insert u64");
    let str_ptr2 = store.insert_string("pumpkin".to_string()).expect("failed to insert str");
    let cons_ptr2 = store.insert_expr(cons_expr).expect("failed to insert cons");
    let u64_expr2 = store.get_expr(&u64_ptr2).expect("failed to get u64");
    let str_expr2 = store.get_expr(&str_ptr2).expect("failed to get str");
    let cons_expr2 = store.get_expr(&cons_ptr2).expect("failed to get cons");

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
}
