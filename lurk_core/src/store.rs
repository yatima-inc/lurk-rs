use std::{
  fmt,
  hash::Hash,
  marker::PhantomData,
};

use ahash::RandomState;
use dashmap::DashMap;
use ldon::PoseidonCache;
use lurk_ff::{
  FWrap,
  LurkField,
  Tag,
};
use once_cell::sync::OnceCell;
use string_interner::symbol::{
  Symbol,
  SymbolUsize,
};

use crate::ptr::{
  Ptr,
  RawPtr,
};

type IndexSet<K> = indexmap::IndexSet<K, RandomState>;

#[derive(Debug)]
struct StringSet(
  string_interner::StringInterner<
    string_interner::backend::BufferBackend<SymbolUsize>,
    ahash::RandomState,
  >,
);

impl Default for StringSet {
  fn default() -> Self { StringSet(string_interner::StringInterner::new()) }
}

#[derive(Debug)]
pub struct Store<F: LurkField> {
  pub cons_store: IndexSet<(Ptr<F>, Ptr<F>)>,
  pub comm_store: IndexSet<(FWrap<F>, Ptr<F>)>,
  // fun_store: IndexSet<(Ptr<F>, Ptr<F>, Ptr<F>)>,

  // sym_store: StringSet,

  //// Other sparse storage format without hashing is likely more efficient
  // pub(crate) num_store: IndexSet<Num<F>>,

  // str_store: StringSet,
  // thunk_store: IndexSet<Thunk<F>>,
  // call0_store: IndexSet<(Ptr<F>, ContPtr<F>)>,
  // call_store: IndexSet<(Ptr<F>, Ptr<F>, ContPtr<F>)>,
  // call2_store: IndexSet<(Ptr<F>, Ptr<F>, ContPtr<F>)>,
  // tail_store: IndexSet<(Ptr<F>, ContPtr<F>)>,
  // lookup_store: IndexSet<(Ptr<F>, ContPtr<F>)>,
  // unop_store: IndexSet<(Op1, ContPtr<F>)>,
  // binop_store: IndexSet<(Op2, Ptr<F>, Ptr<F>, ContPtr<F>)>,
  // binop2_store: IndexSet<(Op2, Ptr<F>, ContPtr<F>)>,
  // if_store: IndexSet<(Ptr<F>, ContPtr<F>)>,
  // let_store: IndexSet<(Ptr<F>, Ptr<F>, Ptr<F>, ContPtr<F>)>,
  // let_rec_store: IndexSet<(Ptr<F>, Ptr<F>, Ptr<F>, ContPtr<F>)>,
  // emit_store: IndexSet<ContPtr<F>>,

  // opaque_map: DashMap<Ptr<F>, ScalarPtr<F>>,
  ///// Holds a mapping of ScalarPtr -> Ptr for reverse lookups
  // pub(crate) scalar_ptr_map: DashMap<ScalarPtr<F>, Ptr<F>, RandomState>,
  ///// Holds a mapping of ScalarPtr -> ContPtr<F> for reverse lookups
  // scalar_ptr_cont_map: DashMap<ScalarContPtr<F>, ContPtr<F>, RandomState>,

  ///// Caches poseidon hashes
  // poseidon_cache: PoseidonCache<F>,
  ///// Contains Ptrs which have not yet been hydrated.
  // dehydrated: Vec<Ptr<F>>,
  // dehydrated_cont: Vec<ContPtr<F>>,
  // opaque_raw_ptr_count: usize,

  // pub(crate) lurk_package: Package,
}
