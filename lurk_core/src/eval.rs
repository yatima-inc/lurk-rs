use lurk_ff::{
  ExprTag,
  LurkField,
};

use crate::{
  error::LurkError,
  eval::{
    control::Control,
    hash_witness::{
      ConsName,
      HashWitness,
    },
    status::Status,
    witness::Witness,
  },
  expr::Expr,
  num::Num,
  ptr::Ptr,
  store::Store,
};

pub mod cons;
pub mod control;
pub mod evaluator;
pub mod frame;
pub mod hash_witness;
pub mod io;
pub mod status;
pub mod witness;

fn reduce<F: LurkField>(
  expr: Ptr<F>,
  env: Ptr<F>,
  cont: Ptr<F>,
  store: &mut Store<F>,
) -> Result<(Ptr<F>, Ptr<F>, Ptr<F>, Witness<F>), LurkError<F>> {
  let (ctrl, witness) = reduce_with_witness(expr, env, cont, store)?;
  let (new_expr, new_env, new_cont) = ctrl.into_results();

  Ok((new_expr, new_env, new_cont, witness))
}

fn reduce_with_witness<F: LurkField>(
  expr: Ptr<F>,
  env: Ptr<F>,
  cont: Ptr<F>,
  store: &mut Store<F>,
) -> Result<(Control<F>, Witness<F>), LurkError<F>> {
  let hash_witness = &mut HashWitness::<F>::new_dummy();
  // let mut closure_to_extend = None;
  // let control = if cont.tag() == ContTag::Terminal {
  //  Control::Return(expr, env, cont)
  //};
  todo!()
}
