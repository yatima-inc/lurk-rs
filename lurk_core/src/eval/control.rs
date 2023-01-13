use lurk_ff::{
  ExprTag,
  LurkField,
};

use crate::{
  error::LurkError,
  eval::{
    hash_witness::ConsName,
    witness::Witness,
  },
  expr::Expr,
  ptr::Ptr,
  store::Store,
};

#[derive(Debug, Clone)]
pub enum Control<F: LurkField> {
  Return(Ptr<F>, Ptr<F>, Ptr<F>),
  MakeThunk(Ptr<F>, Ptr<F>, Ptr<F>),
  ApplyContinuation(Ptr<F>, Ptr<F>, Ptr<F>),
}

impl<F: LurkField> Control<F> {
  pub fn as_results(&self) -> (&Ptr<F>, &Ptr<F>, &Ptr<F>) {
    match self {
      Self::Return(expr, env, cont) => (expr, env, cont),
      Self::MakeThunk(expr, env, cont) => (expr, env, cont),
      Self::ApplyContinuation(expr, env, cont) => (expr, env, cont),
    }
  }

  pub fn into_results(self) -> (Ptr<F>, Ptr<F>, Ptr<F>) {
    match self {
      Self::Return(expr, env, cont) => (expr, env, cont),
      Self::MakeThunk(expr, env, cont) => (expr, env, cont),
      Self::ApplyContinuation(expr, env, cont) => (expr, env, cont),
    }
  }

  pub fn is_return(&self) -> bool { matches!(self, Self::Return(_, _, _)) }

  pub fn is_make_thunk(&self) -> bool {
    matches!(self, Self::MakeThunk(_, _, _))
  }

  pub fn is_apply_continuation(&self) -> bool {
    matches!(self, Self::ApplyContinuation(_, _, _))
  }

  // Returns (Expression::Thunk, Expression::Env, Continuation)
  pub fn make_thunk(
    self,
    store: &mut Store<F>,
    _witness: &mut Witness<F>,
  ) -> Result<Self, LurkError<F>> {
    if !self.is_make_thunk() {
      return Ok(self);
    }

    let (result, env, cont) = self.into_results();

    if let ExprTag::Thunk = result.tag.expr {
      unreachable!("make_thunk should never be called with a thunk");
    };

    match cont.tag.expr {
      ExprTag::Tail => match store.get_expr(cont)? {
        Expr::Tail(saved_env, continuation) => {
          let thunk = store.intern_expr(Expr::Thunk(result, continuation))?;
          let dummy = store.intern_expr(Expr::Dummy)?;
          Ok(Control::Return(thunk, saved_env, dummy))
        },
        _ => unreachable!(),
      },
      // If continuation is outermost, we don't actually make a thunk. Instead,
      // we signal that this is the terminal result by returning a
      // Terminal continuation.
      ExprTag::Outermost => {
        let terminal = store.intern_expr(Expr::Terminal)?;
        Ok(Control::Return(result, env, terminal))
      },
      _ => {
        let thunk = store.intern_expr(Expr::Thunk(result, cont))?;
        let dummy = store.intern_expr(Expr::Dummy)?;
        Ok(Control::Return(thunk, env, dummy))
      },
    }
  }

  pub fn make_tail_continuation(
    env: Ptr<F>,
    continuation: Ptr<F>,
    store: &mut Store<F>,
  ) -> Result<Ptr<F>, LurkError<F>> {
    // Result must be either a Tail or Outermost continuation.
    match continuation.tag.expr {
      // If continuation is already tail, just return it.
      ExprTag::Tail => Ok(continuation),
      // Otherwise, package it along with supplied env as a new Tail
      // continuation.
      _ => store.intern_expr(Expr::Tail(env, continuation)),
    }
    // Since this is the only place Tail continuation are created, this ensures
    // Tail continuations never point to one another: they can only be nested
    // one deep.
  }

  pub fn apply_continuation(
    self,
    store: &mut Store<F>,
    witness: &mut Witness<F>,
  ) -> Result<Self, LurkError<F>> {
    if !self.is_apply_continuation() {
      return Ok(self);
    }

    let hash_witness = &mut witness.hashes;
    let (result, env, cont) = self.as_results();
    witness.apply_continuation_cont = Some(*cont);
    match cont.tag.expr {
      ExprTag::Terminal | ExprTag::Error => {
        Ok(Control::Return(*result, *env, *cont))
      },
      ExprTag::Dummy => {
        unreachable!("Dummy Continuation should never be applied.")
      },
      ExprTag::Outermost => {
        Ok(Control::Return(*result, *env, store.intern_expr(Expr::Terminal)?))
      },
      // Although Emit has no effect within the computation, it has an
      // externally-visible side effect of manifesting an explicit Thunk
      // in the expr register of the execution trace.
      ExprTag::Emit => match store.get_expr(*cont)? {
        Expr::Emit(cont) => Ok(Control::MakeThunk(*result, *env, cont)),
        _ => unreachable!(),
      },
      ExprTag::Call0 => match store.get_expr(*cont)? {
        Expr::Call0(saved_env, cont) => {
          match (result.tag.expr, store.get_expr(*result)?) {
            //(ExprTag::Fun, Expr::Fun(arg, body, closed_env)) => {
            //  if arg == store.lurk_sym("_") {
            //    let (body_form, _) = hash_witness.car_cdr_named(
            //      ConsName::FunBody,
            //      store,
            //      &body,
            //    )?;
            //    let cont =
            //      Self::make_tail_continuation(saved_env, cont, store)?;
            //    Ok(Control::Return(body_form, closed_env, cont))
            //  }
            //  // Applying zero args to a non-zero arg function leaves it
            //  // unchanged. This is arguably consistent with auto-currying.
            //  else {
            //    Ok(Control::Return(*result, *env, cont))
            //  }
            //},
            _ => unreachable!(),
          }
        },
        // bad function
        _ => {
          Ok(Control::Return(*result, *env, store.intern_expr(Expr::Error)?))
        },
      },
      _ => todo!(),
    }
  }
}
