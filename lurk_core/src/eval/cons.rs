use lurk_ff::LurkField;

use crate::{
  error::LurkError,
  expr::Expr,
  ptr::Ptr,
  store::Store,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Cons<F: LurkField> {
  pub car: Ptr<F>,
  pub cdr: Ptr<F>,
  pub cons: Ptr<F>,
}
impl<F: LurkField> Cons<F> {
  pub fn cons(
    store: &mut Store<F>,
    car: Ptr<F>,
    cdr: Ptr<F>,
  ) -> Result<Ptr<F>, LurkError<F>> {
    store.insert_expr(Expr::Cons(car, cdr))
  }

  pub fn strcons(
    store: &mut Store<F>,
    car: Ptr<F>,
    cdr: Ptr<F>,
  ) -> Result<Ptr<F>, LurkError<F>> {
    todo!()
  }

  pub fn car_cdr(
    &self,
    cons: &Ptr<F>,
  ) -> Result<(Ptr<F>, Ptr<F>), LurkError<F>> {
    if *cons != self.cons {
      Err(LurkError::Custom("wrong cons"))
    }
    else {
      Ok((self.car, self.cdr))
    }
  }

  pub fn get_car_cdr(
    s: &mut Store<F>,
    cons: &Ptr<F>,
  ) -> Result<(Ptr<F>, Ptr<F>), LurkError<F>> {
    s.car_cdr(cons)
  }
}
