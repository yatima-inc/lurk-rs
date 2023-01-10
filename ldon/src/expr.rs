use std::fmt;

use lurk_ff::{
  field::LurkField,
  tag::ExprTag,
};

use crate::{
  cid::Cid,
  hash::PoseidonCache,
  op::{
    Op1,
    Op2,
  },
  serde_f::{
    SerdeF,
    SerdeFError,
  },
};

// user-level expressions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Expr<F: LurkField> {
  ConsNil,
  Cons(Cid<F>, Cid<F>),                   // car, cdr
  Comm(Cid<F>, Cid<F>),                   // secret, val
  SymNil,                                 //
  SymCons(Cid<F>, Cid<F>),                // head, tail
  StrNil,                                 //
  StrCons(Cid<F>, Cid<F>),                // head, tail
  Thunk(Cid<F>, Cid<F>),                  // val, cont
  Fun(Cid<F>, Cid<F>, Cid<F>),            // arg, body, env
  Num(F),                                 //
  Char(F),                                //
  U64(F),                                 //
  Map(Cid<F>),                            // symbol
  Link(Cid<F>, Cid<F>),                   // ctx, data
  Outermost,                              //
  Call(Cid<F>, Cid<F>, Cid<F>),           // arg, env, cont
  Call0(Cid<F>, Cid<F>),                  // env, cont
  Call2(Cid<F>, Cid<F>, Cid<F>),          // fun, env, cont
  Tail(Cid<F>, Cid<F>),                   // env, cont
  Error,                                  //
  Lookup(Cid<F>, Cid<F>),                 // env, cont
  Unop(Cid<F>, Cid<F>),                   // op, cont
  Binop(Cid<F>, Cid<F>, Cid<F>, Cid<F>),  // op, env, args, cont
  Binop2(Cid<F>, Cid<F>, Cid<F>),         // op, arg, cont
  If(Cid<F>, Cid<F>),                     // args, cont
  Let(Cid<F>, Cid<F>, Cid<F>, Cid<F>),    // var, body, env, cont
  LetRec(Cid<F>, Cid<F>, Cid<F>, Cid<F>), // var, body, env, cont
  Emit(Cid<F>),                           // cont
  Dummy,
  Terminal,
  Op1(Op1),
  Op2(Op2),
}

impl<F: LurkField> Expr<F> {
  /// All the `Cid`s directly reachable from `expr`, if any.
  pub fn child_cids(&self) -> Vec<Cid<F>> {
    match self {
      Expr::ConsNil => vec![],
      Expr::Cons(car, cdr) => vec![*car, *cdr],
      Expr::Comm(secret, payload) => vec![*secret, *payload],
      Expr::SymNil => vec![],
      Expr::SymCons(head, tail) => vec![*head, *tail],
      Expr::Fun(arg, body, closed_env) => vec![*arg, *body, *closed_env],
      Expr::Num(_) => vec![],
      Expr::StrNil => vec![],
      Expr::StrCons(head, tail) => vec![*head, *tail],
      Expr::Thunk(val, cont) => vec![*val, *cont],
      Expr::Char(_) => vec![],
      Expr::U64(_) => vec![],
      Expr::Map(map) => vec![*map],
      Expr::Link(ctx, data) => vec![*ctx, *data],
      Expr::Call(arg, env, cont) => vec![*arg, *env, *cont],
      Expr::Call0(env, cont) => vec![*env, *cont],
      Expr::Call2(fun, env, cont) => vec![*fun, *env, *cont],
      Expr::Tail(env, cont) => vec![*env, *cont],
      Expr::Lookup(env, cont) => vec![*env, *cont],
      Expr::Unop(op1, cont) => vec![*op1, *cont],
      Expr::Binop(op2, env, args, cont) => vec![*op2, *env, *args, *cont],
      Expr::Binop2(op2, arg, cont) => vec![*op2, *arg, *cont],
      Expr::If(args, cont) => vec![*args, *cont],
      Expr::Let(var, body, env, cont) => vec![*var, *body, *env, *cont],
      Expr::LetRec(var, body, env, cont) => vec![*var, *body, *env, *cont],
      Expr::Emit(cont) => vec![*cont],
      _ => vec![],
    }
  }

  pub fn cid(&self, cache: &PoseidonCache<F>) -> Cid<F> {
    match self {
      Expr::ConsNil => Cid { tag: F::expr_tag(ExprTag::Cons), val: F::zero() },
      Expr::Cons(car, cdr) => Cid {
        tag: F::expr_tag(ExprTag::Cons),
        val: cache.hash4(&[
          F::from_tag_unchecked(car.tag),
          car.val,
          F::from_tag_unchecked(cdr.tag),
          cdr.val,
        ]),
      },
      Expr::Comm(secret, val) => Cid {
        tag: F::expr_tag(ExprTag::Comm),
        val: cache.hash4(&[
          F::from_tag_unchecked(secret.tag),
          secret.val,
          F::from_tag_unchecked(val.tag),
          val.val,
        ]),
      },
      Expr::SymNil => Cid { tag: F::expr_tag(ExprTag::Sym), val: F::zero() },
      Expr::SymCons(head, tail) => Cid {
        tag: F::expr_tag(ExprTag::Sym),
        val: cache.hash4(&[
          F::from_tag_unchecked(head.tag),
          head.val,
          F::from_tag_unchecked(tail.tag),
          tail.val,
        ]),
      },
      Expr::Fun(arg, body, env) => Cid {
        tag: F::expr_tag(ExprTag::Fun),
        val: cache.hash6(&[
          F::from_tag_unchecked(arg.tag),
          arg.val,
          F::from_tag_unchecked(body.tag),
          body.val,
          F::from_tag_unchecked(env.tag),
          env.val,
        ]),
      },
      Expr::Num(f) => Cid { tag: F::expr_tag(ExprTag::Num), val: *f },
      Expr::StrNil => Cid { tag: F::expr_tag(ExprTag::Str), val: F::zero() },
      Expr::StrCons(head, tail) => Cid {
        tag: F::expr_tag(ExprTag::Str),
        val: cache.hash4(&[
          F::from_tag_unchecked(head.tag),
          head.val,
          F::from_tag_unchecked(tail.tag),
          tail.val,
        ]),
      },
      Expr::Char(f) => Cid { tag: F::expr_tag(ExprTag::Char), val: *f },
      Expr::U64(f) => Cid { tag: F::expr_tag(ExprTag::U64), val: *f },
      Expr::Thunk(val, cont) => Cid {
        tag: F::expr_tag(ExprTag::Thunk),
        val: cache.hash4(&[
          F::from_tag_unchecked(val.tag),
          val.val,
          F::from_tag_unchecked(cont.tag),
          cont.val,
        ]),
      },
      Expr::Map(map) => Cid { tag: F::expr_tag(ExprTag::Map), val: map.val },
      Expr::Link(ctx, data) => Cid {
        tag: F::expr_tag(ExprTag::Link),
        val: cache.hash4(&[
          F::from_tag_unchecked(ctx.tag),
          ctx.val,
          F::from_tag_unchecked(data.tag),
          data.val,
        ]),
      },
      Expr::Outermost => {
        Cid { tag: F::expr_tag(ExprTag::Outermost), val: F::zero() }
      },
      Expr::Call(arg, env, cont) => Cid {
        tag: F::expr_tag(ExprTag::Call),
        val: cache.hash6(&[
          F::from_tag_unchecked(arg.tag),
          arg.val,
          F::from_tag_unchecked(env.tag),
          env.val,
          F::from_tag_unchecked(cont.tag),
          cont.val,
        ]),
      },
      Expr::Call0(env, cont) => Cid {
        tag: F::expr_tag(ExprTag::Call0),
        val: cache.hash4(&[
          F::from_tag_unchecked(env.tag),
          env.val,
          F::from_tag_unchecked(cont.tag),
          cont.val,
        ]),
      },
      Expr::Call2(fun, env, cont) => Cid {
        tag: F::expr_tag(ExprTag::Call2),
        val: cache.hash6(&[
          F::from_tag_unchecked(fun.tag),
          fun.val,
          F::from_tag_unchecked(env.tag),
          env.val,
          F::from_tag_unchecked(cont.tag),
          cont.val,
        ]),
      },
      Expr::Tail(env, cont) => Cid {
        tag: F::expr_tag(ExprTag::Tail),
        val: cache.hash4(&[
          F::from_tag_unchecked(env.tag),
          env.val,
          F::from_tag_unchecked(cont.tag),
          cont.val,
        ]),
      },
      Expr::Error => Cid { tag: F::expr_tag(ExprTag::Error), val: F::zero() },
      Expr::Lookup(env, cont) => Cid {
        tag: F::expr_tag(ExprTag::Lookup),
        val: cache.hash4(&[
          F::from_tag_unchecked(env.tag),
          env.val,
          F::from_tag_unchecked(cont.tag),
          cont.val,
        ]),
      },
      Expr::Unop(op1, cont) => Cid {
        tag: F::expr_tag(ExprTag::Unop),
        val: cache.hash4(&[
          F::from_tag_unchecked(op1.tag),
          op1.val,
          F::from_tag_unchecked(cont.tag),
          cont.val,
        ]),
      },
      Expr::Binop(op2, env, args, cont) => Cid {
        tag: F::expr_tag(ExprTag::Binop),
        val: cache.hash8(&[
          F::from_tag_unchecked(op2.tag),
          op2.val,
          F::from_tag_unchecked(env.tag),
          env.val,
          F::from_tag_unchecked(args.tag),
          args.val,
          F::from_tag_unchecked(cont.tag),
          cont.val,
        ]),
      },
      Expr::Binop2(op2, arg, cont) => Cid {
        tag: F::expr_tag(ExprTag::Binop2),
        val: cache.hash6(&[
          F::from_tag_unchecked(op2.tag),
          op2.val,
          F::from_tag_unchecked(arg.tag),
          arg.val,
          F::from_tag_unchecked(cont.tag),
          cont.val,
        ]),
      },
      Expr::If(args, cont) => Cid {
        tag: F::expr_tag(ExprTag::If),
        val: cache.hash4(&[
          F::from_tag_unchecked(args.tag),
          args.val,
          F::from_tag_unchecked(cont.tag),
          cont.val,
        ]),
      },
      Expr::Let(var, body, env, cont) => Cid {
        tag: F::expr_tag(ExprTag::Let),
        val: cache.hash8(&[
          F::from_tag_unchecked(var.tag),
          var.val,
          F::from_tag_unchecked(body.tag),
          body.val,
          F::from_tag_unchecked(env.tag),
          env.val,
          F::from_tag_unchecked(cont.tag),
          cont.val,
        ]),
      },
      Expr::LetRec(var, body, env, cont) => Cid {
        tag: F::expr_tag(ExprTag::LetRec),
        val: cache.hash8(&[
          F::from_tag_unchecked(var.tag),
          var.val,
          F::from_tag_unchecked(body.tag),
          body.val,
          F::from_tag_unchecked(env.tag),
          env.val,
          F::from_tag_unchecked(cont.tag),
          cont.val,
        ]),
      },
      Expr::Emit(cont) => Cid {
        tag: F::expr_tag(ExprTag::Emit),
        val: cache.hash4(&[
          F::from_tag_unchecked(cont.tag),
          cont.val,
          F::zero(),
          F::zero(),
        ]),
      },
      Expr::Dummy => Cid { tag: F::expr_tag(ExprTag::Dummy), val: F::zero() },
      Expr::Terminal => {
        Cid { tag: F::expr_tag(ExprTag::Terminal), val: F::zero() }
      },
      Expr::Op1(op1) => {
        Cid { tag: F::expr_tag(ExprTag::Op1), val: F::from_u16(*op1 as u16) }
      },
      Expr::Op2(op2) => {
        Cid { tag: F::expr_tag(ExprTag::Op2), val: F::from_u16(*op2 as u16) }
      },
    }
  }
}

impl<F: LurkField> SerdeF<F> for Expr<F> {
  fn ser_f(&self) -> Vec<F> {
    let mut res = self.cid(&PoseidonCache::default()).ser_f();
    for cid in self.child_cids() {
      res.append(&mut cid.ser_f());
    }
    res
  }

  fn de_f(fs: &[F]) -> Result<Expr<F>, SerdeFError<F>> {
    if fs.len() < 2 {
      return Err(SerdeFError::UnexpectedEnd);
    }
    let cid = Cid::de_f(&fs[0..])?;
    if fs.len() < cid.child_cid_arity() * 2 {
      return Err(SerdeFError::UnexpectedEnd);
    }
    if let Some(expr) = cid.immediate() {
      Ok(expr)
    }
    else {
      match cid.tag.expr {
        ExprTag::Fun => {
          let arg = Cid::de_f(&fs[2..])?;
          let bod = Cid::de_f(&fs[4..])?;
          let env = Cid::de_f(&fs[6..])?;
          Ok(Expr::Fun(arg, bod, env))
        },
        ExprTag::Cons => {
          let car = Cid::de_f(&fs[2..])?;
          let cdr = Cid::de_f(&fs[4..])?;
          Ok(Expr::Cons(car, cdr))
        },
        ExprTag::Str => {
          let car = Cid::de_f(&fs[2..])?;
          let cdr = Cid::de_f(&fs[4..])?;
          Ok(Expr::StrCons(car, cdr))
        },
        ExprTag::Sym => {
          let car = Cid::de_f(&fs[2..])?;
          let cdr = Cid::de_f(&fs[4..])?;
          Ok(Expr::SymCons(car, cdr))
        },
        ExprTag::Comm => {
          let sec = Cid::de_f(&fs[2..])?;
          let val = Cid::de_f(&fs[4..])?;
          Ok(Expr::Comm(sec, val))
        },
        ExprTag::Link => {
          let ctx = Cid::de_f(&fs[2..])?;
          let val = Cid::de_f(&fs[4..])?;
          Ok(Expr::Link(ctx, val))
        },
        ExprTag::Thunk => {
          let val = Cid::de_f(&fs[2..])?;
          let cont = Cid::de_f(&fs[4..])?;
          Ok(Expr::Thunk(val, cont))
        },
        ExprTag::Map => {
          let map = Cid::de_f(&fs[2..])?;
          Ok(Expr::Map(map))
        },
        ExprTag::Call => {
          let arg = Cid::de_f(&fs[2..])?;
          let env = Cid::de_f(&fs[4..])?;
          let cont = Cid::de_f(&fs[6..])?;
          Ok(Expr::Call(arg, env, cont))
        },
        ExprTag::Call0 => {
          let env = Cid::de_f(&fs[2..])?;
          let cont = Cid::de_f(&fs[4..])?;
          Ok(Expr::Call0(env, cont))
        },
        ExprTag::Call2 => {
          let fun = Cid::de_f(&fs[2..])?;
          let env = Cid::de_f(&fs[4..])?;
          let cont = Cid::de_f(&fs[6..])?;
          Ok(Expr::Call2(fun, env, cont))
        },
        ExprTag::Tail => {
          let env = Cid::de_f(&fs[2..])?;
          let cont = Cid::de_f(&fs[4..])?;
          Ok(Expr::Tail(env, cont))
        },
        ExprTag::Lookup => {
          let env = Cid::de_f(&fs[2..])?;
          let cont = Cid::de_f(&fs[4..])?;
          Ok(Expr::Lookup(env, cont))
        },
        ExprTag::Unop => {
          let op1 = Cid::de_f(&fs[2..])?;
          let cont = Cid::de_f(&fs[4..])?;
          Ok(Expr::Unop(op1, cont))
        },
        ExprTag::Binop => {
          let op2 = Cid::de_f(&fs[2..])?;
          let env = Cid::de_f(&fs[4..])?;
          let args = Cid::de_f(&fs[6..])?;
          let cont = Cid::de_f(&fs[8..])?;
          Ok(Expr::Binop(op2, env, args, cont))
        },
        ExprTag::Binop2 => {
          let op2 = Cid::de_f(&fs[2..])?;
          let arg = Cid::de_f(&fs[4..])?;
          let cont = Cid::de_f(&fs[6..])?;
          Ok(Expr::Binop2(op2, arg, cont))
        },
        ExprTag::If => {
          let args = Cid::de_f(&fs[2..])?;
          let cont = Cid::de_f(&fs[4..])?;
          Ok(Expr::If(args, cont))
        },
        ExprTag::Let => {
          let var = Cid::de_f(&fs[2..])?;
          let body = Cid::de_f(&fs[4..])?;
          let env = Cid::de_f(&fs[6..])?;
          let cont = Cid::de_f(&fs[8..])?;
          Ok(Expr::Let(var, body, env, cont))
        },
        ExprTag::LetRec => {
          let var = Cid::de_f(&fs[2..])?;
          let body = Cid::de_f(&fs[4..])?;
          let env = Cid::de_f(&fs[6..])?;
          let cont = Cid::de_f(&fs[8..])?;
          Ok(Expr::LetRec(var, body, env, cont))
        },
        ExprTag::Emit => {
          let cont = Cid::de_f(&fs[2..])?;
          Ok(Expr::Emit(cont))
        },
        _ => Err(SerdeFError::Expected("Expr".to_string())),
      }
    }
  }
}

impl<F: LurkField> fmt::Display for Expr<F> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let cid = self.cid(&PoseidonCache::default());
    let child_cids = self.child_cids();
    write!(f, "{}", cid.tag.expr)?;
    write!(f, "(")?;
    for child in child_cids {
      write!(f, " {},", child)?;
    }
    write!(f, ")")
  }
}

#[cfg(feature = "test-utils")]
pub mod test_utils {
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

  // These expressions are not necessarily well-formed
  impl Arbitrary for Expr<Fr> {
    fn arbitrary(g: &mut Gen) -> Self {
      #[allow(clippy::type_complexity)]
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> Expr<Fr>>)> = vec![
        (100, Box::new(|_| Self::ConsNil)),
        (100, Box::new(|g| Self::Cons(Cid::arbitrary(g), Cid::arbitrary(g)))),
        (100, Box::new(|g| Self::Comm(Cid::arbitrary(g), Cid::arbitrary(g)))),
        (100, Box::new(|_| Self::StrNil)),
        (
          100,
          Box::new(|g| Self::StrCons(Cid::arbitrary(g), Cid::arbitrary(g))),
        ),
        (100, Box::new(|_| Self::SymNil)),
        (
          100,
          Box::new(|g| Self::SymCons(Cid::arbitrary(g), Cid::arbitrary(g))),
        ),
        (100, Box::new(|g| Self::Num(FWrap::arbitrary(g).0))),
        (100, Box::new(|g| Self::Char(FWrap::arbitrary(g).0))),
        (100, Box::new(|g| Self::U64(FWrap::arbitrary(g).0))),
        (
          100,
          Box::new(|g| {
            Self::Fun(Cid::arbitrary(g), Cid::arbitrary(g), Cid::arbitrary(g))
          }),
        ),
        (100, Box::new(|g| Self::Thunk(Cid::arbitrary(g), Cid::arbitrary(g)))),
        (100, Box::new(|g| Self::Map(Cid::arbitrary(g)))),
        (100, Box::new(|g| Self::Link(Cid::arbitrary(g), Cid::arbitrary(g)))),
        (100, Box::new(|_| Self::Outermost)),
        (
          100,
          Box::new(|g| {
            Self::Call(Cid::arbitrary(g), Cid::arbitrary(g), Cid::arbitrary(g))
          }),
        ),
        (
          100,
          Box::new(|g| {
            Self::Call2(Cid::arbitrary(g), Cid::arbitrary(g), Cid::arbitrary(g))
          }),
        ),
        (100, Box::new(|g| Self::Tail(Cid::arbitrary(g), Cid::arbitrary(g)))),
        (100, Box::new(|_| Self::Error)),
        (100, Box::new(|g| Self::Lookup(Cid::arbitrary(g), Cid::arbitrary(g)))),
        (
          100,
          Box::new(|g| {
            Self::Unop(
              Cid {
                tag: Fr::expr_tag(ExprTag::Op1),
                val: Fr::from_u16(Op1::arbitrary(g) as u16),
              },
              Cid::arbitrary(g),
            )
          }),
        ),
        (
          100,
          Box::new(|g| {
            Self::Binop(
              Cid {
                tag: Fr::expr_tag(ExprTag::Op2),
                val: Fr::from_u16(Op2::arbitrary(g) as u16),
              },
              Cid::arbitrary(g),
              Cid::arbitrary(g),
              Cid::arbitrary(g),
            )
          }),
        ),
        (
          100,
          Box::new(|g| {
            Self::Binop2(
              Cid {
                tag: Fr::expr_tag(ExprTag::Op2),
                val: Fr::from_u16(Op2::arbitrary(g) as u16),
              },
              Cid::arbitrary(g),
              Cid::arbitrary(g),
            )
          }),
        ),
        (100, Box::new(|g| Self::If(Cid::arbitrary(g), Cid::arbitrary(g)))),
        (
          100,
          Box::new(|g| {
            Self::Let(
              Cid::arbitrary(g),
              Cid::arbitrary(g),
              Cid::arbitrary(g),
              Cid::arbitrary(g),
            )
          }),
        ),
        (
          100,
          Box::new(|g| {
            Self::LetRec(
              Cid::arbitrary(g),
              Cid::arbitrary(g),
              Cid::arbitrary(g),
              Cid::arbitrary(g),
            )
          }),
        ),
        (100, Box::new(|_| Self::Dummy)),
        (100, Box::new(|_| Self::Terminal)),
      ];
      frequency(g, input)
    }
  }
}

#[cfg(all(test, feature = "test-utils"))]
pub mod tests {
  use blstrs::Scalar as Fr;

  use super::*;

  #[quickcheck]
  fn prop_expr_serdef(expr1: Expr<Fr>) -> bool {
    println!("===============================");
    let vec = &expr1.ser_f();
    println!("{:?}", vec);
    match Expr::de_f(&vec) {
      Ok(expr2) => {
        println!("expr1: {}", expr1);
        println!("expr2: {}", expr2);
        expr1 == expr2
      },
      Err(e) => {
        println!("{:?}", e);
        false
      },
    }
  }

  #[quickcheck]
  fn prop_expr_serde(expr1: Expr<Fr>) -> bool {
    println!("===============================");
    let vec = &expr1.ser();
    println!("{:?}", vec);
    match Expr::de(&vec) {
      Ok(expr2) => {
        println!("expr1: {}", expr1);
        println!("expr2: {}", expr2);
        expr1 == expr2
      },
      Err(e) => {
        println!("{:?}", e);
        false
      },
    }
  }
}
