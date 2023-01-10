use std::fmt;

use lurk_ff::{
  field::LurkField,
  tag::ExprTag,
};

use crate::{
  hash::PoseidonCache,
  op::{
    Op1,
    Op2,
  },
  ptr::Ptr,
  serde_f::{
    SerdeF,
    SerdeFError,
  },
};

// user-level expressions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Expr<F: LurkField> {
  ConsNil,
  Cons(Ptr<F>, Ptr<F>),        // car, cdr
  Comm(Ptr<F>, Ptr<F>),        // secret, val
  SymNil,                      //
  SymCons(Ptr<F>, Ptr<F>),     // head, tail
  StrNil,                      //
  StrCons(Ptr<F>, Ptr<F>),     // head, tail
  Thunk(Ptr<F>, Ptr<F>),       // val, cont
  Fun(Ptr<F>, Ptr<F>, Ptr<F>), // arg, body, env
  Num(F),                      //
  Char(F),                     //
  U64(F),                      //
  Map(Ptr<F>),                 // symbol
  Link(Ptr<F>, Ptr<F>),        // ctx, data
  Outermost,
  Call(Ptr<F>, Ptr<F>, Ptr<F>),  // arg, env, cont
  Call0(Ptr<F>, Ptr<F>),         // env, cont
  Call2(Ptr<F>, Ptr<F>, Ptr<F>), // fun, env, cont
  Tail(Ptr<F>, Ptr<F>),          // env, cont
  Error,
  Lookup(Ptr<F>, Ptr<F>),                 // env, cont
  Unop(Ptr<F>, Ptr<F>),                   // op, cont
  Binop(Ptr<F>, Ptr<F>, Ptr<F>, Ptr<F>),  // op, env, args, cont
  Binop2(Ptr<F>, Ptr<F>, Ptr<F>),         // op, arg, cont
  If(Ptr<F>, Ptr<F>),                     // args, cont
  Let(Ptr<F>, Ptr<F>, Ptr<F>, Ptr<F>),    // var, body, env, cont
  LetRec(Ptr<F>, Ptr<F>, Ptr<F>, Ptr<F>), // var, body, env, cont
  Emit(Ptr<F>),                           // cont
  Dummy,
  Terminal,
  Op1(Op1),
  Op2(Op2),
}

impl<F: LurkField> Expr<F> {
  /// All the `Ptr`s directly reachable from `expr`, if any.
  pub fn child_ptrs(&self) -> Vec<Ptr<F>> {
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

  pub fn ptr(&self, cache: &PoseidonCache<F>) -> Ptr<F> {
    match self {
      Expr::ConsNil => Ptr { tag: F::expr_tag(ExprTag::Cons), val: F::zero() },
      Expr::Cons(car, cdr) => Ptr {
        tag: F::expr_tag(ExprTag::Cons),
        val: cache.hash4(&[
          F::from_tag_unchecked(car.tag),
          car.val,
          F::from_tag_unchecked(cdr.tag),
          cdr.val,
        ]),
      },
      Expr::Comm(secret, val) => Ptr {
        tag: F::expr_tag(ExprTag::Comm),
        val: cache.hash4(&[
          F::from_tag_unchecked(secret.tag),
          secret.val,
          F::from_tag_unchecked(val.tag),
          val.val,
        ]),
      },
      Expr::SymNil => Ptr { tag: F::expr_tag(ExprTag::Sym), val: F::zero() },
      Expr::SymCons(head, tail) => Ptr {
        tag: F::expr_tag(ExprTag::Sym),
        val: cache.hash4(&[
          F::from_tag_unchecked(head.tag),
          head.val,
          F::from_tag_unchecked(tail.tag),
          tail.val,
        ]),
      },
      Expr::Fun(arg, body, env) => Ptr {
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
      Expr::Num(f) => Ptr { tag: F::expr_tag(ExprTag::Num), val: *f },
      Expr::StrNil => Ptr { tag: F::expr_tag(ExprTag::Str), val: F::zero() },
      Expr::StrCons(head, tail) => Ptr {
        tag: F::expr_tag(ExprTag::Str),
        val: cache.hash4(&[
          F::from_tag_unchecked(head.tag),
          head.val,
          F::from_tag_unchecked(tail.tag),
          tail.val,
        ]),
      },
      Expr::Char(f) => Ptr { tag: F::expr_tag(ExprTag::Char), val: *f },
      Expr::U64(f) => Ptr { tag: F::expr_tag(ExprTag::U64), val: *f },
      Expr::Thunk(val, cont) => Ptr {
        tag: F::expr_tag(ExprTag::Thunk),
        val: cache.hash4(&[
          F::from_tag_unchecked(val.tag),
          val.val,
          F::from_tag_unchecked(cont.tag),
          cont.val,
        ]),
      },
      Expr::Map(map) => Ptr { tag: F::expr_tag(ExprTag::Map), val: map.val },
      Expr::Link(ctx, data) => Ptr {
        tag: F::expr_tag(ExprTag::Link),
        val: cache.hash4(&[
          F::from_tag_unchecked(ctx.tag),
          ctx.val,
          F::from_tag_unchecked(data.tag),
          data.val,
        ]),
      },
      Expr::Outermost => {
        Ptr { tag: F::expr_tag(ExprTag::Outermost), val: F::zero() }
      },
      Expr::Call(arg, env, cont) => Ptr {
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
      Expr::Call0(env, cont) => Ptr {
        tag: F::expr_tag(ExprTag::Call0),
        val: cache.hash4(&[
          F::from_tag_unchecked(env.tag),
          env.val,
          F::from_tag_unchecked(cont.tag),
          cont.val,
        ]),
      },
      Expr::Call2(fun, env, cont) => Ptr {
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
      Expr::Tail(env, cont) => Ptr {
        tag: F::expr_tag(ExprTag::Tail),
        val: cache.hash4(&[
          F::from_tag_unchecked(env.tag),
          env.val,
          F::from_tag_unchecked(cont.tag),
          cont.val,
        ]),
      },
      Expr::Error => Ptr { tag: F::expr_tag(ExprTag::Error), val: F::zero() },
      Expr::Lookup(env, cont) => Ptr {
        tag: F::expr_tag(ExprTag::Lookup),
        val: cache.hash4(&[
          F::from_tag_unchecked(env.tag),
          env.val,
          F::from_tag_unchecked(cont.tag),
          cont.val,
        ]),
      },
      Expr::Unop(op1, cont) => Ptr {
        tag: F::expr_tag(ExprTag::Unop),
        val: cache.hash4(&[
          F::from_tag_unchecked(op1.tag),
          op1.val,
          F::from_tag_unchecked(cont.tag),
          cont.val,
        ]),
      },
      Expr::Binop(op2, env, args, cont) => Ptr {
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
      Expr::Binop2(op2, arg, cont) => Ptr {
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
      Expr::If(args, cont) => Ptr {
        tag: F::expr_tag(ExprTag::If),
        val: cache.hash4(&[
          F::from_tag_unchecked(args.tag),
          args.val,
          F::from_tag_unchecked(cont.tag),
          cont.val,
        ]),
      },
      Expr::Let(var, body, env, cont) => Ptr {
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
      Expr::LetRec(var, body, env, cont) => Ptr {
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
      Expr::Emit(cont) => Ptr {
        tag: F::expr_tag(ExprTag::Emit),
        val: cache.hash4(&[
          F::from_tag_unchecked(cont.tag),
          cont.val,
          F::zero(),
          F::zero(),
        ]),
      },
      Expr::Dummy => Ptr { tag: F::expr_tag(ExprTag::Dummy), val: F::zero() },
      Expr::Terminal => {
        Ptr { tag: F::expr_tag(ExprTag::Terminal), val: F::zero() }
      },
      Expr::Op1(op1) => {
        Ptr { tag: F::expr_tag(ExprTag::Op1), val: F::from_u16(*op1 as u16) }
      },
      Expr::Op2(op2) => {
        Ptr { tag: F::expr_tag(ExprTag::Op2), val: F::from_u16(*op2 as u16) }
      },
    }
  }
}

impl<F: LurkField> SerdeF<F> for Expr<F> {
  fn ser_f(&self) -> Vec<F> {
    let mut res = self.ptr(&PoseidonCache::default()).ser_f();
    for ptr in self.child_ptrs() {
      res.append(&mut ptr.ser_f());
    }
    res
  }

  fn de_f(fs: &[F]) -> Result<Expr<F>, SerdeFError<F>> {
    if fs.len() < 2 {
      return Err(SerdeFError::UnexpectedEnd);
    }
    let ptr = Ptr::de_f(&fs[0..])?;
    if fs.len() < ptr.child_ptr_arity() * 2 {
      return Err(SerdeFError::UnexpectedEnd);
    }
    if let Some(expr) = ptr.immediate() {
      Ok(expr)
    }
    else {
      match ptr.tag.expr {
        ExprTag::Fun => {
          let arg = Ptr::de_f(&fs[2..])?;
          let bod = Ptr::de_f(&fs[4..])?;
          let env = Ptr::de_f(&fs[6..])?;
          Ok(Expr::Fun(arg, bod, env))
        },
        ExprTag::Cons => {
          let car = Ptr::de_f(&fs[2..])?;
          let cdr = Ptr::de_f(&fs[4..])?;
          Ok(Expr::Cons(car, cdr))
        },
        ExprTag::Str => {
          let car = Ptr::de_f(&fs[2..])?;
          let cdr = Ptr::de_f(&fs[4..])?;
          Ok(Expr::StrCons(car, cdr))
        },
        ExprTag::Sym => {
          let car = Ptr::de_f(&fs[2..])?;
          let cdr = Ptr::de_f(&fs[4..])?;
          Ok(Expr::SymCons(car, cdr))
        },
        ExprTag::Comm => {
          let sec = Ptr::de_f(&fs[2..])?;
          let val = Ptr::de_f(&fs[4..])?;
          Ok(Expr::Comm(sec, val))
        },
        ExprTag::Link => {
          let ctx = Ptr::de_f(&fs[2..])?;
          let val = Ptr::de_f(&fs[4..])?;
          Ok(Expr::Link(ctx, val))
        },
        ExprTag::Thunk => {
          let val = Ptr::de_f(&fs[2..])?;
          let cont = Ptr::de_f(&fs[4..])?;
          Ok(Expr::Thunk(val, cont))
        },
        ExprTag::Map => {
          let map = Ptr::de_f(&fs[2..])?;
          Ok(Expr::Map(map))
        },
        ExprTag::Call => {
          let arg = Ptr::de_f(&fs[2..])?;
          let env = Ptr::de_f(&fs[4..])?;
          let cont = Ptr::de_f(&fs[6..])?;
          Ok(Expr::Call(arg, env, cont))
        },
        ExprTag::Call0 => {
          let env = Ptr::de_f(&fs[2..])?;
          let cont = Ptr::de_f(&fs[4..])?;
          Ok(Expr::Call0(env, cont))
        },
        ExprTag::Call2 => {
          let fun = Ptr::de_f(&fs[2..])?;
          let env = Ptr::de_f(&fs[4..])?;
          let cont = Ptr::de_f(&fs[6..])?;
          Ok(Expr::Call2(fun, env, cont))
        },
        ExprTag::Tail => {
          let env = Ptr::de_f(&fs[2..])?;
          let cont = Ptr::de_f(&fs[4..])?;
          Ok(Expr::Tail(env, cont))
        },
        ExprTag::Lookup => {
          let env = Ptr::de_f(&fs[2..])?;
          let cont = Ptr::de_f(&fs[4..])?;
          Ok(Expr::Lookup(env, cont))
        },
        ExprTag::Unop => {
          let op1 = Ptr::de_f(&fs[2..])?;
          let cont = Ptr::de_f(&fs[4..])?;
          Ok(Expr::Unop(op1, cont))
        },
        ExprTag::Binop => {
          let op2 = Ptr::de_f(&fs[2..])?;
          let env = Ptr::de_f(&fs[4..])?;
          let args = Ptr::de_f(&fs[6..])?;
          let cont = Ptr::de_f(&fs[8..])?;
          Ok(Expr::Binop(op2, env, args, cont))
        },
        ExprTag::Binop2 => {
          let op2 = Ptr::de_f(&fs[2..])?;
          let arg = Ptr::de_f(&fs[4..])?;
          let cont = Ptr::de_f(&fs[6..])?;
          Ok(Expr::Binop2(op2, arg, cont))
        },
        ExprTag::If => {
          let args = Ptr::de_f(&fs[2..])?;
          let cont = Ptr::de_f(&fs[4..])?;
          Ok(Expr::If(args, cont))
        },
        ExprTag::Let => {
          let var = Ptr::de_f(&fs[2..])?;
          let body = Ptr::de_f(&fs[4..])?;
          let env = Ptr::de_f(&fs[6..])?;
          let cont = Ptr::de_f(&fs[8..])?;
          Ok(Expr::Let(var, body, env, cont))
        },
        ExprTag::LetRec => {
          let var = Ptr::de_f(&fs[2..])?;
          let body = Ptr::de_f(&fs[4..])?;
          let env = Ptr::de_f(&fs[6..])?;
          let cont = Ptr::de_f(&fs[8..])?;
          Ok(Expr::LetRec(var, body, env, cont))
        },
        ExprTag::Emit => {
          let cont = Ptr::de_f(&fs[2..])?;
          Ok(Expr::Emit(cont))
        },
        _ => Err(SerdeFError::Expected("Expr".to_string())),
      }
    }
  }
}

impl<F: LurkField> fmt::Display for Expr<F> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let ptr = self.ptr(&PoseidonCache::default());
    let child_ptrs = self.child_ptrs();
    write!(f, "{}", ptr.tag.expr)?;
    write!(f, "(")?;
    for child in child_ptrs {
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
        (100, Box::new(|g| Self::Cons(Ptr::arbitrary(g), Ptr::arbitrary(g)))),
        (100, Box::new(|g| Self::Comm(Ptr::arbitrary(g), Ptr::arbitrary(g)))),
        (100, Box::new(|_| Self::StrNil)),
        (
          100,
          Box::new(|g| Self::StrCons(Ptr::arbitrary(g), Ptr::arbitrary(g))),
        ),
        (100, Box::new(|_| Self::SymNil)),
        (
          100,
          Box::new(|g| Self::SymCons(Ptr::arbitrary(g), Ptr::arbitrary(g))),
        ),
        (100, Box::new(|g| Self::Num(FWrap::arbitrary(g).0))),
        (100, Box::new(|g| Self::Char(FWrap::arbitrary(g).0))),
        (100, Box::new(|g| Self::U64(FWrap::arbitrary(g).0))),
        (
          100,
          Box::new(|g| {
            Self::Fun(Ptr::arbitrary(g), Ptr::arbitrary(g), Ptr::arbitrary(g))
          }),
        ),
        (100, Box::new(|g| Self::Thunk(Ptr::arbitrary(g), Ptr::arbitrary(g)))),
        (100, Box::new(|g| Self::Map(Ptr::arbitrary(g)))),
        (100, Box::new(|g| Self::Link(Ptr::arbitrary(g), Ptr::arbitrary(g)))),
        (100, Box::new(|_| Self::Outermost)),
        (
          100,
          Box::new(|g| {
            Self::Call(Ptr::arbitrary(g), Ptr::arbitrary(g), Ptr::arbitrary(g))
          }),
        ),
        (
          100,
          Box::new(|g| {
            Self::Call2(Ptr::arbitrary(g), Ptr::arbitrary(g), Ptr::arbitrary(g))
          }),
        ),
        (100, Box::new(|g| Self::Tail(Ptr::arbitrary(g), Ptr::arbitrary(g)))),
        (100, Box::new(|_| Self::Error)),
        (100, Box::new(|g| Self::Lookup(Ptr::arbitrary(g), Ptr::arbitrary(g)))),
        (
          100,
          Box::new(|g| {
            Self::Unop(
              Ptr {
                tag: Fr::expr_tag(ExprTag::Op1),
                val: Fr::from_u16(Op1::arbitrary(g) as u16),
              },
              Ptr::arbitrary(g),
            )
          }),
        ),
        (
          100,
          Box::new(|g| {
            Self::Binop(
              Ptr {
                tag: Fr::expr_tag(ExprTag::Op2),
                val: Fr::from_u16(Op2::arbitrary(g) as u16),
              },
              Ptr::arbitrary(g),
              Ptr::arbitrary(g),
              Ptr::arbitrary(g),
            )
          }),
        ),
        (
          100,
          Box::new(|g| {
            Self::Binop2(
              Ptr {
                tag: Fr::expr_tag(ExprTag::Op2),
                val: Fr::from_u16(Op2::arbitrary(g) as u16),
              },
              Ptr::arbitrary(g),
              Ptr::arbitrary(g),
            )
          }),
        ),
        (100, Box::new(|g| Self::If(Ptr::arbitrary(g), Ptr::arbitrary(g)))),
        (
          100,
          Box::new(|g| {
            Self::Let(
              Ptr::arbitrary(g),
              Ptr::arbitrary(g),
              Ptr::arbitrary(g),
              Ptr::arbitrary(g),
            )
          }),
        ),
        (
          100,
          Box::new(|g| {
            Self::LetRec(
              Ptr::arbitrary(g),
              Ptr::arbitrary(g),
              Ptr::arbitrary(g),
              Ptr::arbitrary(g),
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
