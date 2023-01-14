use ldon::{
  op::{
    Op1,
    Op2,
  },
  sym::Symbol,
};
use lurk_ff::LurkField;

use crate::{
  num::Num,
  ptr::Ptr,
  uint::UInt,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr<F: LurkField> {
  Cons(Ptr<F>, Ptr<F>), // car, cdr
  Comm(Ptr<F>, Ptr<F>), // secret, payload
  SymNil,
  SymCons(Ptr<F>, Ptr<F>),
  Keyword(Ptr<F>),
  Fun(Ptr<F>, Ptr<F>, Ptr<F>), // arg, body, env
  Num(Num<F>),                 //
  StrNil,
  StrCons(Ptr<F>, Ptr<F>),
  Thunk(Ptr<F>, Ptr<F>),                  // val, cont
  Char(char),                             //
  UInt(UInt),                             //
  Op1(Op1),                               //
  Op2(Op2),                               //
  Map(Ptr<F>),                            // assoc-list
  Link(Ptr<F>, Ptr<F>),                   // ctx, data
  Outermost,                              //
  Call(Ptr<F>, Ptr<F>, Ptr<F>),           // arg, env, cont
  Call0(Ptr<F>, Ptr<F>),                  // env, cont
  Call2(Ptr<F>, Ptr<F>, Ptr<F>),          // fun, env, cont
  Tail(Ptr<F>, Ptr<F>),                   // env, cont
  Error,                                  //
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
}

// impl<F: LurkField> Expr<F> {
//
//}
// pub fn is_keyword_sym(&self) -> bool {
//  match self {
//    Expression::Sym(s) => s.is_keyword(),
//    _ => false,
//  }
//}

// pub fn as_str(&self) -> Option<&str> {
//  match self {
//    Expression::Str(s) => Some(s),
//    _ => None,
//  }
//}

// pub fn as_sym_str(&self) -> Option<String> {
//  match self {
//    Expression::Sym(s) => Some(s.full_name()),
//    _ => None,
//  }
//}

// pub fn as_sym(&self) -> Option<&Sym> {
//  match self {
//    Expression::Sym(s) => Some(s),
//    _ => None,
//  }
//}

// pub fn as_simple_keyword_string(&self) -> Option<String> {
//  match self {
//    Expression::Sym(s) => s.simple_keyword_name(),
//    _ => None,
//  }
//}

// pub fn is_null(&self) -> bool { matches!(self, Self::Nil) }
// pub fn is_cons(&self) -> bool { matches!(self, Self::Cons(_, _)) }
// pub fn is_list(&self) -> bool { self.is_null() || self.is_cons() }
// pub fn is_sym(&self) -> bool { matches!(self, Self::Sym(_)) }
// pub fn is_fun(&self) -> bool { matches!(self, Self::Fun(_, _, _)) }
// pub fn is_num(&self) -> bool { matches!(self, Self::Num(_)) }
// pub fn is_str(&self) -> bool { matches!(self, Self::Str(_)) }
// pub fn is_thunk(&self) -> bool { matches!(self, Self::Thunk(_)) }
// pub fn is_opaque(&self) -> bool { matches!(self, Self::Opaque(_)) }
//}

// impl<F: LurkField> Write<F> for Expression<'_, F> {
//  fn fmt<W: io::Write>(&self, store: &Store<F>, w: &mut W) -> io::Result<()> {
//    use Expression::*;
//
//    match self {
//      Nil => write!(w, "NIL"),
//      Sym(s) => write_symbol::<F, _>(w, store, s),
//      Str(s) => write!(w, "\"{}\"", s),
//      Fun(arg, body, _closed_env) => {
//        let is_zero_arg =
//          *arg == store.get_lurk_sym("_", true).expect("dummy_arg (_)
// missing");        let arg = store.fetch(arg).unwrap();
//        write!(w, "<FUNCTION (")?;
//        if !is_zero_arg {
//          arg.fmt(store, w)?;
//        }
//        write!(w, ") ")?;
//
//        // Assume body is a single-element cons, ignore the cdr
//        match store.fetch(body).unwrap() {
//          Expression::Cons(expr, _) => {
//            let expr = store.fetch(&expr).unwrap();
//            expr.fmt(store, w)?;
//          },
//          Expression::Nil => {
//            store.get_nil().fmt(store, w)?;
//          },
//          _ => {
//            panic!("Function body was neither a Cons nor Nil");
//          },
//        }
//        write!(w, ">")
//      },
//      Num(n) => write!(w, "{}", n),
//      Thunk(f) => {
//        write!(w, "Thunk{{ value: ")?;
//        f.value.fmt(store, w)?;
//        write!(w, " => cont: ")?;
//        f.continuation.fmt(store, w)?;
//        write!(w, "}}")
//      },
//      Cons(..) => {
//        write!(w, "(")?;
//        self.print_tail(store, w)
//      },
//      Comm(secret, payload) => {
//        // This requires a run-time coercion.
//        // Consider implementing the equivalent of CL's #. reader macro to let
//        // this happen at read-time.
//        write!(w, "(comm ")?;
//        let c =
//          store.commitment_hash(*secret,
// store.get_expr_hash(payload).unwrap());
//        Num(crate::num::Num::Scalar(c)).fmt(store, w)?;
//        write!(w, ")")
//      },
//      Opaque(f) => f.fmt(store, w),
//      Char(c) => {
//        write!(w, "#\\{}", c)
//      },
//      UInt(n) => write!(w, "{}u64", n),
//    }
//  }
//}

// impl<F: LurkField> Expression<'_, F> {
//  fn print_tail<W: io::Write>(
//    &self,
//    store: &Store<F>,
//    w: &mut W,
//  ) -> io::Result<()> {
//    match self {
//      Expression::Nil => write!(w, ")"),
//      Expression::Cons(car, cdr) => {
//        let car = store.fetch(car);
//        let cdr = store.fetch(cdr);
//        let fmt_car = |store, w: &mut W| {
//          if let Some(car) = car {
//            car.fmt(store, w)
//          }
//          else {
//            write!(w, "<Opaque>")
//          }
//        };
//        let fmt_cdr = |store, w: &mut W| {
//          if let Some(cdr) = &cdr {
//            cdr.fmt(store, w)
//          }
//          else {
//            write!(w, "<Opaque>")
//          }
//        };
//
//        match cdr {
//          Some(Expression::Nil) => {
//            fmt_car(store, w)?;
//            write!(w, ")")
//          },
//          Some(Expression::Cons(..)) => {
//            fmt_car(store, w)?;
//            write!(w, " ")?;
//            if let Some(cdr) = cdr {
//              cdr.print_tail(store, w)
//            }
//            else {
//              write!(w, "<Opaque Tail>")
//            }
//          },
//          Some(_) => {
//            fmt_car(store, w)?;
//            write!(w, " . ")?;
//            fmt_cdr(store, w)?;
//            write!(w, ")")
//          },
//          None => write!(w, "<Opaque>"),
//        }
//      },
//      expr => expr.fmt(store, w),
//    }
//  }
//}
// impl<F: LurkField> Write<F> for Continuation<F> {
//  fn fmt<W: io::Write>(&self, store: &Store<F>, w: &mut W) -> io::Result<()> {
//    match self {
//      Continuation::Outermost => write!(w, "Outermost"),
//      Continuation::Call0 { saved_env, continuation } => {
//        write!(w, "Call0{{ saved_env: ")?;
//        saved_env.fmt(store, w)?;
//        write!(w, ", ")?;
//        continuation.fmt(store, w)?;
//        write!(w, " }}")
//      },
//      Continuation::Call { unevaled_arg, saved_env, continuation } => {
//        write!(w, "Call{{ unevaled_arg: ")?;
//        unevaled_arg.fmt(store, w)?;
//        write!(w, ", saved_env: ")?;
//        saved_env.fmt(store, w)?;
//        write!(w, ", continuation: ")?;
//        continuation.fmt(store, w)?;
//        write!(w, " }}")
//      },
//      Continuation::Call2 { function, saved_env, continuation } => {
//        write!(w, "Call2{{ function: ")?;
//        function.fmt(store, w)?;
//        write!(w, ", saved_env: ")?;
//        saved_env.fmt(store, w)?;
//        write!(w, ", continuation: ")?;
//        continuation.fmt(store, w)?;
//        write!(w, " }}")
//      },
//      Continuation::Tail { saved_env, continuation } => {
//        write!(w, "Tail{{ saved_env: ")?;
//        saved_env.fmt(store, w)?;
//        write!(w, ", continuation: ")?;
//        continuation.fmt(store, w)?;
//        write!(w, " }}")
//      },
//      Continuation::Error => write!(w, "Error"),
//      Continuation::Lookup { saved_env, continuation } => {
//        write!(w, "Lookup{{ saved_env: ")?;
//        saved_env.fmt(store, w)?;
//        write!(w, ", ")?;
//        continuation.fmt(store, w)?;
//        write!(w, " }}")
//      },
//      Continuation::Unop { operator, continuation } => {
//        write!(w, "Unop{{ operator: {}, continuation: ", operator)?;
//        continuation.fmt(store, w)?;
//        write!(w, " }}")
//      },
//      Continuation::Binop {
//        operator,
//        saved_env,
//        unevaled_args,
//        continuation,
//      } => {
//        write!(w, "Binop{{ operator: ")?;
//        write!(w, "{}, unevaled_args: ", operator)?;
//        unevaled_args.fmt(store, w)?;
//        write!(w, ", saved_env: ")?;
//        saved_env.fmt(store, w)?;
//        write!(w, ", continuation: ")?;
//        continuation.fmt(store, w)?;
//        write!(w, " }}")
//      },
//      Continuation::Binop2 { operator, evaled_arg, continuation } => {
//        write!(w, "Binop2{{ operator: {}, evaled_arg: ", operator)?;
//        evaled_arg.fmt(store, w)?;
//        write!(w, ", continuation: ")?;
//        continuation.fmt(store, w)?;
//        write!(w, " }}")
//      },
//      Continuation::If { unevaled_args, continuation } => {
//        write!(w, "If{{ unevaled_args: ")?;
//        unevaled_args.fmt(store, w)?;
//        write!(w, ", continuation: ")?;
//        continuation.fmt(store, w)?;
//        write!(w, " }}")
//      },
//      Continuation::Let { var, body, saved_env, continuation } => {
//        write!(w, "Let{{ var: ")?;
//        var.fmt(store, w)?;
//        write!(w, ", body: ")?;
//        body.fmt(store, w)?;
//        write!(w, ", saved_env: ")?;
//        saved_env.fmt(store, w)?;
//        write!(w, ", continuation: ")?;
//        continuation.fmt(store, w)?;
//        write!(w, " }}")
//      },
//      Continuation::LetRec { var, saved_env, body, continuation } => {
//        write!(w, "LetRec{{var: ")?;
//        var.fmt(store, w)?;
//        write!(w, ", saved_env: ")?;
//        saved_env.fmt(store, w)?;
//        write!(w, ", body: ")?;
//        body.fmt(store, w)?;
//        write!(w, ", continuation: ")?;
//        continuation.fmt(store, w)?;
//        write!(w, " }}")
//      },
//      Continuation::Dummy => write!(w, "Dummy"),
//      Continuation::Terminal => write!(w, "Terminal"),
//      Continuation::Emit { continuation: _continuation } => {
//        write!(w, "Emit")?;
//        write!(w, "<CONTINUATION>") // Omit continuation for clarity when
//                                    // logging and using output.
//                                    // write!(w, " {{ continuation: ")?;
//                                    // continuation.fmt(store, w)?;
//                                    // write!(w, " }}")
//      },
//    }
//  }
//}
