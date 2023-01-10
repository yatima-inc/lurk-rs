use ldon::op::{
  Op1,
  Op2,
};
use lurk_ff::LurkField;

use crate::{
  num::Num,
  ptr::Ptr,
  uint::UInt,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr<F: LurkField> {
  ConsNil,                                //
  Cons(Ptr<F>, Ptr<F>),                   // car, cdr
  Comm(Ptr<F>, Ptr<F>),                   // secret, payload
  SymNil,                                 //
  SymCons(Ptr<F>, Ptr<F>),                // head, tail
  Fun(Ptr<F>, Ptr<F>, Ptr<F>),            // arg, body, env
  Num(Num<F>),                            //
  StrNil,                                 //
  StrCons(Ptr<F>, Ptr<F>),                // head, tail
  Thunk(Ptr<F>, Ptr<F>),                  // val, cont
  Char(char),                             //
  UInt(UInt),                             //
  Op1(Op1),                               //
  Op2(Op2),                               //
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
