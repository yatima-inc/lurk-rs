use lurk_ff::field::LurkField;
use nom::{
  branch::alt,
  bytes::complete::{
    tag,
    take_till,
  },
  character::complete::{
    char,
    multispace0,
    multispace1,
    none_of,
  },
  combinator::{
    peek,
    success,
    value,
  },
  multi::{
    many0,
    separated_list0,
    separated_list1,
  },
  sequence::{
    preceded,
    terminated,
  },
  IResult,
};

use crate::{
  lurksym,
  parser::{
    base,
    error::{
      ParseError,
      ParseErrorKind,
    },
    position::Pos,
    string,
    Span,
  },
  sym,
  sym::Symbol,
  syntax::Syn,
};
pub fn parse_line_comment<F: LurkField>(
  i: Span,
) -> IResult<Span, Span, ParseError<Span, F>> {
  let (i, _) = tag("//")(i)?;
  let (i, com) = take_till(|c| c == '\n')(i)?;
  Ok((i, com))
}
pub fn parse_space<F: LurkField>(
  i: Span,
) -> IResult<Span, Vec<Span>, ParseError<Span, F>> {
  let (i, _) = multispace0(i)?;
  let (i, com) = many0(terminated(parse_line_comment, multispace1))(i)?;
  Ok((i, com))
}
pub fn parse_space1<F: LurkField>(
  i: Span,
) -> IResult<Span, Vec<Span>, ParseError<Span, F>> {
  let (i, _) = multispace1(i)?;
  let (i, com) = many0(terminated(parse_line_comment, multispace1))(i)?;
  Ok((i, com))
}

pub fn parse_symbol<F: LurkField>(
) -> impl Fn(Span) -> IResult<Span, Symbol, ParseError<Span, F>> {
  move |from: Span| {
    let key_mark = sym::KEYWORD_MARKER;
    let sym_mark = sym::SYM_MARKER;
    let sym_sep = sym::SYM_SEPARATOR;
    let (i, (is_root, mark)) = alt((
      value((true, key_mark), tag("_:")),
      value((true, sym_mark), tag("_.")),
      // .foo
      value((false, sym_mark), char(sym_mark)),
      // :foo
      value((false, key_mark), char(key_mark)),
      // foo
      value((false, sym_mark), peek(none_of(",=(){}[]1234567890"))),
    ))(from)?;
    if is_root && mark == key_mark {
      Ok((i, Symbol::Key(vec![])))
    }
    else if is_root && mark == sym_mark {
      Ok((i, Symbol::Sym(vec![])))
    }
    else {
      let (i, limbs) = separated_list1(
        char(sym_sep),
        string::parse_string_inner(sym_sep, false, sym::ESCAPE_CHARS),
      )(i)?;
      // println!("limbs {:?}", limbs);
      if mark == key_mark {
        Ok((i, Symbol::Key(limbs)))
      }
      else {
        Ok((i, Symbol::Sym(limbs)))
      }
    }
  }
}
pub fn parse_syn_sym<F: LurkField>(
) -> impl Fn(Span) -> IResult<Span, Syn<F>, ParseError<Span, F>> {
  move |from: Span| {
    let (upto, sym) = parse_symbol()(from)?;
    let pos = Pos::from_upto(from, upto);
    Ok((upto, Syn::Symbol(pos, sym)))
  }
}

pub fn parse_syn_u64<F: LurkField>(
) -> impl Fn(Span) -> IResult<Span, Syn<F>, ParseError<Span, F>> {
  move |from: Span| {
    let (i, base) = alt((
      preceded(tag("0"), base::parse_litbase_code()),
      success(base::LitBase::Dec),
    ))(from)?;
    let (i, digits) = base::parse_litbase_digits(base)(i)?;
    // when more uint types are supported we can do:
    // alt((tag("u8"), tag("u16"), tag("u32"), tag("u64"), tag("u128")))
    let (upto, suffix) = tag("u64")(i)?;
    match *suffix.fragment() {
      "u64" => {
        let (_, x) = ParseError::res(
          u64::from_str_radix(&digits, base.radix()),
          from,
          |e| ParseErrorKind::ParseIntErr(e),
        )?;
        let pos = Pos::from_upto(from, upto);
        Ok((upto, Syn::U64(pos, x)))
      },
      _ => unreachable!("implementation error in parse_nat"),
    }
  }
}

pub fn parse_syn_num<F: LurkField>(
) -> impl Fn(Span) -> IResult<Span, Syn<F>, ParseError<Span, F>> {
  move |from: Span| {
    let (i, base) = alt((
      preceded(tag("0"), base::parse_litbase_code()),
      success(base::LitBase::Dec),
    ))(from)?;
    let (upto, bytes): (Span, Vec<u8>) = base::parse_litbase_le_bytes(base)(i)?;
    let max_bytes = (F::zero() - F::one()).to_le_bytes_canonical();
    let max_uint = num_bigint::BigUint::from_bytes_le(&max_bytes);
    if num_bigint::BigUint::from_bytes_le(&bytes) > max_uint {
      ParseError::throw(
        from,
        ParseErrorKind::NumLiteralTooBig(F::most_positive(), max_uint),
      )
    }
    else {
      let pos = Pos::from_upto(from, upto);
      Ok((upto, Syn::Num(pos, F::from_le_bytes_canonical(&bytes))))
    }
  }
}

pub fn parse_syn_str<F: LurkField>(
) -> impl Fn(Span) -> IResult<Span, Syn<F>, ParseError<Span, F>> {
  move |from: Span| {
    let (upto, s) = string::parse_string('"')(from)?;
    let pos = Pos::from_upto(from, upto);
    Ok((upto, Syn::String(pos, s)))
  }
}

pub fn parse_syn_char<F: LurkField>(
) -> impl Fn(Span) -> IResult<Span, Syn<F>, ParseError<Span, F>> {
  move |from: Span| {
    let (upto, s) = string::parse_string('\'')(from)?;
    let mut chars: Vec<char> = s.chars().collect();
    if chars.len() == 1 {
      let c = chars.pop().unwrap();
      let pos = Pos::from_upto(from, upto);
      Ok((upto, Syn::Char(pos, c)))
    }
    else {
      ParseError::throw(from, ParseErrorKind::InvalidChar(s))
    }
  }
}

// (1, 2)
// FIXME: if the end of an improper list is a list, the result is a single list
pub fn parse_syn_list_improper<F: LurkField>(
) -> impl Fn(Span) -> IResult<Span, Syn<F>, ParseError<Span, F>> {
  move |from: Span| {
    let (i, _) = tag("(")(from)?;
    let (i, mut xs) = separated_list1(
      preceded(parse_space, tag(",")),
      preceded(parse_space, parse_syn()),
    )(i)?;
    let (upto, _) = preceded(parse_space, tag(")"))(i)?;
    // separated_list1 is guaranteed to return at least 1 thing
    let end = match xs.pop().unwrap() {
      // if the end is a list, we want to merge it into our current list
      Syn::List(_, end_xs, end_end) => {
        xs.extend(end_xs);
        end_end
      },
      x => Box::new(x),
    };
    let pos = Pos::from_upto(from, upto);
    Ok((upto, Syn::List(pos, xs, end)))
  }
}

pub fn parse_syn_list_proper<F: LurkField>(
) -> impl Fn(Span) -> IResult<Span, Syn<F>, ParseError<Span, F>> {
  move |from: Span| {
    let (i, _) = tag("(")(from)?;
    let (i, _) = parse_space(i)?;
    let (i, xs) = separated_list0(parse_space1, parse_syn())(i)?;
    // println!("list xs {:?}", xs);
    let (upto, _) = tag(")")(i)?;
    let pos = Pos::from_upto(from, upto);
    let nil = Syn::Symbol(pos, lurksym!["nil"]);
    if xs.is_empty() {
      Ok((upto, nil))
    }
    else {
      Ok((upto, Syn::List(pos, xs, Box::new(nil))))
    }
  }
}

pub fn parse_syn_link<F: LurkField>(
) -> impl Fn(Span) -> IResult<Span, Syn<F>, ParseError<Span, F>> {
  move |from: Span| {
    let (i, _) = tag("[")(from)?;
    let (i, _) = parse_space(i)?;
    let (i, ctx) = parse_syn()(i)?;
    let (i, _) = parse_space(i)?;
    let (i, xs) = separated_list0(parse_space1, parse_syn_u64())(i)?;
    let mut xs2 = vec![];
    for x in xs {
      match x {
        Syn::U64(_, x) => xs2.push(x),
        _ => unreachable!(
          "xs should only be generated from a list of Syn from parse_syn_u64"
        ),
      }
    }
    let (upto, _) = tag("]")(i)?;
    let pos = Pos::from_upto(from, upto);
    Ok((upto, Syn::Link(pos, Box::new(ctx), xs2)))
  }
}

pub fn parse_syn_map_entry<F: LurkField>(
) -> impl Fn(Span) -> IResult<Span, (Syn<F>, Syn<F>), ParseError<Span, F>> {
  move |from: Span| {
    let (i, key) = parse_syn()(from)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = tag("=")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, val) = parse_syn()(i)?;
    Ok((i, (key, val)))
  }
}

pub fn parse_syn_map<F: LurkField>(
) -> impl Fn(Span) -> IResult<Span, Syn<F>, ParseError<Span, F>> {
  move |from: Span| {
    let (i, _) = tag("{")(from)?;
    let (i, xs) = separated_list0(
      preceded(parse_space, tag(",")),
      preceded(parse_space, parse_syn_map_entry()),
    )(i)?;
    let (upto, _) = preceded(parse_space, tag("}"))(i)?;
    let pos = Pos::from_upto(from, upto);
    Ok((upto, Syn::Map(pos, xs)))
  }
}

// top-level syntax parser
pub fn parse_syn<F: LurkField>(
) -> impl Fn(Span) -> IResult<Span, Syn<F>, ParseError<Span, F>> {
  move |from: Span| {
    alt((
      parse_syn_char(),
      parse_syn_str(),
      parse_syn_u64(),
      parse_syn_num(),
      parse_syn_list_proper(),
      parse_syn_list_improper(),
      parse_syn_link(),
      parse_syn_map(),
      parse_syn_sym(),
    ))(from)
  }
}

#[cfg(test)]
pub mod tests {
  use blstrs::Scalar as Fr;
  use lurk_ff::field::FWrap;
  use nom::Parser;

  use super::*;
  #[allow(unused_imports)]
  use crate::{
    char,
    keyword,
    list,
    map,
    num,
    str,
    symbol,
    u64,
  };

  fn test<'a, P>(mut p: P, i: &'a str, expected: Option<Syn<Fr>>) -> bool
  where P: Parser<Span<'a>, Syn<Fr>, ParseError<Span<'a>, Fr>> {
    match (expected, p.parse(Span::new(i))) {
      (Some(expected), Ok((_i, x))) if x == expected => true,
      (Some(expected), Ok((i, x))) => {
        println!("input: {:?}", i);
        println!("expected: {}", expected);
        println!("detected: {}", x);
        false
      },
      (Some(..), Err(e)) => {
        println!("{}", e);
        false
      },
      (None, Ok((i, x))) => {
        println!("input: {:?}", i);
        println!("expected parse error");
        println!("detected: {:?}", x);
        false
      },
      (None, Err(_e)) => true,
    }
  }

  #[test]
  fn unit_parse_string() {
    assert!(test(parse_syn_str(), "\"foo\"", Some(str!("foo"))));
    assert!(test(parse_syn_str(), "\"fo\\no\"", Some(str!("fo\no"))));
    assert!(test(parse_syn_str(), "\"fo\\u{00}o\"", Some(str!("fo\u{00}o"))));
    assert!(test(parse_syn_str(), "\"foo\\   \"", Some(str!("foo"))));
  }

  #[test]
  fn unit_parse_symbol() {
    assert!(test(parse_syn_sym(), "", None));
    assert!(test(parse_syn(), "_.", Some(symbol!([]))));
    assert!(test(parse_syn(), ".", Some(symbol!([""]))));
    assert!(test(parse_syn(), "..", Some(symbol!(["", ""]))));
    assert!(test(parse_syn(), "foo", Some(symbol!(["foo"]))));
    assert!(test(parse_syn(), ".foo", Some(symbol!(["foo"]))));
    assert!(test(parse_syn(), "..foo", Some(symbol!(["", "foo"]))));
    assert!(test(parse_syn(), "foo.", Some(symbol!(["foo", ""]))));
    assert!(test(parse_syn(), ".foo.", Some(symbol!(["foo", ""]))));
    assert!(test(parse_syn(), ".foo..", Some(symbol!(["foo", "", ""]))));
    assert!(test(parse_syn(), ".foo.bar", Some(symbol!(["foo", "bar"]))));
    assert!(test(parse_syn(), ".foo?.bar?", Some(symbol!(["foo?", "bar?"]))));
    assert!(test(parse_syn(), ".fooλ.barλ", Some(symbol!(["fooλ", "barλ"]))));
    assert!(test(
      parse_syn(),
      ".foo\\n.bar\\n",
      Some(symbol!(["foo\n", "bar\n"]))
    ));
    assert!(test(
      parse_syn(),
      ".foo\\u{00}.bar\\u{00}",
      Some(symbol!(["foo\u{00}", "bar\u{00}"]))
    ));
    assert!(test(parse_syn(), ".foo\\.bar", Some(symbol!(["foo.bar"]))));
  }

  #[test]
  fn unit_parse_keyword() {
    assert!(test(parse_syn_sym(), "", None));
    assert!(test(parse_syn(), "_:", Some(keyword!([]))));
    assert!(test(parse_syn(), ":", Some(keyword!([""]))));
    assert!(test(parse_syn(), ":.", Some(keyword!(["", ""]))));
    assert!(test(parse_syn(), ":foo", Some(keyword!(["foo"]))));
    assert!(test(parse_syn(), ":.foo", Some(keyword!(["", "foo"]))));
    assert!(test(parse_syn(), ":foo.", Some(keyword!(["foo", ""]))));
    assert!(test(parse_syn(), ":foo..", Some(keyword!(["foo", "", ""]))));
    assert!(test(parse_syn(), ":foo.bar", Some(keyword!(["foo", "bar"]))));
    assert!(test(parse_syn(), ":foo?.bar?", Some(keyword!(["foo?", "bar?"]))));
    assert!(test(parse_syn(), ":fooλ.barλ", Some(keyword!(["fooλ", "barλ"]))));
    assert!(test(
      parse_syn(),
      ":foo\\n.bar\\n",
      Some(keyword!(["foo\n", "bar\n"]))
    ));
    assert!(test(
      parse_syn(),
      ":foo\\u{00}.bar\\u{00}",
      Some(keyword!(["foo\u{00}", "bar\u{00}"]))
    ));
    assert!(test(parse_syn(), ":foo\\.bar", Some(keyword!(["foo.bar"]))));
  }

  #[test]
  fn unit_parse_map() {
    assert!(test(parse_syn(), "{}", Some(map!([]))));
    assert!(test(
      parse_syn(),
      "{ 'a' = 1u64,  'b' = 2u64,  'c' = 3u64 }",
      Some(map!([
        (char!('a'), u64!(1)),
        (char!('b'), u64!(2)),
        (char!('c'), u64!(3))
      ]))
    ));
    assert!(test(
      parse_syn(),
      "{ :a = 1u64,  :b = 2u64,  :c = 3u64 }",
      Some(map!([
        (keyword!(["a"]), u64!(1)),
        (keyword!(["b"]), u64!(2)),
        (keyword!(["c"]), u64!(3))
      ]))
    ));
  }

  #[test]
  fn unit_parse_list() {
    assert!(test(parse_syn(), "()", Some(symbol!(["lurk", "nil"]))));
    assert!(test(
      parse_syn(),
      "(a b)",
      Some(list!([symbol!(["a"]), symbol!(["b"])])),
    ));
    assert!(test(
      parse_syn(),
      "(.a .b)",
      Some(list!([symbol!(["a"]), symbol!(["b"])])),
    ));
    assert!(test(
      parse_syn(),
      "(.LURK.LAMBDA .LURK.LAMBDA)",
      Some(list!([symbol!(["LURK", "LAMBDA"]), symbol!(["LURK", "LAMBDA"])])),
    ));
    assert!(test(
      parse_syn(),
      "(a, b)",
      Some(list!([symbol!(["a"])], symbol!(["b"]))),
    ));
    assert!(test(
      parse_syn(),
      "(.a, .b)",
      Some(list!([symbol!(["a"])], symbol!(["b"]))),
    ));
    assert!(test(
      parse_syn(),
      "(a, b, c)",
      Some(list!([symbol!(["a"]), symbol!(["b"])], symbol!(["c"]))),
    ));
    assert!(test(
      parse_syn(),
      "(a, (b, c))",
      Some(list!([symbol!(["a"]), symbol!(["b"])], symbol!(["c"]))),
    ));
    assert!(test(
      parse_syn(),
      "(a b c)",
      Some(list!([symbol!(["a"]), symbol!(["b"]), symbol!(["c"])])),
    ));
    assert!(test(
      parse_syn(),
      "('a' 'b' 'c')",
      Some(list!([char!('a'), char!('b'), char!('c')])),
    ));
  }

  #[test]
  fn unit_parse_char() {
    assert!(test(parse_syn(), "'a'", Some(char!('a'))));
    assert!(test(parse_syn(), "'b'", Some(char!('b'))));
    assert!(test(parse_syn(), "'\\u{8f}'", Some(char!('\u{8f}'))));
  }

  #[test]
  fn unit_parse_num() {
    assert!(test(parse_syn(), "0", Some(num!(0))));
    assert!(test(parse_syn(), "0b0", Some(num!(0))));
    assert!(test(parse_syn(), "0o0", Some(num!(0))));
    assert!(test(parse_syn(), "0d0", Some(num!(0))));
    assert!(test(parse_syn(), "0x0", Some(num!(0))));
    assert!(test(
      parse_syn(),
      "0xffff_ffff_ffff_ffff",
      Some(num!(0xffff_ffff_ffff_ffff))
    ));
    assert!(test(
      parse_syn(),
      "0x1234_5678_9abc_def0",
      Some(num!(0x1234_5678_9abc_def0))
    ));
    assert!(test(
      parse_syn(),
      &format!("0x{}", Fr::most_positive().hex_digits()),
      Some(num!(Fr::most_positive()))
    ));
    assert!(test(
      parse_syn(),
      "0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000000",
      Some(Syn::Num(Pos::No, <Fr as ff::Field>::zero() - Fr::from(1u64))),
    ));
    assert!(test(
      parse_syn(),
      "0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001",
      None,
    ));
  }

  // (1 2)
  // Cons(Num(1), Num(2))
  // (.1 .2)
  // Cons(Sym(vec!["1"]), Sym(vec!["2"]))
  #[test]
  fn unit_parse_syn_misc() {
    let vec: Vec<u8> = vec![
      0x6e, 0x2e, 0x50, 0x55, 0xdc, 0xf6, 0x14, 0x86, 0xb0, 0x3b, 0xb8, 0x0e,
      0xd2, 0xb3, 0xf1, 0xa3, 0x5c, 0x30, 0xe1, 0x22, 0xde, 0xfe, 0xba, 0xe8,
      0x24, 0xfa, 0xe4, 0xed, 0x32, 0x40, 0x8e, 0x87,
    ]
    .into_iter()
    .rev()
    .collect();
    assert!(test(
      parse_syn(),
      "(0x6e2e5055dcf61486b03bb80ed2b3f1a35c30e122defebae824fae4ed32408e87)",
      Some(list!([num!(Fr::from_le_bytes_canonical(&vec))])),
    ));

    assert!(test(parse_syn(), ".\\.", Some(symbol!(["."]))));
    assert!(test(parse_syn(), ".\\'", Some(symbol!(["'"]))));
    assert!(test(
      parse_syn(),
      ".\\'\\u{8e}\\u{fffc}\\u{201b}",
      Some(symbol!(["'\u{8e}\u{fffc}\u{201b}"])),
    ));
    assert!(test(
      parse_syn(),
      "(lambda (🚀) 🚀)",
      Some(list!([
        symbol!(["lambda"]),
        list!([symbol!(["🚀"])]),
        symbol!(["🚀"])
      ])),
    ));
    assert!(test(
      parse_syn(),
      "(_:, 11242421860377074631u64, :\u{ae}\u{60500}\u{87}..)",
      Some(list!(
        [keyword!([]), u64!(11242421860377074631)],
        keyword!(["®\u{60500}\u{87}", "", ""])
      ))
    ));
  }

  #[quickcheck]
  fn prop_parse_num(f: FWrap<Fr>) -> bool {
    let hex = format!("0x{}", f.0.hex_digits());
    match parse_syn_num::<Fr>()(Span::new(&hex)) {
      Ok((_, Syn::Num(_, f2))) => {
        println!("f1 0x{}", f.0.hex_digits());
        println!("f2 0x{}", f2.hex_digits());
        f.0 == f2
      },
      _ => false,
    }
  }
  #[quickcheck]
  fn prop_syn_parse_print(syn: Syn<Fr>) -> bool {
    println!("==================");
    println!("syn1 {}", syn);
    println!("syn1 {:?}", syn);
    let hex = format!("{}", syn);
    match parse_syn::<Fr>()(Span::new(&hex)) {
      Ok((_, syn2)) => {
        println!("syn2 {}", syn2);
        println!("syn2 {:?}", syn2);
        syn == syn2
      },
      _ => false,
    }
  }
}
