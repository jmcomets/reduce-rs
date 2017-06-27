#[macro_use]
extern crate nom;

use std::fmt;
use std::fmt::{Display, Debug, Formatter};

use std::str;
use std::str::FromStr;

use std::ops::{Add, Sub, Mul, Div};

use nom::{digit, IError};

type Arg = i32;

pub enum Expr {
    Value(Arg),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Paren(Box<Expr>),
}

trait Eval {
    type Output: Add<Output=Self::Output> + Sub<Output=Self::Output> + Mul<Output=Self::Output> + Div<Output=Self::Output>;

    fn eval(&self, arg: &Arg) -> Self::Output;
}

fn eval<F, V>(expr: &Expr, f: &F) -> V
    where 
          F: Fn(&Arg) -> V,
          V: Add<Output=V> + Sub<Output=V> + Mul<Output=V> + Div<Output=V>
{
    match expr {
        &Expr::Value(ref val)           => f(val),
        &Expr::Add(ref left, ref right) => eval(left, f) + eval(right, f),
        &Expr::Sub(ref left, ref right) => eval(left, f) - eval(right, f),
        &Expr::Mul(ref left, ref right) => eval(left, f) * eval(right, f),
        &Expr::Div(ref left, ref right) => eval(left, f) / eval(right, f),
        &Expr::Paren(ref expr)          => eval(expr, f),
    }
}

pub enum Oper {
    Add,
    Sub,
    Mul,
    Div,
}

impl Display for Expr {
    fn fmt(&self, format: &mut Formatter) -> fmt::Result {
        use self::Expr::*;
        match *self {
            Value(ref val) => write!(format, "{}", val),
            Add(ref left, ref right) => write!(format, "{} + {}", left, right),
            Sub(ref left, ref right) => write!(format, "{} - {}", left, right),
            Mul(ref left, ref right) => write!(format, "{} * {}", left, right),
            Div(ref left, ref right) => write!(format, "{} / {}", left, right),
            Paren(ref expr) => write!(format, "({})", expr),
        }
    }
}

impl Debug for Expr {
    fn fmt(&self, format: &mut Formatter) -> fmt::Result {
        use self::Expr::*;
        match *self {
            Value(ref val) => write!(format, "{:?}", val),
            Add(ref left, ref right) => write!(format, "({:?} + {:?})", left, right),
            Sub(ref left, ref right) => write!(format, "({:?} - {:?})", left, right),
            Mul(ref left, ref right) => write!(format, "({:?} * {:?})", left, right),
            Div(ref left, ref right) => write!(format, "({:?} / {:?})", left, right),
            Paren(ref expr) => write!(format, "[{:?}]", expr),
        }
    }
}

named!(parens< Expr >, ws!(
    delimited!(
        tag!("("),
        map!(
            map!(
                expr,
                Box::new
            ),
            Expr::Paren
        ),
        tag!(")"))
    )
);

named!(factor< Expr >, alt_complete!(
    map!(
      map_res!(
        map_res!(
          ws!(digit),
          str::from_utf8
        ),
      FromStr::from_str
    ),
    Expr::Value)
  | parens
  )
);

fn fold_exprs(initial: Expr, remainder: Vec<(Oper, Expr)>) -> Expr {
    remainder.into_iter().fold(initial, |acc, pair| {
        let (oper, expr) = pair;
        match oper {
            Oper::Add => Expr::Add(Box::new(acc), Box::new(expr)),
            Oper::Sub => Expr::Sub(Box::new(acc), Box::new(expr)),
            Oper::Mul => Expr::Mul(Box::new(acc), Box::new(expr)),
            Oper::Div => Expr::Div(Box::new(acc), Box::new(expr)),
        }
    })
}

named!(term< Expr >, do_parse!(
    initial: factor >>
    remainder: many0!(
           alt!(
             do_parse!(tag!("*") >> mul: factor >> (Oper::Mul, mul)) |
             do_parse!(tag!("/") >> div: factor >> (Oper::Div, div))
           )
         ) >>
    (fold_exprs(initial, remainder))
));

named!(expr< Expr >, do_parse!(
    initial: term >>
    remainder: many0!(
           alt!(
             do_parse!(tag!("+") >> add: term >> (Oper::Add, add)) |
             do_parse!(tag!("-") >> sub: term >> (Oper::Sub, sub))
           )
         ) >>
    (fold_exprs(initial, remainder))
));

#[derive(Debug)]
pub struct Lambda(Expr);

impl Lambda {
    pub fn from_str(s: &str) -> Result<Lambda, Error> {
        println!("{}", s);
        Lambda::from_bytes(s.as_bytes())
    }

    pub fn from_bytes(s: &[u8]) -> Result<Lambda, Error> {
        let e = expr(s)
            .to_full_result()
            .map_err(Error::ParseError)?;

        Ok(Lambda(e))
    }

    pub fn eval(&self) -> () {
        // TODO
    }
}

#[derive(Eq, PartialEq, Debug)]
pub enum Error {
    ParseError(IError),
}

#[cfg(test)]
mod tests {
    use super::*;

    use nom::IResult;

    #[test]
    fn factor_test() {
        assert_eq!(factor(&b"  3  "[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], String::from("3")));
    }

    #[test]
    fn term_test() {
        assert_eq!(term(&b" 3 *  5   "[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], String::from("(3 * 5)")));
    }

    #[test]
    fn expr_test() {
        assert_eq!(expr(&b" 1 + 2 *  3 "[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], String::from("(1 + (2 * 3))")));
        assert_eq!(expr(&b" 1 + 2 *  3 / 4 - 5 "[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], String::from("((1 + ((2 * 3) / 4)) - 5)")));
        assert_eq!(expr(&b" 72 / 2 / 3 "[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], String::from("((72 / 2) / 3)")));
    }

    #[test]
    fn parens_test() {
        assert_eq!(expr(&b" ( 1 + 2 ) *  3 "[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], String::from("([(1 + 2)] * 3)")));
    }

    #[test]
    fn multidigit_test() {
        assert_eq!(expr(&b"10"[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], "10".to_string()));
    }

    #[test]
    fn eval_test() {
        assert_eq!(expr(&b" ( 1 + 2 ) *  3 "[..]).map(|x| eval(&x, &|x| x.clone())),
                IResult::Done(&b""[..], 9));
    }
}
