#[macro_use]
extern crate nom;

use std::fmt;
use std::fmt::{Display, Debug, Formatter};

use std::str;
use std::str::FromStr;

use nom::{digit, multispace, IError};

pub enum Expr {
    Value(Arg),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Paren(Box<Expr>),
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

pub struct Arg(i64);

impl Display for Arg {
    fn fmt(&self, format: &mut Formatter) -> fmt::Result {
        write!(format, "{}", self.0)
    }
}

impl Debug for Arg {
    fn fmt(&self, format: &mut Formatter) -> fmt::Result {
        write!(format, "{:?}", self.0)
    }
}

impl FromStr for Arg {
    type Err = <i64 as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        <i64 as FromStr>::from_str(s).map(Arg)
    }
}

named!(parens< Expr >, delimited!(
    delimited!(opt!(multispace), tag!("("), opt!(multispace)),
    map!(map!(expr, Box::new), Expr::Paren),
    delimited!(opt!(multispace), tag!(")"), opt!(multispace))
  )
);

named!(factor< Expr >, alt_complete!(
    map!(
      map_res!(
        map_res!(
          delimited!(opt!(multispace), digit, opt!(multispace)),
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
}
