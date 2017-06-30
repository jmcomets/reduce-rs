use std::fmt;
use std::fmt::{Display, Debug, Formatter};

use std::str;
use std::str::FromStr;

use std::ops::{Add, Sub, Mul, Div};

use nom::{digit, IError};

pub trait BinaryOps : Add<Output=Self> + Sub<Output=Self> +
                      Mul<Output=Self> + Div<Output=Self> +
                      PartialOrd + From<bool>
    where Self: Sized
{
}

impl<T> BinaryOps for T
    where T: Add<Output=T> + Sub<Output=T> +
             Mul<Output=T> + Div<Output=T> +
             PartialOrd + From<bool>
{
}

type Arg = usize;

pub enum Expr {
    Value(Arg),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    LessThan(Box<Expr>, Box<Expr>),
    GreaterThan(Box<Expr>, Box<Expr>),
    // LessThanOrEqualTo(Box<Expr>, Box<Expr>),
    // GreaterThanOrEqualTo(Box<Expr>, Box<Expr>),
    Paren(Box<Expr>),
}

impl FromStr for Expr {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Expr, ParseError> {
        expr(s.as_bytes())
            .to_full_result()
            .map_err(ParseError)
    }
}

#[derive(Eq, PartialEq, Debug)]
pub struct ParseError(IError);

impl Expr {
    pub fn eval<F, V>(&self, f: &F) -> V
        where F: Fn(Arg) -> V,
              V: BinaryOps,
    {
        use self::Expr::*;
        match *self {
            Value(val)                       => f(val),
            Add(ref left, ref right)         => left.eval(f) + right.eval(f),
            Sub(ref left, ref right)         => left.eval(f) - right.eval(f),
            Mul(ref left, ref right)         => left.eval(f) * right.eval(f),
            Div(ref left, ref right)         => left.eval(f) / right.eval(f),
            LessThan(ref left, ref right)    => (left.eval(f) < right.eval(f)).into(),
            GreaterThan(ref left, ref right) => (left.eval(f) > right.eval(f)).into(),
            // LessThanOrEqualTo(ref left, ref right)    => (left.eval(f) <= right.eval(f)).into(),
            // GreaterThanOrEqualTo(ref left, ref right) => (left.eval(f) >= right.eval(f)).into(),
            Paren(ref expr)                  => expr.eval(f),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, format: &mut Formatter) -> fmt::Result {
        use self::Expr::*;
        match *self {
            Value(ref val)                            => write!(format, "{}", val),
            Add(ref left, ref right)                  => write!(format, "{} + {}", left, right),
            Sub(ref left, ref right)                  => write!(format, "{} - {}", left, right),
            Mul(ref left, ref right)                  => write!(format, "{} * {}", left, right),
            Div(ref left, ref right)                  => write!(format, "{} / {}", left, right),
            LessThan(ref left, ref right)             => write!(format, "{} < {}", left, right),
            GreaterThan(ref left, ref right)          => write!(format, "{} > {}", left, right),
            // LessThanOrEqualTo(ref left, ref right)    => write!(format, "{} <= {}", left, right),
            // GreaterThanOrEqualTo(ref left, ref right) => write!(format, "{} >= {}", left, right),
            Paren(ref expr)                           => write!(format, "({})", expr),
        }
    }
}

impl Debug for Expr {
    fn fmt(&self, format: &mut Formatter) -> fmt::Result {
        use self::Expr::*;
        match *self {
            Value(ref val)                            => write!(format, "{:?}", val),
            Add(ref left, ref right)                  => write!(format, "({:?} + {:?})", left, right),
            Sub(ref left, ref right)                  => write!(format, "({:?} - {:?})", left, right),
            Mul(ref left, ref right)                  => write!(format, "({:?} * {:?})", left, right),
            Div(ref left, ref right)                  => write!(format, "({:?} / {:?})", left, right),
            LessThan(ref left, ref right)             => write!(format, "({:?} < {:?})", left, right),
            GreaterThan(ref left, ref right)          => write!(format, "({:?} > {:?})", left, right),
            // LessThanOrEqualTo(ref left, ref right)    => write!(format, "({:?} < {:?})", left, right),
            // GreaterThanOrEqualTo(ref left, ref right) => write!(format, "({:?} > {:?})", left, right),
            Paren(ref expr)                           => write!(format, "[{:?}]", expr),
        }
    }
}

enum Oper {
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    GreaterThan,
    // LessThanOrEqualTo,
    // GreaterThanOrEqualTo,
}

impl Oper {
    fn to_expr(&self, lhs: Expr, rhs: Expr) -> Expr {
        use self::Oper::*;
        match *self {
            Add                  => Expr::Add(Box::new(lhs), Box::new(rhs)),
            Sub                  => Expr::Sub(Box::new(lhs), Box::new(rhs)),
            Mul                  => Expr::Mul(Box::new(lhs), Box::new(rhs)),
            Div                  => Expr::Div(Box::new(lhs), Box::new(rhs)),
            LessThan             => Expr::LessThan(Box::new(lhs), Box::new(rhs)),
            GreaterThan          => Expr::GreaterThan(Box::new(lhs), Box::new(rhs)),
            // LessThanOrEqualTo    => Expr::LessThanOrEqualTo(Box::new(lhs), Box::new(rhs)),
            // GreaterThanOrEqualTo => Expr::GreaterThanOrEqualTo(Box::new(lhs), Box::new(rhs)),
        }
    }
}

fn fold_exprs(initial: Expr, remainder: Vec<(Oper, Expr)>) -> Expr {
    remainder
        .into_iter()
        .fold(initial, |lhs, (oper, rhs)| oper.to_expr(lhs, rhs))
}

named!(arg< Arg >, ws!(
    map_res!(
        map_res!(
            do_parse!(
                char!('$') >>
                d: digit >>
                (d)
            ),
            str::from_utf8
            ),
            FromStr::from_str)
));

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
    ));

named!(value< Expr >, alt_complete!(
        map!(
            arg,
            Expr::Value
        )
        | parens
    ));

named!(mul_div< Expr >, do_parse!(
    initial: value >>
    remainder: many0!(
           alt!(
             do_parse!(tag!("*") >> mul: value >> (Oper::Mul, mul)) |
             do_parse!(tag!("/") >> div: value >> (Oper::Div, div))
           )
         ) >>
    (fold_exprs(initial, remainder))
));

named!(add_sub< Expr >, do_parse!(
    initial: mul_div >>
    remainder: many0!(
           alt!(
             do_parse!(tag!("+") >> add: mul_div >> (Oper::Add, add)) |
             do_parse!(tag!("-") >> sub: mul_div >> (Oper::Sub, sub))
           )
         ) >>
    (fold_exprs(initial, remainder))
));

named!(gt_lt< Expr >, do_parse!(
    initial: add_sub >>
    remainder: many0!(
           alt!(
             do_parse!(tag!("<") >> lt: add_sub >> (Oper::LessThan, lt)) |
             do_parse!(tag!(">") >> gt: add_sub >> (Oper::GreaterThan, gt))// |
             // do_parse!(tag!("<=") >> lt: add_sub >> (Oper::LessThanOrEqualTo, lt)) |
             // do_parse!(tag!(">=") >> gt: add_sub >> (Oper::GreaterThanOrEqualTo, gt))
           )
         ) >>
    (fold_exprs(initial, remainder))
));

named!(expr< Expr >, do_parse!(
    e: gt_lt >>
    (e)
));

#[cfg(test)]
mod tests {
    use super::*;

    use nom::IResult;

    #[test]
    fn factor_test() {
        assert_eq!(value(&b"  $3  "[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], String::from("3")));
    }

    #[test]
    fn term_test() {
        assert_eq!(mul_div(&b" $3 *  $5   "[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], String::from("(3 * 5)")));
    }

    #[test]
    fn expr_test() {
        assert_eq!(expr(&b" $1 + $2 *  $3 "[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], String::from("(1 + (2 * 3))")));
        assert_eq!(expr(&b" $1 + $2 *  $3 / $4 - $5 "[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], String::from("((1 + ((2 * 3) / 4)) - 5)")));
        assert_eq!(expr(&b" $72 / $2 / $3 "[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], String::from("((72 / 2) / 3)")));
    }

    #[test]
    fn parens_test() {
        assert_eq!(expr(&b" ( $1 + $2 ) *  $3 "[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], String::from("([(1 + 2)] * 3)")));
    }

    #[test]
    fn gt_lt_test() {
        assert_eq!(expr(&b"$1 < $2"[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], String::from("(1 < 2)")));
    }

    #[test]
    fn multidigit_test() {
        assert_eq!(expr(&b"$10"[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], "10".to_string()));
    }

    #[test]
    fn eval_test() {
        use auto_int::AutoInt;
        assert_eq!(expr(&b" ( $1 + $2 ) *  $3 "[..]).map(|x| x.eval(&|v| AutoInt::from(v as i32))),
                IResult::Done(&b""[..], 9.into()));
    }
}
