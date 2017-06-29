#[macro_use]
extern crate nom;

use std::fmt;
use std::fmt::{Display, Debug, Formatter};

use std::str;
use std::str::FromStr;

use std::ops::{Add, Sub, Mul, Div};

use nom::{digit, IError};

pub trait Value : Add<Output=Self> + Sub<Output=Self> +
                  Mul<Output=Self> + Div<Output=Self> +
                  Clone + Default
{
}

impl<T> Value for T
    where T: Add<Output=T> + Sub<Output=T> +
             Mul<Output=T> + Div<Output=T> +
             Clone + Default
{
}

#[derive(Debug)]
pub struct Lambda(Expr);

impl Lambda {
    pub fn from_str(s: &str) -> Result<Lambda, ParseError> {
        expr(s.as_bytes())
            .to_full_result()
            .map(Lambda)
            .map_err(ParseError)
    }

    pub fn eval<V>(&self, args: &[V], zero: V) -> V
        where V: Value
    {
        self.0.eval(&|i| if i == 0 {
            zero.clone()
        } else if i > args.len() {
            V::default()
        } else {
            args[i - 1].clone()
        })
    }
}

#[derive(Eq, PartialEq, Debug)]
pub struct ParseError(IError);

type Arg = usize;

enum Expr {
    Value(Arg),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    // LessThan(Box<Expr>, Box<Expr>),
    // GreaterThan(Box<Expr>, Box<Expr>),
    // LessThanOrEqualTo(Box<Expr>, Box<Expr>),
    // GreaterThanOrEqualTo(Box<Expr>, Box<Expr>),
    Paren(Box<Expr>),
}

impl Expr {
    fn eval<F, V>(&self, f: &F) -> V
        where F: Fn(Arg) -> V,
              V: Add<Output = V> + Sub<Output = V> + Mul<Output = V> + Div<Output = V>
    {
        use Expr::*;
        match *self {
            Value(val)               => f(val),
            Add(ref left, ref right) => left.eval(f) + right.eval(f),
            Sub(ref left, ref right) => left.eval(f) - right.eval(f),
            Mul(ref left, ref right) => left.eval(f) * right.eval(f),
            Div(ref left, ref right) => left.eval(f) / right.eval(f),
            Paren(ref expr)          => expr.eval(f),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, format: &mut Formatter) -> fmt::Result {
        use Expr::*;
        match *self {
            Value(ref val)           => write!(format, "{}", val),
            Add(ref left, ref right) => write!(format, "{} + {}", left, right),
            Sub(ref left, ref right) => write!(format, "{} - {}", left, right),
            Mul(ref left, ref right) => write!(format, "{} * {}", left, right),
            Div(ref left, ref right) => write!(format, "{} / {}", left, right),
            Paren(ref expr)          => write!(format, "({})", expr),
        }
    }
}

impl Debug for Expr {
    fn fmt(&self, format: &mut Formatter) -> fmt::Result {
        use Expr::*;
        match *self {
            Value(ref val)           => write!(format, "{:?}", val),
            Add(ref left, ref right) => write!(format, "({:?} + {:?})", left, right),
            Sub(ref left, ref right) => write!(format, "({:?} - {:?})", left, right),
            Mul(ref left, ref right) => write!(format, "({:?} * {:?})", left, right),
            Div(ref left, ref right) => write!(format, "({:?} / {:?})", left, right),
            Paren(ref expr)          => write!(format, "[{:?}]", expr),
        }
    }
}

enum Oper {
    Add,
    Sub,
    Mul,
    Div,
}

impl Oper {
    fn to_expr(&self, lhs: Expr, rhs: Expr) -> Expr {
        use Oper::*;
        match *self {
            Add => Expr::Add(Box::new(lhs), Box::new(rhs)),
            Sub => Expr::Sub(Box::new(lhs), Box::new(rhs)),
            Mul => Expr::Mul(Box::new(lhs), Box::new(rhs)),
            Div => Expr::Div(Box::new(lhs), Box::new(rhs)),
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
        )
      );

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
            arg,
            Expr::Value
        )
        | parens
    )
);

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

named!(expr< Expr >, );

#[cfg(test)]
mod tests {
    use super::*;

    use nom::IResult;

    #[test]
    fn factor_test() {
        assert_eq!(factor(&b"  $3  "[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], String::from("3")));
    }

    #[test]
    fn term_test() {
        assert_eq!(term(&b" $3 *  $5   "[..]).map(|x| format!("{:?}", x)),
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
    fn multidigit_test() {
        assert_eq!(expr(&b"$10"[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], "10".to_string()));
    }

    #[test]
    fn eval_test() {
        assert_eq!(expr(&b" ( $1 + $2 ) *  $3 "[..]).map(|x| x.eval(&|v| v)),
                IResult::Done(&b""[..], 9));
    }
}
