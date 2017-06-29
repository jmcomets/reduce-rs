extern crate reduce;

extern crate docopt;
extern crate serde;
#[macro_use]
extern crate serde_derive;

use std::error::Error;

use std::io;
use std::io::{Write, BufRead, BufReader};

use std::process;

use std::fmt;

use docopt::Docopt;

use serde::de::{Deserialize, Deserializer, Error as DeserializeError, Visitor};

const USAGE: &str = r#"
Usage:
    reduce [options] <lambda> [<init>]
    reduce (-h|--help)

Apply the lambda expression to the input as a fold, with the lines being the list and the columns
being the arguments. Integer, floating or boolean arithmetics are available, depending on the type
chosen (defaults to integer).

Arguments:
    LAMBDA      Valid expression for the current type, args are positional, and prefixed with a '$'
                sign (ex: $1, $2). $0 is special as it represents the fold accumulator.
    INIT        Start value for the fold. Defaults:
                    "0" for integers
                    "0.0" for doubles
                    "true" for booleans

Options:
    -t=<type>, --type=<type>            Sets the input type for the lambda expression [default: int].
    -d=<delim>, --delimiter=<delim>     Sets the column delimiter [default: " "].
    --exit                              Use the exit code to output the result.
"#;


struct ReturnTypeVisitor;

impl<'de> Visitor<'de> for ReturnTypeVisitor {
    type Value = ReturnType;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a type int, float or bool")
    }

    fn visit_str<E>(self, s: &str) -> Result<Self::Value, E>
        where E: DeserializeError
    {
        match s {
            "bool"  => Ok(ReturnType::Boolean),
            "float" => Ok(ReturnType::Floating),
            "int"   => Ok(ReturnType::Integer),
            _       => Err(DeserializeError::custom(format!("invalid type {}", s))),
        }
    }
}

#[derive(Debug)]
enum ReturnType {
    Boolean,
    Floating,
    Integer,
}

impl<'de> Deserialize<'de> for ReturnType {
    fn deserialize<D>(d: D) -> Result<ReturnType, D::Error>
        where D: Deserializer<'de>
    {
        d.deserialize_str(ReturnTypeVisitor)
    }
}

#[derive(Debug, Deserialize)]
struct Args {
    arg_lambda: String,
    arg_init: Option<String>,
    flag_type: ReturnType,
    flag_delimiter: String,
    flag_exit: bool,
}

fn main() {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.deserialize())
        .unwrap_or_else(|e| e.exit());
    //println!("args: {:?}", args);

    let init = args.arg_init
        .map(|v| v.parse::<i32>().unwrap())
        .unwrap_or_default();
    let lambda = reduce::Lambda::from_str(&args.arg_lambda).unwrap();
    let delimiter = args.flag_delimiter.chars().nth(1).unwrap();
    //println!("evaluating: {:?}", lambda);

    let input = io::stdin();
    let reader = BufReader::new(input.lock());

    let value = reader
        .lines()

        // validate the lines
        .enumerate()
        .map(|(i, r)| {
                 r.unwrap_or_else(|e| {
                                      let mut err = io::stderr();
                                      writeln!(err, "Failed to read line {}: {:?}", i, e).unwrap();
                                      process::exit(1);
                                  })
             })

        // parse the arguments for each line
        .map(|l| {
                 l.split(delimiter)
                     .map(|s| s.parse::<i32>().unwrap())
                     .collect::<Vec<_>>()
             })

        // fold using the lambda expression
        .fold(init, |acc, vs| lambda.eval(&vs, acc));

    if args.flag_exit {
        process::exit(value);
    } else {
        println!("{}", value);
    }
}

// fn parse_value(s: &str, returnType: ReturnType) -> Result<Box<reduce::Value>, Box<Error>> {
//     use ReturnType::*;

//     match returnType {
//         Boolean  => s.parse::<bool>().map(Box::new).map_err(Box::new),
//         Integer  => s.parse::<i32>().map(Box::new).map_err(Box::new),
//         Floating => s.parse::<f32>().map(Box::new).map_err(Box::new),
//     }
// }
