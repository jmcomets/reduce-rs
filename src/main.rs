extern crate docopt;

extern crate serde;
#[macro_use] extern crate serde_derive;

extern crate reduce;

use std::io;
use std::io::{Write, BufRead, BufReader};

use std::process;

use std::str::FromStr;

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
    INIT        Start value for the fold (default: 0).

Options:
    -d=<delim>, --delimiter=<delim>     Sets the column delimiter [default: " "].
    --exit                              Use the exit code to output the result.
"#;

#[derive(Debug, Deserialize)]
struct Args {
    arg_lambda: String,
    arg_init: i32,
    flag_delimiter: String,
    flag_exit: bool,
}

fn main() {
    let args: Args = docopt::Docopt::new(USAGE)
        .and_then(|d| d.deserialize())
        .unwrap_or_else(|e| e.exit());
    //println!("args: {:?}", args);

    let init = args.arg_init;
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
                     .map(reduce::AutoInt::from)
                     .collect::<Vec<_>>()
             })

        // fold using the lambda expression
        .fold(reduce::AutoInt::from(init), |acc, vs| lambda.eval(&vs, acc));

    if args.flag_exit {
        process::exit(value.into());
    } else {
        println!("{}", value);
    }
}
