reduce
======

File-based arithmetic to use in the shell.

```
Usage: reduce [options] LAMBDA [INIT]

Apply the lambda expression to the input as a fold, with the lines being the
list and the columns being the arguments tuple. Integer, floating or boolean
arithmetics are available, depending on the type chosen (defaults to integer).

Arguments:
    LAMBDA      Valid haskell-y lambda expression (`args -> expression`).
                The expression should return a tuple of values
                (parens can be ignored for a singleton).
    INIT        Start value for the fold. Defaults:
                    "0" for integers
                    "0.0" for doubles
                    "true" for booleans

Options:
    --type={int, float, bool}   Sets the input type for the lambda expression.
    --separator                 Sets the separator (default is whitespace).
    --exit                      Use the exit code to output the result
                                (conflicts with multi-column output).
```
