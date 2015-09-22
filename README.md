# haskell scheme

Incomplete and non-standard compliant Scheme intepreter written in Haskell and based on:
https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

This deviates from the above tutorial in a few ways, including but not limited to:
- Uses the Haskeline library for a better REPL exprience.
- Only Float and Integer number types are implemented, and there is no automatic type conversion.
- Uses the Except Haskell library rather than Error.

## Building and running

Build with the Glasgow Haskell Compiler using

`ghc --make -o bin/hscheme -isrc src/main.hs`

Running the binary (`bin/hscheme`) goes in to the REPL from where you can load and run other prepared scripts.

## Laguage documentation

Key features are:
- Lexical scoping
- Functions are first-class values
- Supports functional closures
- Dynamic strong typing

Like other LISPs, Scheme is built around lists.  Code is expressed in lists, and lists are used as a data type.

Lists are ued for function calls:

```Scheme
(<fn_expr> <expr_1> <expr_2> .. <expr_n>)
```
The list is evaluated by evaluating each term in the list.  The first term should evaluate to a function, whcih is called with the results of subsequent expressions applied as arguments.

To create a list which is _not_ evaluted (e.g. to store some data) use the quoted list form:

```Scheme
'(1 2 3 4)
```

### Special forms

To be completed...

### Basic literals

- Number literals are mainly as expected: `123` is an Integer. `#xFF`, `#o77`, and `#b10101` are integers given in hex, oct, and binary respectively. `0.4` and `1.0` are Floats.
- `"Hello world"` is a string literal. Typial escape sequencs apply: `\n`, `\t`, `\\`, `\"`
- A Character literal is given by the character preceeded by `#\`, e.g. `#\&` for `&`.
- `#t` is booean true and `#f` is boolean false.
