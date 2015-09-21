# haskell scheme

Incomplete and non-standard compliant Scheme intepreter written in Haskell and based on:
https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

This deviates from the above tutorial in a few ways, inclding but not limited to:
- Uses the Haskeline library for a better REPL exprience.
- Only Float and Integer number types are implemented, and there is no automatic type conversion.
- Uses the Except Haskell library rather than Error.

## Building and running

Build with the Glasgow Haskell Compiler using

`ghc --make -o bin/hscheme -isrc src/main.hs`

Running the binary (`bin/hscheme`) goes in to the REPL from where you can load and run other prepared scripts.

## Laguage documentation

### Expressions

Basic literals:
- Number literals are mainly as expected: `123` is an Integer. `#xFF`, `#o77`, and `#b10101` are integers given in hex, oct, and binary respectively. `0.4` and `1.0` are Floats.
- `"Hello world"` is a string literal. Typial escape sequencs apply: `\n`, `\t`, `\\`, `\"`
- A Character literal is given by the character preceeded by `#\`, e.g. `#\&` for `&`.
- `#t` is booean true and `#f` is boolean false.
