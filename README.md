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
- Lexical scoping (for the most part, anyway) with _function_ scope.
- Functions are first-class values.
- Supports functional closures.
- Dynamic strong typing.

Like other LISPs, Scheme is built around lists.  Code is expressed in lists, and lists are used as a data type.

Lists are used for function calls:

```Scheme
(<fn_expr> <expr_1> <expr_2> .. <expr_n>)
```
The list is evaluated by evaluating each term in the list.  The first term should evaluate to a function, whcih is called with the results of subsequent expressions applied as arguments.

To create a list which is _not_ evaluted (e.g. to store some data) use the quoted list form:

```Scheme
'(1 2 3 4)
```

### Special forms

```Scheme
(load "lib/stdlib.scm")
```
Loads and executes the given file.

```Scheme
(if <pred_expr> <true_expr> <false_expr>)
```

`if` evalutes `<pred_expr>`.  If it evaluates to `#t` then `<true_expr>` is evaluated and returned; if it evaluates to `#f` then `<false_expr>` is evaluated and returned.  If `<pred_expr>` evalutes to any non-boolean type it's an error.

```Scheme
(define foo "Hello")
```

Creates the variable `foo` in the current scope. If `foo` is already defined in the current scope it overwrites the value. A `foo` defined the local scope hides any `foo` defined in a parent scope leaving it unaffected. Additially, `define` returns the value which has been set.

The `define` form is allowed anywhere that an expression is allowed.  This breaks proper lexical scoping, as a `define` form can be placed in an `if` block, and so the resolution of a variable name may not be determined until run-time.

```Scheme
(set! foo 42)
```

Sets the variable `foo` to 42. `foo` should already be defined, either in the local scope or some parent scope.  `set!` will not create a new bindng - it's an error if `foo` is not already bound.

```Scheme
(lambda (<arg_1> ... <arg_n>) <expr_1> <expr_2> .. <expr_n>)
```

`lambda` creates an anonymous function. `lambda` takes a list of argument names, followed by a sequence of expressions.  When the function is called the the expressions are evaluated in order with the result of the last expression being the return value.  A funnction can be bound to a variable name like any other value:

```Scheme
(define foo (lambda (x) (+ x 1)) )
(foo 5)
;; => 6
```

`define` can create a new function without using the `lambda` form with the syntax:

```Scheme
(define (<function_name> <arg_1> ... <arg_n>) <expr_1> <expr_2> .. <expr_n>)
```

This is an eqivilent definition of `foo`:

```Scheme
(define (foo x) (+ x 1))
(foo 5)
;; => 6
```

.... to be completed ...

### Basic literals

- Number literals are mainly as expected: `123` is an Integer. `#xFF`, `#o77`, and `#b10101` are integers given in hex, oct, and binary respectively. `0.4` and `1.0` are Floats.
- `"Hello world"` is a string literal. Typial escape sequencs apply: `\n`, `\t`, `\\`, `\"`
- A Character literal is given by the character preceeded by `#\`, e.g. `#\&` for `&`.
- `#t` is booean true and `#f` is boolean false.
