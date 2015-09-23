# haskell scheme

Incomplete and not standard-compliant Scheme interpreter written in Haskell and based on:
https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

This deviates from the above tutorial in a few ways, including but not limited to:
- Uses the Haskeline library for a better REPL experience.
- Only Float and Integer number types are implemented, and there is no automatic type conversion.
- Uses the Except Haskell library rather than Error.

## Building and running

Build with the Glasgow Haskell Compiler using

`ghc --make -o bin/hscheme -isrc src/main.hs`

Running the binary (`bin/hscheme`) goes in to the REPL from where you can load and run other prepared scripts.

## Language documentation

Key features are:
- Lexical scoping (for the most part, anyway) with _function_ scope.
- Functions are first-class values.
- Supports functional closures.
- Dynamic strong typing.

As a LISP, Scheme is built around lists.  Code is expressed in lists, and lists are used as a data type.

Lists are used for function calls:

```Scheme
(<fn_expr> <expr_1> <expr_2> .. <expr_n>)
```

For example, the `+` function adds two numbers together and the `*` function multiplies two numbers.

```Scheme
(+ 2 5)
;; => 7
(+ 3 (* 2 5))
;; => 13
```

As we said before, lists are used as code, and for data.  The `cdr` function gets the first item from a list.

```Scheme
(cdr (1 2 3 4))
;; => "Does not evaluate to a function: 1"
```

This fails because before calling `cdr` the interpreter tries to evaluate `(1 2 3 4)` as an expression by calling `1` as a function with arguments `2`, `3`, `4`. To create a list which is _not_ evaluated (e.g. to store some data) use the quoted list form `'(1 2 3 4)`:

```Scheme
(cdr '(1 2 3 4))
;; => 1
```

### Special forms

Variables are bound with `define` (which also returns the bound value).

```Scheme
(define foo 7)
;; => 7
(+ foo 1)
;; => 8
```

`define` is also used to create functions.

```Scheme
(define (fn x y) (+ x (* y 2)))
;; => ((lambda "x" "y") ...)
(fn 3 4)
;; => 11
```

The general form of a function definition is:

```Scheme
(define (<function_name> <arg_1> ... <arg_n>) <expr_1> <expr_2> .. <expr_n>)
```

When the function is called, the expressions `<expr_1>` up to `<expr_n>` are evaluated in order and the result of the final expression is the return value. For example `fn` above could be more verbosely defined as:

```Scheme
(define (fn x y)
  (define z (* y 2))
  (+ x z))
```
In this example the scope of `z` is the function `fn`. In general `define` creates a variable in the local scope. If a variable in the parent scope has the same name then `define` creates a new variable in the local scope and does not alter the value in the parent scope.

To alter a value in a parent scope, use `set!`. Compare the functions `bar` and `baz`:

```Scheme
(define foo "Hello")
(define (bar)
  (define foo 42))
(define (baz)
  (set! foo 42))
(bar)
;; foo is still "Hello"
(baz)
;; now foo is 42
```

Note that, like `define`, `set!` also returns the set value.

The `lambda` form creates anonymous functions.

```Scheme
(lambda (<arg_1> ... <arg_n>) <expr_1> <expr_2> ... <expr_n>)
```

These can be used to pass to other functions, or as function return values. For example

```Scheme
(define (incrementer N)
  (lambda (x) (+ N x)))
(define inc7 (incrementer 7))
(inc7 2)
;; => 9
(inc7 11)
;; => 18
```

The `if` form is:

```Scheme
(if <pred_expr> <true_expr> <false_expr>)
```

`if` evaluates `<pred_expr>`.  If it evaluates to `#t` then `<true_expr>` is evaluated and returned; if it evaluates to `#f` then `<false_expr>` is evaluated and returned.  If `<pred_expr>` evaluates to any non-boolean type it's an error.

The final special form is `load`.

```Scheme
(load "lib/stdlib.scm")
```

It reads and executes the given file.

#### What's special about special forms?

First, consider functions. Functions, whether built-in or user defined with `define` or `lambda`, are all treated and evaluated in exactly the same way. They can be bound to variables, passed to and from other functions, stored in lists, etc... They are called using the form

```Scheme
(<fn_expr> <expr_1> <expr_2> ... <expr_n>)
```

where `<fn_expr>` should evaluate to a function, and the expressions `<expr_1>` to `<expr_n>` are _first_ evaluated and _then_ passed as arguments to (the evaluation of) `<fn_expr>`.

Here's an example:

```Scheme
(define (do-something-with-2-and-5 func)
  (func 2 5))
(do-something-with-2-and-5 +)
;; => 7
(do-something-with-2-and-5 *)
;; => 10
(do-something-with-2-and-5 -)
;; => -3
```

See how `+`, `*`, and `-` can be passed to `do-something-with-2-and-5` and are bound to `func` within the body of `do-something-with-2-and-5`

However the special forms described above are language constructs and cannot be bound to variable names or passed around like in this example. Moreover they have special rules about how their argument are evaluated.  While every argument passed to function is always evaluated first, this is not the case for special forms.

In the form `(if <pred_expr> <true_expr> <false_expr>)` only one of `<true_expr>` or `<false_expr>` will be evaluated.

For `(define <var_name> <expr>)`, `<var_name>` cannot be evaluated before being passed to `define` - it is not yet defined!

This may all seem obvious, or maybe not. Because of the list syntax, special forms and function calls look very similar. In other languages the syntax makes it very clear that function calls are different from variable declarations, function definitions, or control-flow. But in a lisp not so much.

### Functions

Functions which are not built-in, but are defined in the library `lib/stdlib.scm` are identified.

#### Numerical functions

```Scheme
(= 3 3)
;; => #t
```

```Scheme
(> 2 3)
;; => #f
```

There are similar comparison functions `<`, `>=`, and `<=`.

```Scheme
(+ 2 3)
;; => 5
```

```Scheme
(sum 1 2 3 4)
;; => 10
```

`sum` is library function. Unlike `+` it takes arbitrarily many arguments.

```Scheme
(* 2 3)
;; => 6
```

```Scheme
(product 1 2 3 4)
;; => 24
```

`product` is a library function. Unlike `*` it takes arbitrarily many arguments.

```Scheme
(- 2 5)
;; => -3
```

```Scheme
(/ 2.0 3.0)
;; => 0.6666667
```

`/` only operates on floats.

```Scheme
(mod 10 4)
;; => 2
```

```Scheme
(quotent 11 3)
;; => 3
```

```Scheme
(remainder 11 3)
;; => 2
```

### Basic literals

- Number literals are mainly as expected: `123` is an Integer. `#xFF`, `#o77`, and `#b10101` are integers given in hex, oct, and binary respectively. `0.4` and `1.0` are Floats.
- `"Hello world"` is a string literal. Typical escape sequences apply: `\n`, `\t`, `\\`, `\"`
- A Character literal is given by the character proceeded by `#\`, e.g. `#\&` for `&`.
- `#t` is boolean true and `#f` is boolean false.
