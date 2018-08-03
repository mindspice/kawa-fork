# KaShell - ideas for an interaction-friendly language

KaShell is an evolving design for a programming language
with a compact syntax similar to shell and friendly for interactive use,
and with semantics similar to optionally-typed languages like Scheme.

The prototype is a variant of [Kawa](https://www.gnu.org/software/kawa).
To try it, [install Kawa](https://www.gnu.org/software/kawa/Installation.html),
and then start up kawa with the `--kashell` option.

KaShell was previously known as Q2.
There is some old documentation/ideas
[here](https://www.gnu.org/software/kawa/q2,here).

## Basic syntax

Whitespace and indentation are signficant.

Commands are similar to shell syntax:
A simple command has the form of a command followed
by the argument expressions, separated by spaces:

    expt 2 3

This calls the `expt` function with the given arguments.
Parentheses are not needed, except for grouping:

    expt 2 (sqrt 9)

Such a command is an example of a _phrase_.
The function name and each argument is a _word_.

_word_ ::= _identifier_ | _literal_ | ....  
&nbsp;&nbsp;| `(` _phrase_ `)`  
_phrase_ ::= _word_ ...

Phrases can be separated by newline or semicolons.

A procedure call is a _phrase_ whose first word
evaluates to a procedure value.
(It can be a single-word phrase, if there are no arguments.)

A syntactic form is  _phrase_ whose first word is
a predefined syntactic keyword or an in-scope macro.

There are no reserved identifiers, though there are
syntactic keywords predefined in the default scope.

## Arithmetic

The usual infix and operator precedence rules apply.
For example, the following evaluates as expected to 22:

    10 + 2 * 6

Note that spaces are (generally) required.

However, note that infix operators like `+` are *not* reserved
syntax.  They are predefined syntatic keyword (with associated
precedence information), and there will be a way to
add or replace operators.

## Variables and definitions

All variables must be defined before using them,
so catch typos.  Howevr, the syntax to define a variable
is quite compact - you just need to add `^` after the variable:

    twenty^ = 10 + 5 + 5

You can do simple pattern matching:

    [x^ y^] = [3 4]

(In the future, the `=` operator may be extended to bi-directonal
[unification](https://en.wikipedia.org/wiki/Unification_(computer_science)).)

Variable defined using `=` may not be re-assigned
(though this is not currently enforced).
For a variable that can be modfied, use `:=`.

     counter^ := 0
     counter := counter + 1

## Optional type specifiers [not working yet[

You can add an optional type specifier after the `^` in a definition:

    pi^float = 3.14
    
## Conditional operator

The `?>` is syntatically an infix operator but it integrates
with the phrase-parsing to provide a ternary if-the-else operator:

    (3 > 4 ?> "it is true"; "if is false")

[This is a hack that needs further thught and specification.]

## Running programs [not implemented yet]

The `run` macro quasi-quotes its arguments, and then executes
the resulting string list as a process invocation,
as if using the Kawa `run-process` function.

    run date --utc

The result is a _process_ object. A _process_ can be coerced to
a string (or more generally a _blob_), which is the result of
standard output from the process.
A _process_ "written" to the REPL coerces it to a string.

The `run` macro can be left out of the following word
has the form of a fully-qualified filename (i.e. starting with `/`).
Also, if the following word is not in the lexical scope,
but if there is (at compile-time) an executable file by that name in
the `PATH` then `run` is also implied.
