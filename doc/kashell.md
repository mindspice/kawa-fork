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

## Identifiers

An identifier is used to name things in a program.
The allowed characters in an identifier is bigger that
in most programming languages and roughly follows Scheme.

There are no reserved identifiers, though there are
syntactic keywords predefined in the default scope.

The recommend style for multi-part names is to use hyphens between the parts:
`array-rank`.

There will be some syntax to include otherwise-disallowed characters
in an identifier.  This has not been decided or implemented
but I'm leaning towareds backslash followed by a string template.
For example `\{1.5}` would be an identifier
(with the 3 characters `"1"`, `"."`, and `"5"`) rather than a number.

Compound identifiers have two parts, separated by a colon (and no whitespace).
The first part is a namespace (an identifier), and the second part is a name
within that namespace.

## Indentation

Indentation is significant:

    foo 1 2 3
       bar 4 5
           3 + 3
       baz 10 11

is equivalent to:

    foo 1 2 3 (bar 4 5 (3 + 3)) (baz 10 11)

## Comments

A hash-sign `# ` followed by at least one space
comments out the rest of the line.

A hash-sign followed by an exclamation point `#!` is also a comment.

Syntax for nestable comments hasn't been decided yet.
Candidates include `#[comment#]` or `#[comment]#` or plain `#[comment]`.

## Numbers

KaShell implements the Kawa Scheme "numeric tower",
including exact integers and rationals, floating-point reals,
and complex numbers.  (Syntax of literals may change slightly from Kawa Scheme.)
Quaternions are also supported.

We may add exact decimal numbers, possibly with repeating fractional part.
These are mathematically equivalent to exact rationals, but are
typically easier to read and write.

Quantities are a product of a real number and a unit.
For example: `3cm + 2in` evaluates to `8.08cm` (the second
quantity is converted to the unit of the first).
A designed extension will be able to do unit-checking at compile-time
based on [this design](https://www.gnu.org/software/kawa/Ideas-and-tasks.html#Types-for-units).

## Arithmetic

The usual infix and operator precedence rules apply.
For example, the following evaluates as expected to 22:

    10 + 2 * 6

Note that spaces are (generally) required.

However, note that infix operators like `+` are *not* reserved
syntax.  They are predefined syntatic keywords (with associated
precedence information), and there will be a way to
add or replace operators.

## Variables and definitions

All variables must be defined before using them,
to catch typos.  However, the syntax to define a variable
is quite compact - you just need to add `^` after the variable:

    twenty^ = 10 + 5 + 5

You can do simple pattern matching:

    [x^ y^] = [3 4]

(In the future, the `=` operator may be extended to bi-directonal
[unification](https://en.wikipedia.org/wiki/Unification_(computer_science)).)

Variables defined using `=` may not be re-assigned
(though this is not currently enforced).
For a variable that can be modfied, use `:=`.

     counter^ := 0
     counter := counter + 1

## Optional type specifiers [not working yet]

You can add an optional type specifier after the `^` in a definition:

    pi^float = 3.14
    
## Conditional operator

The `?>` is syntatically an infix operator but it integrates
with the phrase-parsing to provide a ternary if-the-else operator:

    (3 > 4 ?> "it is true"; "if is false")

or:

    x > 0 ?>
       display x
       display " is positive"
       newline
    x < 0 ?>
       display x
       display " is negative"
       newline
    display x
    display " is zero"
    newline

[This is a hack that needs further thought and specification.]

## Vectors and arrays

Use square brackets to construct (immutable) vectors:

    [3 (2 + 2) 5]

A vector is a function from an integer to an element.

    [3 4 5] 2

evalutes to 5.

You can use a vector index to select elements:

    [10 11 12] [2 1]

evaluates to `[12 11]`.

There is supports for [multi-dimensional arrays](https://www.gnu.org/software/kawa/Arrays.html) but specifics (such as syntax and operator names) have not been decided.

## Strings

A string is an immutable sequence (vector) of characters (Unicode code points).
You can index it (like a vector) to get a character.

(Not yet implemented: A character is also a string of length 1,
so `"X" 0` yields the same `"X"`.
This removed the need for distict character literal syntax.)

There are two syntaxes for string literals:

### Double quoted string literals

The traditional syntax with double quotes: `"Hello"`.
Most C-style escapes are supported: `"Hello!\n"`.
May not extend over multiple lines, to help catch errors.
(They may be a way to continue line using some escape sequence,
details not yet decided.)

### Brace string literals

A string may also be written using curly braces: `{Hello}`.
Braces nest: `{string with {braces}.}`.
These maybe multi-line and there are various escape sequences,
[like Kawa template strings](https://www.gnu.org/software/kawa/String-literals.html#String-templates), though backslash is used as the
escape character rather than `&`.

`{L\aelig;rdals\oslash;yri}` evaluates to `"Lærdalsøyri"`.

`{Adding 3 and 4 yields \(3 + 4).}` evaluates to `"Adding 3 and 4 yields 7."`.

You can also add formatting specifiers.

## Object constructor syntax [not implemented yet]

An identifier allowed by a brace-literal is a conveniece syntax
for constructing complex objects:

`URI{http://example.com/}`

The constructor can also contain expressions in parentheses
(which is evaluated), or bracket literals that contain multiple expressions.
There may be no (unescaped) spaces between the parts of an object literal.

The concept and implementation are similar to Kawa's and SRFI-108's
[Named quasi-literal constructors](https://srfi.schemers.org/srfi-108/srfi-108.html).  However, the syntax is different in using backslash
as the escape character, and not requiring an initial backslash.

## Rich text objects

A rich text is an enhanced string,
with embedded objects and formatting.
It is syntatic sugar for a kind of object constructor.

    '{Some text *strong* and \em{emphasized}.}

A subset of Markdown syntax is recognized, including
`*`, `_` and blank lines for paragrph separator.
Beyond that, general object literal syntax is used.

The above is equivalent to:

    text{Some text \text:b{strong} and \text:em{emphasized}.}

Evaluating either expression yields a text object,
which is a tree-structure that generalies strings.
The text object can then be converted to various formats
depending on context.  For example:

    write-pdf filename: "hello.pdf" '{Hello!}

or

    as-html '{Some text *strong* and \em{emphasized}.}

which yields `"<p>Some text <b>strong</b> and <em>emphasized</em>.</p>"`

It is intended that text literals be used to document programs.
Tools that pretty-print programs or extract API information
should format these documentation strings.

The DomTerm terminal emulator allows "printing" HTML as rich text.
When printing a text value in a DomTerm REPL it should
implicitly call `as-html` and show that.

## Running programs [not implemented yet]

The `run` macro quasi-quotes its arguments, and then executes
the resulting string list as a process invocation,
as if using the Kawa `run-process` function.

    run date --utc

The result is a _process_ object. A _process_ can be coerced to
a string (or more generally a _blob_), which is the result of
standard output from the process.
A _process_ "written" to the REPL coerces it to a string.

The `run` macro can be left out if the following word
has the form of a fully-qualified filename (i.e. starting with `/`).
Also, if the following word is not in the lexical scope,
but if there is (at compile-time) an executable file by that name in
the `PATH` then `run` is also implied.
