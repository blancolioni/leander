# Structured Types in the Lambda Calculus

This document describes how Leander compiles algebraic data types and
pattern matching into untyped lambda calculus, which is then executed on
the SKI combinator machine.

## Constructor encoding

Leander uses a **Scott encoding**: each constructor becomes a projection
function that selects one branch from N arguments, where N is the total
number of constructors in the data type.

### Construction rules

Given a data type with N constructors, where the i-th constructor takes
M_i arguments:

    Con_i x1 .. xM  ==>  \$1.\$2...\$N.\x1...\xM. $i x1 ... xM

Each constructor is a function of (N + M_i) arguments. The first N
arguments are continuations (one per constructor). The remaining M_i
arguments are the constructor's fields. The body applies the i-th
continuation to the field values.

### Examples

**Bool** (2 constructors, 0 arguments each):

    True   =  \$a.\$b. $a
    False  =  \$a.\$b. $b

**List** (2 constructors: [] with 0 args, (:) with 2 args):

    []     =  \$a.\$b. $a
    (:)    =  \$a.\$b.\x.\y. $b x y

**Unit** (1 constructor, 0 arguments):

    ()     =  \$a. $a

**(,)** (1 constructor, 2 arguments):

    (,)    =  \$a.\x.\y. $a x y

The encoding is built in `leander-data_types-builder.adb`, procedure
`Build`. Variable names are generated as `$a`, `$b`, ... for the
constructor-selection parameters, and continuation from an offset for
the field parameters.

## Pattern matching compilation

Pattern matching on constructors is compiled by the alts compiler
(`leander-core-alts-compiler.adb`) into **constructor application**.
Because a constructed value *is* its projection function, matching is
simply applying the value to one continuation per constructor branch.

### Constructor patterns

For a case expression over a type with N constructors:

    \$v. $v branch_1 branch_2 ... branch_N

Each `branch_i` is a lambda abstracting over the i-th constructor's
field variables, with the body being the corresponding case arm:

    branch_i = \x1.\x2...\xM. expr_i

When a constructor is missing from the case expression and a default
clause exists, the default expression fills that slot instead.

If there is no pattern and no default, `#error` is used.

### Example: length

    length []     = 0
    length (_:xs) = 1 + length xs

Compiles to:

    length = Y (\length. \$v. $v 0 (\_.\xs. + 1 (length xs)))

The value `$v` is applied to two continuations:
- First (for `[]`): returns 0
- Second (for `:`): takes head (ignored) and tail `xs`, returns `1 + length xs`

The `Y` combinator provides recursion by binding `length` in its own
definition.

### Example: not

    not True  = False
    not False = True

Compiles to:

    not = \$v. $v False True

The Bool value selects between its two continuations directly.

### Integer patterns

Integer matching uses a different strategy (compare mode) since integers
are primitive values, not Scott-encoded constructors. The compiler emits
a chain of equality tests using the `#eq` primitive:

    case n of
      1 -> e1
      2 -> e2
      _ -> e3

Compiles to:

    \$v. (#eq $v 1 e1) ((#eq $v 2 e2) (e3))

### Default/variable patterns

A variable or wildcard pattern becomes a lambda that captures the
scrutinee:

    case x of
      []  -> e1
      xs  -> e2

The default `xs -> e2` compiles as `\xs. e2`, placed in each
constructor slot that lacks an explicit pattern.

## Built-in data types

The prelude environment (`leander-environment-prelude.adb`) defines
the primitive structured types before any Haskell source is loaded:

| Type    | Constructors         | Notes                      |
|---------|----------------------|----------------------------|
| `()`    | `()`                 | 1 constructor, 0 fields    |
| `Bool`  | `True`, `False`      | 2 constructors, 0 fields   |
| `[a]`   | `[]`, `:`            | 2 constructors; `:` has 2  |
| `(a,b)` | `(,)`                | 1 constructor, 2 fields    |

Additional data types defined in Haskell source (via `data`
declarations) are processed by `Data_Types.Builder` through the same
encoding.

## From lambda calculus to SKI machine

The `Compile` procedure in `leander-calculus.adb` translates the lambda
calculus tree to SKI machine instructions:

- **Lambda nodes** push a `Lambda` primitive, then the bound variable
  object and the compiled body, followed by two `Apply` operations to
  build a closure.
- **Apply nodes** compile left and right subtrees, then execute `Apply`.
- **Reference nodes** are resolved by first checking the lambda binding
  stack (local variables), then the skit environment (global bindings),
  and finally the Leander calculus environment (which triggers recursive
  compilation on first access).
- **Number nodes** push integer objects directly.

At runtime, constructed values are chains of `Application_Object` cells
in the SKI machine's heap. Matching a constructor value means reducing
the application of a projection function to its continuation arguments,
which the SKI machine handles through normal combinator reduction.
