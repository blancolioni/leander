# ADR 0003: Prioritise Tier-1 Haskell 2010 Features

- **Status:** Proposed
- **Date:** 2026-07-11
- **Deciders:** Fraser Wilson

## Context

A source-level audit of Haskell 2010 support is recorded in
[haskell-2010-status.md](../haskell-2010-status.md). The core is strong —
Hindley-Milner inference, type classes with superclasses and dictionary
passing, `data`/polymorphic types, `case`, multi-equation definitions, `do`
notation, arithmetic sequences. The gaps are uneven: some are small omissions,
a few are architectural, and several *look* supported but fail downstream.

The audit surfaces three kinds of gap, and they should not be worked in reading
order:

- **Cheap and high-frequency** — absent everyday features whose implementation
  is contained and low-risk (missing Prelude functions, guards, `deriving
  Show`).
- **Foundational** — limitations that block idiomatic code and gate later work
  (nested pattern compilation, `type`/`newtype`, expression annotations).
- **Correctness cliffs** — features that appear to work but are wrong:
  `Float`/`Double` literals **truncate to integers**, `Integer` is not a
  bignum, numeric literals are monomorphic `Int` with no `fromInteger` and no
  defaulting. These are large and interlocked, and they belong with the object
  representation decision in [Skit ADR 0001](../../skit/docs/adr/0001-object-representation.md).

This ADR decides **what to do first**, not everything. It commits only to the
cheap, high-frequency tier, because that tier unblocks the largest fraction of
ordinary Haskell per unit of effort and carries no architectural risk, and it
explicitly defers the numeric-tower cliff to its own sequenced work.

## Decision drivers

- **Leverage per unit effort.** A missing `foldl`/`zip`/`elem` or a missing
  guard blocks real programs constantly and costs little to add. The numeric
  tower blocks a different class of program but costs a project.
- **Risk isolation.** Tier-1 items touch the parser surface and the Prelude,
  not the core IR or the type system's hard parts. They can land incrementally
  behind the existing integration suite.
- **Sequencing discipline.** The numeric cliff is entangled with ADR 0001's
  representation choice; starting it now would either be redone after 0001 or
  force 0001 prematurely. Keep it out of this ADR.
- **Observability.** `deriving Show` is a force multiplier for every subsequent
  feature: it makes data values printable at the REPL and in tests, which is how
  the rest of this work gets verified.

## Options considered

### A. Work the audit top-to-bottom

Implement gaps in the order the status doc lists them (lexical → expressions →
… → prelude). Rejected: mixes trivial and architectural work arbitrarily, front-
loads low-value lexical polish, and defers the highest-ROI Prelude/guards work.

### B. Attack the numeric tower first

It is the single highest-value *feature*. Rejected as *first*: it is a project,
it is interlocked with ADR 0001, and it would stall visible progress for weeks
while cheaper wins wait.

### C. Tier-1 first: Prelude breadth, guards, `deriving Show` (proposed)

Land the contained, high-frequency, low-risk features, then reassess. Unblocks
the most programs soonest, keeps the suite green throughout, and produces the
`Show` observability the rest of the roadmap needs.

## Decision

Adopt Option **C**. Implement, in this order:

### 1. Prelude breadth

Add the missing everyday definitions to
[Prelude.hs](../../share/leander/modules/Prelude.hs): `foldl`, `zip`/`zipWith`,
`drop`, `takeWhile`/`dropWhile`, `span`/`break`, `elem`/`notElem`, `lookup`,
`(!!)`, `replicate`, `concatMap`, `and`/`or`/`any`/`all`,
`maximum`/`minimum`/`product`, `last`/`init`, and the `Either` type with
`either`. Pure Haskell, no compiler change. Each addition is covered by a
`Test_Eval` case.

- Risk: near zero — new definitions in the standard library, guarded by the
  existing type checker and evaluator.
- Watch: functions that need currently-absent machinery (e.g. anything wanting
  `Show` for a compound type, or list-monad `do`) are out of scope here and move
  to their own step.

### 2. Guards

Add `| guard` grammar to function equations
([leander-parser-bindings.adb](../../src/leander-parser-bindings.adb)) and case
alternatives (`Parse_Case_Alt` in
[leander-parser-expressions.adb](../../src/leander-parser-expressions.adb)), and
desugar guarded RHSs to nested `if`/`else` (with `otherwise = True` already in
the Prelude). No core-IR change: guards lower to expressions the pipeline
already handles. Boolean guards only — pattern guards and `MultiWayIf` are not
Haskell 2010 in this position and are out of scope.

- Open detail: the fall-through when all guards fail must continue to the next
  equation/alternative, not error. This requires the equation compiler to treat
  a fully-failed guard set like a non-match. Confirm the alts compiler
  (`leander-core-alts-compiler.adb`) can thread a "guard failed → try next"
  continuation, or lower guards before alternative selection so a failed guard
  reduces to the existing default path.

### 3. `deriving Show` (then `deriving Ord`)

Extend the deriving dispatch
([leander-syntax-deriving.adb](../../src/leander-syntax-deriving.adb), currently
`Eq`-only) with a `Show` generator: per-constructor `show` producing the
constructor name and space-separated `showsPrec`-style arguments, adding a
`Show` constraint per type variable exactly as the `Eq` generator adds `Eq`.
Follow with `Ord` (lexicographic by constructor index then fields), reusing the
same structural-traversal shape.

- Prerequisite noted: a useful `Show` wants `showsPrec` and precedence-aware
  parenthesisation; the current `Show` class has only `show`
  ([Prelude.hs](../../share/leander/modules/Prelude.hs)). Decide whether to add
  `showsPrec` to the class now (cleaner, matches H2010) or generate flat `show`
  first and refine later.

## Out of scope (explicitly deferred)

- **Numeric tower / real `Float`/`Double` / literal overloading / defaulting /
  ambiguity detection.** The correctness cliff. Sequenced with
  [Skit ADR 0001](../../skit/docs/adr/0001-object-representation.md); to be its
  own ADR once representation is settled.
- **Nested pattern compilation, `type`/`newtype`, `e :: T` annotations.** The
  foundational tier. Candidates for the next ADR after Tier-1 lands.
- Module imports/exports, input IO, a real layout algorithm, and the small
  polish items (block comments, numeric escapes, left sections, multi-arg
  lambdas).

## Staged migration

Each step keeps the self-test and integration suites green and adds tests.

1. Prelude breadth — batch by group (list functions, then `Either`), each with
   `Test_Eval` coverage.
2. Guards — parser + desugar; add function-guard and case-guard integration
   tests, including the all-guards-fail fall-through case.
3. `deriving Show` — generator + tests deriving `Show` on nullary and
   parameterised types; then `deriving Ord` likewise.
4. Update [haskell-2010-status.md](../haskell-2010-status.md) statuses as each
   lands.

## Open questions

- Guards: lower before or after alternative selection? The fall-through
  semantics decide this; prototype the simpler lowering first.
- `Show`: add `showsPrec` to the class now, or ship flat `show` and refine?
- Does adding `Either` (and later compound `Show`) expose the absence of generic
  `Show`/`Ord` instances for lists/tuples/`Maybe`, and should those hand-written
  instances land alongside the deriving work?

## Consequences

To be recorded as steps land. Expected:

- A large fraction of ordinary Haskell (list-heavy code, guarded definitions,
  printable/derivable data types) becomes expressible, with no change to the
  core IR or type system.
- `deriving Show` gives every later feature a way to be observed and tested.
- The status doc becomes a living checklist; the remaining gaps are cleanly
  partitioned into the foundational tier and the numeric cliff, each with a
  designated home.
