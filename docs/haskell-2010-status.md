# Haskell 2010 Feature Status

Status of Haskell 2010 language support in Leander. Legend:

- ✅ **Full** — works end to end
- 🟡 **Partial** — parses/works with real limits (noted)
- ❌ **None** — absent, or parses but fails downstream

Status reflects *end-to-end* behaviour: a form that the parser accepts but the
core rejects is marked by its effective (worst) status.

Last checked against `f17c29c` (2026-10-09), by running each form through
`leander -e` / `leander --main`.

---

## Lexical

| Feature | Status | Note |
|---|---|---|
| Integer literals | ✅ | Decimal, hex (`0x1F`) and octal (`0o17`). |
| Floating literals | 🟡 | Lexed per the Report (`1.5`, `2e3`, `2.5E-1`; `[1..5]` is a range). Typed `Double`, but the value **rounds to an integer** (`3.7` gives `4`), and nothing has a `Double` instance, so `3.7 + 1` fails to type-check (see Numeric). |
| Char literals | ✅ | Latin-1 only. On Windows, `leander -e "'a'"` loses the quotes before leander sees them (GNAT's runtime strips a leading quote pair from an argument); the same literal in a source file is fine. |
| String literals | ✅ | Latin-1 only. |
| Escape sequences | ✅ | All of Report 2.6: `\a \b \f \n \r \t \v \\ \" \'`, ASCII control names (`\SOH`, longest match), `\^A`, `\65`/`\x41`/`\o101`, and in strings `\&` and gaps, including gaps across lines. A code above 255 is rejected. |
| Negative literals | ❌ | `-` is a binary operator only; Prelude spells it `0 - x`. |
| Layout / offside rule | 🟡 | Ad-hoc per-construct indent checks; no real layout algorithm, no virtual braces, no parse-error close. |
| Explicit braces / semicolons | ✅ | |
| Line comments `--` | ✅ | Per the Report, `-->` and the like are operators, not comments. |
| Block comments `{- -}` | ✅ | Nested. Pragmas (`{-# … #-}`) are skipped as comments. |
| User-defined operators | ✅ | |
| Fixity (`infix`/`infixl`/`infixr`) | ✅ | Prec 0–9, shunting-yard. |

## Expressions

| Feature | Status | Note |
|---|---|---|
| Lambda | ✅ | Any number of atomic patterns: `\x y -> e`, `\(a,b) -> e`, `\(Just x) _ -> e`. Patterns nest only one level deep, as everywhere (#109). |
| Application | ✅ | |
| Operator sections | 🟡 | Right sections `(op e)`, backtick sections `` (`div` 2) `` and bare `(op)` work. **Left sections `(e op)` fail to parse.** |
| Unary negation | ❌ | `(-1)` mis-parses as a right section, so it is a function. Use `negate`. |
| `let` / `where` | 🟡 | Both work on bindings, including recursive local functions. `where` does **not** work on case alternatives (syntax error). |
| `if`/`then`/`else` | ✅ | |
| `case` | 🟡 | Works, guards included. No `where` on alternatives. |
| `do` notation | ✅ | Monad-generic desugar, including `let`. **No `MonadFail`**: a refutable bind that doesn't match is a hard error. |
| Tuples | ✅ | Types, constructors and patterns up to size 15, including `(,,)` written alone. Patterns nest only one level, as for any constructor (`f (Just (a,b))` is rejected). |
| List literals | ✅ | |
| List comprehensions | ❌ | No generator/guard syntax. |
| Arithmetic sequences `[a..]`,`[a..b]`,`[a,b..c]` | ✅ | Via `enumFrom*`, on `Int`, `Char`, and any `Enum` instance that relies on the class defaults. `['a' ..]` never ends: the default `enumFrom` counts up through `Int` past code 255. |
## Patterns

| Feature | Status | Note |
|---|---|---|
| Variable / wildcard `_` | ✅ | |
| Constructor patterns | ✅ | |
| Cons / list patterns `(x:xs)` | ✅ | |
| Literal patterns | 🟡 | **`Int` only.** Char and float literal patterns fail with "undefined constructor: Char/Double"; string literal patterns fail an assertion in `leander-core-patterns.ads`. |
| Nested patterns | 🟡 | **One level only.** The core requires constructor arguments to be variables (`leander-core-patterns.ads`), so `Just (x:_)`, `[a,b]` and `(Just x, y)` all fail with "problem in binding". |
| Multiple equations | ✅ | |
| As-patterns `@` | ❌ | `a@(Just x)` raises "invalid pattern". |
| Irrefutable / lazy `~` | ❌ | `~` parses as an unbound variable. |
| Guards (function & case) | 🟡 | Boolean guards with `,` conjunction, fall-through to the next equation, and `where` over guards all work. Pattern guards and `let` in guards ❌. **A guarded function whose branches return different parameters loses its dictionary**, even at `Int` with a signature: `f :: Int -> Int -> Int; f a b \| a > b = a \| otherwise = b` fails at runtime with "undefined: <Ord $t>". So does a guard that falls through to `f a b = b`. The same happens for any constrained function that returns its own constrained variable, guards or not. |
| Exhaustiveness / redundancy checks | ❌ | No static check. An unmatched value is a run-time error that names where the match failed, e.g. `f.hs:3:7: Non-exhaustive patterns in f`. |

## Data types

| Feature | Status | Note |
|---|---|---|
| `data`, multiple constructors, args | ✅ | At most 10 arguments per constructor. An 11th overflows a fixed array in `leander-parser-declarations.adb`. |
| Type parameters (polymorphic data) | ✅ | |
| `newtype` | ✅ | Compiles to its field with no wrapper; patterns bind the value itself. |
| Type synonyms (`type`) | 🟡 | Parameterised aliases expand in `To_Core`. The table is process-global, so a synonym is visible in every module regardless of imports. |
| Record syntax (fields, selectors, update) | ❌ | Positional atomic args only. |
| Strictness annotations `!` | ❌ | `data P = P !Int` fails with "expected an atomic expression". |
| Infix / operator constructors in `data` | ❌ | Prefix constructors only. |

## Deriving

| Feature | Status | Note |
|---|---|---|
| `deriving Eq` | ✅ | Nullary + parameterized; structural generator. |
| `deriving (Ord, Enum, Bounded, Show, Read, Ix)` | ❌ | Rejected with "cannot derive …". Only `Eq` has a generator. |

## Type system

| Feature | Status | Note |
|---|---|---|
| Hindley-Milner inference | ✅ | Robinson unification, occurs check, Algorithm-W. |
| Let-polymorphism / generalization | ✅ | |
| Binding-group dependency analysis | 🟡 | Inference consumes pre-grouped SCCs from an upstream builder. |
| Type signatures (top-level) | 🟡 | Used to seed inference, and a plain mismatch (`f :: Int -> Bool; f x = x`) is rejected. But the **inferred-vs-declared generality check is discarded**: `f :: a -> a; f x = x + 1` loads, and so does the Prelude's own `last`, whose body returns `[x]` against `[a] -> a`. |
| Expression annotations `e :: T` | ❌ | No `ESig` form: `(1 :: Int)` fails with "expected ')'". |
| Polymorphic recursion | 🟡 | Only for signature-annotated bindings. |
| Monomorphism restriction | ❌ | Implicit bindings always generalized. |
| Type defaulting (numeric) | ❌ | Ambiguous numeric predicates never resolved. Rarely visible yet, because literals are monomorphic `Int`. |
| Ambiguity detection | ❌ | Unresolvable predicates silently kept. |
| Higher-rank types | ❌ | (Correctly — not H2010.) |

## Type classes

| Feature | Status | Note |
|---|---|---|
| `class` decls, method sigs | ✅ | |
| Default methods | ✅ | |
| Superclasses | ✅ | Transitive closure. |
| `instance` decls | ✅ | |
| Instance contexts `instance C a => C [a]` | ✅ | |
| Context propagation / constrained fns | ✅ | |
| Context reduction / simplification | ✅ | HNF + superclass redundancy removal. |
| Dictionary passing | ✅ | No class trace at runtime. |
| Instance with no `where` | ❌ | `instance Foo Bool` (relying on defaults alone) fails with "missing 'where'". |
| Overlapping / flexible instances | ❌ | First head match wins; no overlap check (not H2010). |
| Multi-parameter type classes | ❌ | Single class variable (not H2010 base). |

## Kinds

| Feature | Status | Note |
|---|---|---|
| `* -> *` constructors (`[]`, `(->)`, `(,)`) | ✅ | |
| Higher-kinded tyvars (`Functor f`) | 🟡 | Work when a scheme supplies the kind; default var kind is `*`. |
| Kind inference / checking | 🟡 | Arity-assigned, not unified; no kind mismatch rejection, no kind variables. |

## Modules

| Feature | Status | Note |
|---|---|---|
| `module Name where` header | ✅ | Dotted names. The header names the module; the file base name must match its last component. |
| Export lists | ✅ | `name`, `(op)`, `T(..)`, `T(C1,C2)`, `C(..)`, `C(m1,m2)`. A name kept back is unreachable even qualified. `module M` re-export rejected. |
| `import` declarations | ✅ | Resolved by name: `Data.List` is `Data/List.hs` under the importing file's directory, then each `-i` directory, then the installed module directory. Cycles are reported. |
| Qualified / `hiding` / `as` / import lists | 🟡 | All enforced, including `T(..)` and `C(..)` expansion. Qualified operators work in a section (`(M.+)`) but not infix (`a M.+ b`). Fixities, like synonyms, are process-global, so `hiding` does not affect parsing. |
| Multiple modules / separate compilation | 🟡 | Several modules load and link together, but their own top-level names still share one namespace inside a handle. Separate compilation is not there yet: `--precompile` writes a `.skix` for any module, but only the Prelude's is read back. |
| Auto-imported Prelude | 🟡 | A synthesised `import Prelude`, so it carries an export list like any module. It is still unconditional, so `import Prelude ()` cannot restrict it. A local declaration does now shadow an imported name. |

## IO

| Feature | Status | Note |
|---|---|---|
| IO monad (`Int -> (a, Int)`) | ✅ | World-passing ADT. |
| `do` desugaring for IO | ✅ | |
| `>>=` / `>>` / `return` | ✅ | |
| `putChar`/`putStr`/`putStrLn`/`print` | ✅ | |
| `mapM`/`mapM_`/`sequence`/`sequence_` | ✅ | Monad-generic. |
| Input (`getLine`/`getChar`/`readFile`) | ❌ | **Output only** — single `#putChar` primitive. |
| `main :: IO ()` entry | ✅ | Driver runs `runIO main`. |
| `foreign import` | 🟡 | `skit` backend only (internal primitives); not H2010 FFI (`ccall`, `foreign export`). |

## Numeric tower

| Feature | Status | Note |
|---|---|---|
| `Num` | 🟡 | Only instance is `Int`, and its `fromInteger` returns `0` regardless of the argument. |
| `Real`, `Integral`, `Fractional`, `Floating`, `RealFrac`, `RealFloat` | ❌ | Entire hierarchy below `Num` missing. |
| `Int` | ✅ | Machine integer; `minBound` is `-536870912`. |
| `Integer` (bignum) | ❌ | Not arbitrary precision. It appears only in `fromInteger`'s signature. |
| `Float` / `Double` | ❌ | The type constructors exist and float literals are typed `Double`, but the value **rounds to an integer** and there are no instances, so no float arithmetic. |
| `Rational` / `Word` | ❌ | Absent. |
| Overloaded numeric literals + defaulting | ❌ | An integer literal is monomorphic `Int`; never wrapped in `fromInteger`. |
| `div`/`mod` | ✅ | `Int -> Int -> Int`, not class methods. |
| `quot`/`rem`/`divMod`/`quotRem`, `^`/`^^`/`**` | ❌ | Fixities declared, functions never defined. |
| `even`/`odd`/`gcd`/`lcm`/`fromIntegral` | ❌ | Absent. |

## Prelude coverage

Item-by-item status against the Report's Prelude export list is tracked in [#97](https://github.com/blancolioni/leander/issues/97).

| Area | Status | Note |
|---|---|---|
| List functions | 🟡 | Have: `map filter foldr foldl foldl' foldl1 (++) concat reverse length null head tail last init (!!) take drop takeWhile dropWhile span break iterate repeat replicate cycle zip zipWith and or any all elem notElem lookup sum product maximum minimum`. Also `zip3 zipWith3 unzip unzip3`. **Missing**: `concatMap`, `foldr1`, `splitAt`, `lines`/`words`/`unlines`/`unwords`, scans. `sum` and `product` are `[Int] -> Int`. |
| Miscellaneous | 🟡 | Have `id const (.) flip ($) ($!) seq not (&&) (\|\|) otherwise fst snd curry uncurry subtract error`. Missing `undefined`, `until`, `asTypeOf`, `(=<<)` (fixity declared, never defined). `subtract` is `Int`-only. |
| `Maybe` | 🟡 | Type, `maybe`, derived `Eq`, `Show`, Functor/Applicative/Monad. Missing `fromMaybe`, `isJust`, `catMaybes`, `mapMaybe`, … |
| `Either` | 🟡 | Type, `either`, derived `Eq` and `Show`. No Functor/Monad instances. |
| Tuples | 🟡 | `Eq`, `Ord` and `Show` instances up to size 7 (the Report asks for 15, plus `Bounded` and `Read`). |
| `Eq`/`Ord`/`Enum`/`Bounded` | 🟡 | `Eq`: `Bool`, `Int`, `Char`, `[a]`, plus derived `Ordering`/`Maybe`/`Either`. `Ord`: `Int` and `Char` only, with `compare`/`max`/`min` defaults. `Enum`: `Int` and `Char`. `Bounded`: `Int` only. |
| `Functor`/`Applicative`/`Monad` | 🟡 | Classes, with instances for `[]` (Functor/Applicative only), `Maybe` and `IO`. **No `Monad []`.** No `*>`/`<*`/`liftA2`/`fail`. |
| `Show` | ✅ | As in the Report: `showsPrec`, `show` and `showList`, with `ShowS`, `shows`, `showChar`, `showString` and `showParen`. Instances for `Bool`, `Int`, `Char` (with the Report's escapes, and strings through `showList`), `[a]`, `()`, `Ordering`, `Maybe`, `Either` and tuples up to size 7. No `deriving Show` yet. |
| `Read` | ❌ | Absent. |
| `Monoid`/`Foldable`/`Traversable` | ❌ | Absent. |

---

## Suggested next steps

Ordered by leverage — most Haskell programs unblocked per unit effort.

### Tier 0 — bugs found while checking this page

These are wrong answers or crashes in forms that are otherwise supported.

- **`last` returns a singleton list.** `last (x:xs) = if null xs then [x] else …` in `Prelude.hs`; should be `x`. It type-checks only because signature generality isn't enforced (item 8). Tracked in [#83](https://github.com/blancolioni/leander/issues/83).
- **Guarded functions that return different parameters lose their dictionary**, even at `Int` (see Patterns → Guards). [#93](https://github.com/blancolioni/leander/issues/93)
- ~~**`['a' .. 'e']` runs out of memory.**~~ Fixed in [#94](https://github.com/blancolioni/leander/issues/94). Any class default that used its own method was compiled as a call to itself; `Enum`'s `enumFromTo` was the case the Prelude hit.
- ~~**`[1..5]` lexes `1.` as a float.**~~ Fixed by the new lexer ([#99](https://github.com/blancolioni/leander/issues/99)). [#95](https://github.com/blancolioni/leander/issues/95)
- ~~**Char literal lexing.**~~ Fixed in [#96](https://github.com/blancolioni/leander/issues/96). Most of what was reported came from the Windows argument layer; the real faults were an end-of-line crash and missing Haskell escapes.
- **`fromInteger` for `Int` returns `0`.** Harmless today because literals never go through it, but it will bite as soon as item 7 lands. Tracked in [#82](https://github.com/blancolioni/leander/issues/82).

### Tier 1 — cheap, high-frequency (do first)

1. ~~**Prelude breadth.**~~ Mostly landed: folds, zips, `drop`/`takeWhile`/`dropWhile`/`span`/`break`, `elem`/`lookup`/`(!!)`, `replicate`/`iterate`/`repeat`/`cycle`, `any`/`all`/`and`/`or`, `maximum`/`minimum`, `Either`+`either`. Still missing: `concatMap`, `lines`/`words`/`unlines`/`unwords`, scans, `fromMaybe` and friends, `even`/`odd`, `quot`/`rem`, `(^)`, `undefined`, `Monad []`.
2. ~~**Guards** (function equations + case alts).~~ Landed for boolean guards; pattern guards and `let` in guards remain.
3. **`deriving Show`** (then `Ord`). `Show` is needed to print a user's data types at the REPL/`print`. Mirror the existing `Eq` generator (`leander-syntax-deriving.adb`). The hand-written instances for lists, `Char`, `Maybe`, `Either` and tuples are in place.

### Tier 2 — foundational

4. **Nested pattern compilation.** Core requires constructor args be variables, so `Just (x:xs)`, `[a,b]`, `(Just x, y)` all fail. Desugar nested patterns into fresh vars + inner matches in the alts compiler. Unlocks idiomatic matching; prerequisite for a lot of real code.
5. ~~**Type synonyms (`type`)** and **`newtype`.**~~ Both landed. Synonyms remain process-global rather than per module.
6. **Expression type annotations `e :: T`.** Needed to disambiguate and to write idiomatic code; small AST + inference addition.

### Tier 3 — correctness cliffs (bigger, sequence together)

7. **Real numeric literals: overloading + defaulting + `Float`/`Double`.** Today floats silently truncate to `Int` and literals are monomorphic `Int`. Insert `fromInteger`/`fromRational` at literal sites, add numeric-tower classes (`Fractional`, `Integral`, …), and add machine float support — this ties directly to **ADR 0001** (object representation / NaN-boxing). Pair with **type defaulting** and **ambiguity detection** (both currently absent) so overloaded literals are usable and ambiguous programs are rejected.
8. **Enforce signature generality** (stop discarding the inferred-vs-declared check).

### Tier 4 — breadth when needed

9. ~~**Module imports/exports** (multi-file programs).~~ Landed — see `share/leander/docs/modules.md`. What is left is separate compilation: reading a module's `.skix` back rather than only the Prelude's.
10. **Input IO** (`getLine`/`getChar`).
11. **Real layout algorithm** (replace ad-hoc indent checks) — foundational but higher risk; do when the ad-hoc rules start failing real code.
12. **Char/String literal patterns**, **left sections**, **`where` on case alternatives**, **instances with no `where`** — small polish items.
