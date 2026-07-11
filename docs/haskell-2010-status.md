# Haskell 2010 Feature Status

Status of Haskell 2010 language support in Leander. Legend:

- ✅ **Full** — works end to end
- 🟡 **Partial** — parses/works with real limits (noted)
- ❌ **None** — absent, or parses but fails downstream

Status reflects *end-to-end* behaviour: a form that the parser accepts but the
core rejects is marked by its effective (worst) status.

---

## Lexical

| Feature | Status | Note |
|---|---|---|
| Integer literals (decimal) | ✅ | No hex/octal (number bases off). |
| Floating literals | 🟡 | Lex OK, but **compile to truncated integers** — no real float (see Numeric). |
| Char literals | 🟡 | Limited escape set. |
| String literals | ✅ | |
| Escape sequences | 🟡 | Only `\a \b \f \n \r \t`; no numeric `\65`/`\x41`, control names, `\&`, or gaps. |
| Negative literals | ❌ | `-` is a binary operator only; Prelude spells it `0 - x`. |
| Layout / offside rule | 🟡 | Ad-hoc per-construct indent checks; no real layout algorithm, no virtual braces, no parse-error close. |
| Explicit braces / semicolons | ✅ | |
| Line comments `--` | ✅ | |
| Block comments `{- -}` | ❌ | Never wired into the lexer. |
| User-defined operators | ✅ | |
| Fixity (`infix`/`infixl`/`infixr`) | ✅ | Prec 0–9, shunting-yard. |

## Expressions

| Feature | Status | Note |
|---|---|---|
| Lambda | 🟡 | **Single variable binder only**; no `\x y ->`, no pattern lambdas. |
| Application | ✅ | |
| Operator sections | 🟡 | Right sections `(op e)` and bare `(op)` only; **left sections `(e op)` fail**. |
| Unary negation | ❌ | `(-1)` mis-parses as a right section. |
| `let` / `where` | 🟡 | Both work on bindings; `where` **not** on case alternatives. |
| `if`/`then`/`else` | ✅ | |
| `case` | 🟡 | Works; **no guards** in alternatives. |
| `do` notation | ✅ | Monad-generic desugar; **no `MonadFail`** — refutable bind mismatch is a hard error. |
| Tuples | ✅ | Construction + `fst`/`snd`; pairs best-supported. |
| List literals | ✅ | |
| List comprehensions | ❌ | No generator/guard syntax. |
| Arithmetic sequences `[a..]`,`[a..b]`,`[a,b..c]` | ✅ | Via `enumFrom*`. |

## Patterns

| Feature | Status | Note |
|---|---|---|
| Variable / wildcard `_` | ✅ | |
| Constructor patterns | ✅ | |
| Cons / list patterns `(x:xs)` | ✅ | |
| Literal patterns | 🟡 | **`Int` only**; char/string/float literal patterns fall into the constructor path and fail. |
| Nested patterns | 🟡 | **One level only** — core requires constructor args be variables (`leander-core-patterns.ads`), so `Just (x:xs)` fails the assert. |
| Multiple equations | ✅ | |
| As-patterns `@` | ❌ | Unreachable (patterns are parsed as expressions). |
| Irrefutable / lazy `~` | ❌ | Same. |
| Guards (function & case) | ❌ | No `\| guard` grammar anywhere. |
| Exhaustiveness / redundancy checks | ❌ | Unmatched → runtime `#error`; no static check. |

## Data types

| Feature | Status | Note |
|---|---|---|
| `data`, multiple constructors, args | ✅ | Silent per-constructor arg cap of 10. |
| Type parameters (polymorphic data) | ✅ | |
| `newtype` | ❌ | Token exists; no parser branch. |
| Type synonyms (`type`) | ❌ | Token exists; no handler. |
| Record syntax (fields, selectors, update) | ❌ | Positional atomic args only. |
| Strictness annotations `!` | ❌ | No bang token. |
| Infix / operator constructors in `data` | ❌ | Prefix constructors only. |

## Deriving

| Feature | Status | Note |
|---|---|---|
| `deriving Eq` | ✅ | Nullary + parameterized; structural generator. |
| `deriving (Ord, Enum, Bounded, Show, Read, Ix)` | ❌ | Explicitly rejected — only `Eq` has a generator. |

## Type system

| Feature | Status | Note |
|---|---|---|
| Hindley-Milner inference | ✅ | Robinson unification, occurs check, Algorithm-W. |
| Let-polymorphism / generalization | ✅ | |
| Binding-group dependency analysis | 🟡 | Inference consumes pre-grouped SCCs from an upstream builder. |
| Type signatures (top-level) | 🟡 | Used to seed inference, but the **inferred-vs-declared generality check is discarded** — over-general signatures not rejected. |
| Expression annotations `e :: T` | ❌ | No `ESig` form. |
| Polymorphic recursion | 🟡 | Only for signature-annotated bindings. |
| Monomorphism restriction | ❌ | Implicit bindings always generalized. |
| Type defaulting (numeric) | ❌ | Ambiguous numeric predicates never resolved. |
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
| `module Name where` header | 🟡 | Name parsed/checked; nothing else. |
| Export lists | ❌ | Everything implicitly exported. |
| `import` declarations | ❌ | No import grammar at all. |
| Qualified / `hiding` / `as` / import lists | ❌ | — |
| Multiple modules / separate compilation | ❌ | Single user module; `Load_Module` replaces the env. |
| Auto-imported Prelude | ✅ | Wholesale; cannot be hidden/shadowed. |

## IO

| Feature | Status | Note |
|---|---|---|
| IO monad (`Int -> (a, Int)`) | ✅ | World-passing ADT. |
| `do` desugaring for IO | ✅ | |
| `>>=` / `>>` / `return` | ✅ | |
| `putChar`/`putStr`/`putStrLn`/`print` | ✅ | |
| Input (`getLine`/`getChar`/`readFile`) | ❌ | **Output only** — single `#putChar` primitive. |
| `main :: IO ()` entry | ✅ | Driver runs `runIO main`. |
| `foreign import` | 🟡 | `skit` backend only (internal primitives); not H2010 FFI (`ccall`, `foreign export`). |

## Numeric tower

| Feature | Status | Note |
|---|---|---|
| `Num` | ✅ | |
| `Real`, `Integral`, `Fractional`, `Floating`, `RealFrac`, `RealFloat` | ❌ | Entire hierarchy below `Num` missing. |
| `Int` | ✅ | Fixed-width Ada `Integer`. |
| `Integer` (bignum) | ❌ | Not arbitrary precision — appears only in `fromInteger`'s signature. |
| `Float` / `Double` | ❌ | Literals **truncate to integer**; no float arithmetic. |
| `Rational` / `Word` | ❌ | Absent. |
| Overloaded numeric literals + defaulting | ❌ | Literal is monomorphic `Int`; never wrapped in `fromInteger`. |
| `div`/`mod` | ✅ | |
| `quot`/`rem`/`divMod`/`quotRem` | ❌ | Declared in fixity list, never defined. |

## Prelude coverage

| Area | Status | Note |
|---|---|---|
| List functions | 🟡 | Have: `map filter foldr (++) concat reverse length take null head tail sum`. **Missing**: `foldl`, `zip`/`zipWith`, `drop`, `takeWhile`/`dropWhile`, `span`/`break`, `elem`, `lookup`, `(!!)`, `replicate`, `iterate`/`repeat`/`cycle`, `last`/`init`, `concatMap`, `and`/`or`/`any`/`all`, `maximum`/`minimum`/`product`, `lines`/`words`/`unlines`/`unwords`, scans. |
| `Maybe` | 🟡 | Type + `maybe` + Functor/Applicative/Monad; missing `fromMaybe`, `isJust`, `catMaybes`, `mapMaybe`, … |
| `Either` | ❌ | Type and `either` absent entirely. |
| Tuples | 🟡 | Pairs only; no triples/`swap`. |
| `Functor`/`Applicative`/`Monad` | 🟡 | Classes + instances for `[]`,`Maybe`,`IO`; **no `Monad []`**; no `*>`/`<*`/`liftA2`/`fail`. |
| `Show` | 🟡 | `show` only (no `showsPrec`/`shows`/`showString`); instances `Bool`,`Int` only — no list/`Maybe`/tuple/`Char` Show. |
| `Read` | ❌ | Absent. |
| `Monoid`/`Foldable`/`Traversable` | ❌ | Absent. |

---

## Suggested next steps

Ordered by leverage — most Haskell programs unblocked per unit effort.

### Tier 1 — cheap, high-frequency (do first)

1. **Prelude breadth.** Add `foldl`, `zip`/`zipWith`, `drop`, `takeWhile`/`dropWhile`, `elem`, `lookup`, `(!!)`, `replicate`, `any`/`all`, `concatMap`, `Either`+`either`. Pure Haskell in `Prelude.hs`; near-zero risk; removes constant friction.
2. **Guards** (function equations + case alts). Pervasive idiom, currently absent. Parser + desugar to nested `if`; no core-IR change. High value, contained scope.
3. **`deriving Show`** (then `Ord`). `Show` is needed to print data types at the REPL/`print`; currently only hand-written `Bool`/`Int`. Mirror the existing `Eq` generator (`leander-syntax-deriving.adb`).

### Tier 2 — foundational

4. **Nested pattern compilation.** Core requires constructor args be variables, so `Just (x:xs)`, `[a,b]`, `(Just x, y)` all fail. Desugar nested patterns into fresh vars + inner matches in the alts compiler. Unlocks idiomatic matching; prerequisite for a lot of real code.
5. **Type synonyms (`type`)** and **`newtype`.** `type` is cheap (alias resolution); `newtype` ≈ `data` with one field. Both common in ordinary programs.
6. **Expression type annotations `e :: T`.** Needed to disambiguate and to write idiomatic code; small AST + inference addition.

### Tier 3 — correctness cliffs (bigger, sequence together)

7. **Real numeric literals: overloading + defaulting + `Float`/`Double`.** Today floats silently truncate to `Int` and literals are monomorphic `Int`. Insert `fromInteger`/`fromRational` at literal sites, add numeric-tower classes (`Fractional`, `Integral`, …), and add machine float support — this ties directly to **ADR 0001** (object representation / NaN-boxing). Pair with **type defaulting** and **ambiguity detection** (both currently absent) so overloaded literals are usable and ambiguous programs are rejected.
8. **Enforce signature generality** (stop discarding the inferred-vs-declared check).

### Tier 4 — breadth when needed

9. **Module imports/exports** (multi-file programs).
10. **Input IO** (`getLine`/`getChar`).
11. **Real layout algorithm** (replace ad-hoc indent checks) — foundational but higher risk; do when the ad-hoc rules start failing real code.
12. **Char/String literal patterns**, **block comments**, **numeric escapes**, **left sections**, **multi-arg/pattern lambdas** — small polish items.
