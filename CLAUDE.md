# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

Leander is a Haskell compiler written in Ada 2022. It compiles a subset of Haskell to SKI combinators, which are executed by an embedded virtual machine called [skit/](skit/). The standard library lives in [share/leander/modules/Prelude.hs](share/leander/modules/Prelude.hs).

## Build & Run

The project uses [Alire](https://alire.ada.dev/) (Ada package manager) with GNAT.

```bash
# Build
alr build

# Run the REPL
./bin/leander

# Evaluate a single expression (requires Prelude loaded separately via REPL or module)
./bin/leander -e "1 + 2"

# Run a Haskell file with a `main :: IO ()` entry point
./bin/leander --main=share/leander/tests/RunTests.hs

# Run all tests (unit + integration)
./bin/leander --self-test
```

There is no way to run a single named test suite from the CLI. Individual test suites are invoked from within [src/leander-tests.adb](src/leander-tests.adb) — to isolate one, temporarily comment out the others there and rebuild.

## Compiler Pipeline

```
String
  → Leander.Parser          (leander-parser-*.adb)
  → Leander.Syntax          (leander-syntax-*.ads/adb)   — parse tree
  → .To_Core                (leander-syntax-*.adb)
  → Leander.Core.Expressions                             — core IR
  → Expressions.Inference.Infer                          — HM type inference
  → .To_Calculus            (leander-core-expressions.adb)
  → Leander.Calculus        (leander-calculus.ads/adb)   — lambda calculus
  → Leander.Calculus.Compile → Skit.Compiler.Compile
  → Skit.Machine.Evaluate                                — SKI execution
```

The public API for all of this is [src/leander-handles.ads](src/leander-handles.ads): `Handle.Load_Module`, `Handle.Evaluate`, `Handle.Infer_Type`. The driver at [src/leander-driver.adb](src/leander-driver.adb) is a thin wrapper around `Handles` and the test runner.

## Type Class Compilation

Type classes compile to **dictionary passing** — no trace of the class system remains at runtime:

- A **class declaration** produces one selector function per method (Scott-encoded: `\$inst. $inst (\$1..$N. $i)`) and stores them in the environment's value map under the method name.
- An **instance declaration** compiles to a dictionary value stored under a synthetic key like `<Eq Bool>`.
- A **constrained variable** at a use site becomes the variable applied to its dictionary arguments: `f <Eq a>`.
- During elaboration (`leander-environment.adb`), concrete predicates are resolved to dictionary names; remaining type-variable predicates become lambda parameters.

Full details in [share/leander/docs/type-classes.md](share/leander/docs/type-classes.md).

## Type Inference

Standard Hindley-Milner following Jones 1999 ("Typing Haskell in Haskell"):

- Types are immutable trees: `TVar` (type variable), `TCon` (constructor), `TApp` (application), `TGen` (quantified index). Function types are `TApp(TApp(Arrow, A), B)` — no special node.
- Substitutions are composed incrementally; the inference context accumulates them in `Subst`.
- Predicates (type class constraints) are collected during inference and **reduced** afterward: first to head-normal form (resolve concrete instances), then simplified (remove predicates entailed by others via superclasses).
- Binding group generalization: after inferring a binding group, free type variables not appearing in the enclosing environment are universally quantified.

Key files: [src/leander-core-inference.ads](src/leander-core-inference.ads), [src/leander-core-expressions-inference.adb](src/leander-core-expressions-inference.adb), [src/leander-core-binding_groups-inference.adb](src/leander-core-binding_groups-inference.adb).

Full details in [share/leander/docs/type-inference.md](share/leander/docs/type-inference.md).

## Integration Tests

Integration tests live in [share/leander/tests/integration/](share/leander/tests/integration/) and are driven by [src/leander-tests-integration.adb](src/leander-tests-integration.adb). There are three test patterns:

- `Test_Eval` — parse and evaluate a single expression against the Prelude environment; check inferred type and SKI result value.
- `Test_Module` — load a `.hs` file, evaluate an expression in its environment; check the SKI result.
- `Test_Main` — load a `.hs` file and evaluate `runIO main`; pass if the result is `"I"` (the SKI identity, representing `()`).

SKI machine values are shown in combinator notation: `K` = `True`, `K I` = `False`, `I` = `()` / unit.

## IO Encoding

`IO a` is `Int -> (a, Int)` (a state-passing function over a world token). `runIO` seeds it with `1`. `putChar` calls the `#primPutChar` primitive. The do-notation desugaring in the parser lowers `do { x <- e; ... }` to `e >>= \x -> ...` before reaching the core IR. Details in [share/leander/docs/io.md](share/leander/docs/io.md).

## Foreign Imports

Primitives are imported from the Skit machine with:

```haskell
foreign import skit "#opcode" #localName :: Type
```

The `#name` syntax denotes a Skit built-in. These are the only way to reach machine operations; everything else is defined in terms of them in [Prelude.hs](share/leander/modules/Prelude.hs).

## Ada Conventions

- All source is Ada 2022 with assertions (`-gnata`), full validity checks (`-gnatVa`), and strict style checks (`-gnatyaABbc...`). Style warnings are errors.
- Interfaces are in `.ads` files; bodies in `.adb`. Package hierarchies mirror directory structure under [src/](src/).
- Access types are used for reference semantics (e.g., `type Reference is access constant Instance'Class`). The codebase does not use Ada.Finalization for GC — memory is manually managed or arena-allocated.
- Line endings must be LF (not CRLF).
