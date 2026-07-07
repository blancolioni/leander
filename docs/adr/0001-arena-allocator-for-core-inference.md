# ADR 0001: Arena Allocator for Core Type Inference

- **Status:** Proposed
- **Date:** 2026-07-07
- **Deciders:** Fraser Wilson

## Context

Every `Leander.Core` node type (expressions, patterns, alts, bindings, binding
groups, types, schemes, type environments, ...) allocates through the generic
[`Leander.Allocator`](../../src/leander-allocator.ads): `Allocate` does a plain
`new`, appends the pointer to an `Allocated_List`, and a mark-and-sweep `Prune`
frees everything not previously `Protect`ed. In practice `Leander.Core.Prune` is
called only from the test runner ([leander-tests.adb](../../src/leander-tests.adb));
during normal operation Core memory is never reclaimed — it accumulates for the
life of the process.

The dominant source of allocation is type inference. Types are immutable trees,
and they are **not interned**: `Application`/`Apply` allocate a fresh tree on
every substitution step ([leander-core-types.adb](../../src/leander-core-types.adb),
`Application` and `Apply`). A single unification churns many intermediate `Type`
trees, almost all of which are discarded — only the final inferred type attaches
to a surviving node. This mirrors the situation ADR 0003 found on the Skit side,
where 99.8% of compile-phase allocation was transient scratch.

An arena (bump-allocate, bulk-reset) is the natural fit for this churn. The
difficulty is that some inferred values *do* survive, and they are entangled with
the transient ones.

### Survivor analysis

The concrete environment record ([leander-environment.adb](../../src/leander-environment.adb),
`type Instance`) retains, transitively, almost the entire Core graph:

| Env field | Core types retained (transitive) |
|-----------|----------------------------------|
| `Tycons` | Data_Types → Types, Schemes, Kinds |
| `Cons` | Schemes → Qualified_Types → Types, Predicates; Data_Types |
| `Bindings` | Binding_Groups → Bindings → Alts → Patterns + Expressions |
| `Type_Env` | Type_Env → Schemes, Types |
| `Classes` | Type_Classes → Binding_Groups (→ Expressions) + Types/Predicates |
| `Instances` | Type_Instances → Qualifiers → Predicates → Types; + Binding_Groups |
| `Context` | Inference_Context → Types, Substitutions → Types, Predicates |
| `Values` | `Calculus.Tree` — **off-arena, already survives** |
| `Class_Syntax_Bindings` | `Syntax.Bindings` — **separate allocator, not Core** |
| `Imports` | other environments — recurse |

Four facts constrain the design:

1. **`Get_Bound_Calculus` is lazy.** It compiles a binding's Core graph to a
   `Calculus.Tree` on first use and caches the tree in `Values`
   ([leander-environment.adb](../../src/leander-environment.adb),
   `Get_Bound_Calculus`). A binding never forced keeps its Core expression graph
   alive indefinitely, across operations.
2. **`Import` aliases by reference.** An importing environment shares the
   imported environment's classes, instances, and bindings; the full binding and
   expression graph survives cross-module.
3. **`To_Calculus` on expressions reads the node's baked qualified type**
   ([leander-core-expressions.adb](../../src/leander-core-expressions.adb),
   `To_Calculus` uses `This.Qualified_Type`); the context is used only write-side
   there (`Save_Predicates`). **But compiling pattern matches does not.** The alts
   compiler reads `Context.Get_Type (Pat)` and applies `Context.Current_Substitution`
   at compile time
   ([leander-core-alts-compiler.adb](../../src/leander-core-alts-compiler.adb),
   `Add`). Compilation is lazy (`Get_Bound_Calculus`), so it runs against the
   *persistent* `This.Context` populated at load. `Inference_Context.Expr_Types`
   (pattern types) and `Inference_Context.Subst` are therefore **survivors read
   after load**, not transient scaffolding. (An earlier draft of this ADR wrongly
   assumed Expr_Types was transient; that error is corrected here.)
4. **The one-off expression path stores nothing back into the environment.**
   [`Handles.Compile`](../../src/leander-handles.adb) and `Infer_Type` build a
   *local* `Inference_Context` and a *local* Core expression, emit a
   `Calculus.Tree` (off-arena) or a type string, and discard everything else.

The naive reading of "put all of Core in an arena, copy survivors out, reset"
implies a whole-Core copying collector: relocate every node type, maintain an
old→new forwarding map, re-key `Inference_Context.Expr_Types` (keyed by
expression-node address) against the relocated addresses, and break cycles
(recursive `let`, mutually recursive binding groups). That is a large, high-risk
change, and it spends copy effort on the expression graph — which mostly survives
anyway and gains nothing from an arena.

## Decision drivers

- **Attack the real garbage, not the whole graph.** The transient churn is
  inference-internal `Type` trees. The expression/binding graph is retained
  lazily and does not benefit from arena management.
- **No pervasive signature churn.** The environment stores values typed
  `Types.Reference`, `Schemes.Reference`, etc. A survivor must keep its type so it
  can be stored back without touching every interface.
- **No re-keying and no cycle handling if avoidable.** Both are consequences of
  arenaing the expression graph. Keeping expressions on the permanent heap makes
  their addresses stable and confines the arena to the acyclic type layer.
- **Bound risk by staging.** The high-value, near-zero-risk case (per-operation
  REPL churn) should land first and independently of the harder module-load case.

## Decision

Confine the arena to the **type layer** and drive it with **subpools**, not with
two separate access types.

### One pool, two subpools

`Leander.Core.Types` gains a `System.Storage_Pools.Subpools`-based pool with two
subpools:

- **`Permanent`** — types that outlive the current operation (module-load
  metadata). Never reset.
- **`Scratch`** — per-operation inference churn. Reset at the end of the
  operation.

There is a single access type, `Types.Reference`, backed by this pool
(`for Reference'Storage_Pool use Pool`). Survivors keep their type; no interface
changes. A **stack** of "current subpool" selectors is pushed/popped by the
handle around each operation; inference code keeps calling
`Types.Application`/`Apply` unchanged and allocates into whichever subpool is
current. This is standard Ada 2022; there are no availability concerns.

The expression graph (expressions, patterns, alts, bindings, binding groups) and
the metadata headers stay on the **permanent heap exactly as today**. They are
retained lazily and an arena would only force them to be relocated wholesale. The
type layer is acyclic — types are trees, schemes wrap types, and there are no
back-edges into expressions — so confining the arena to it removes both the cycle
problem and the address re-keying problem.

### Where the boundaries go

| Path | What survives | Allocation |
|------|---------------|------------|
| `Compile` / `Evaluate` / `Infer_Type` (one-off) | nothing Core/Type — output is off-arena `Calculus.Tree` or a string | `Scratch`; reset on return, **zero relocation** |
| `Get_Bound_Calculus` (lazy) | off-arena `Calculus.Tree` cached in `Values` | `Scratch`; reset after, clearing context predicates first |
| Source module load (`Load_Module`) | the whole final type layer **plus the persistent `Inference_Context`** (`Expr_Types`, `Subst`, `Type_Env`), read lazily by later compilation — see fact 3 | **deferred** (Stage 3); currently allocates into `Permanent`, unbracketed |

`Evaluate` delegates all Core/Type allocation to `Compile` and then works purely
on the Skit machine (separate memory). The bracket therefore belongs **inside
`Compile`**, not around `Evaluate`: the machine-evaluation phase triggers lazy
`Resolve` → `Get_Bound_Calculus` callbacks *after* `Compile` returns, and each of
those is its own independent `Scratch` episode.

## Staged migration

Stages 1–2 reclaim per-operation churn and require nothing but a
bracket-and-reset, because at the reset point *everything* in `Scratch` is
garbage. Both are **implemented**. Stage 3 (source module load) is **deferred** —
the survivor analysis below turned out larger than first drafted.

Each stage keeps the self-test and integration suites green.

1. **Scratch arena for one-off evaluation.** *(Implemented.)* Add the two-region
   pool to `Leander.Core.Types` (defaulting to `Permanent`). Bracket
   [`Handles.Compile`](../../src/leander-handles.adb) and `Infer_Type`: push
   `Scratch`, run, reset `Scratch` on return. No relocation — the one-off path
   stores nothing into the environment. This reclaims the dominant REPL
   substitution churn.
2. **Extend to `Get_Bound_Calculus`.** *(Implemented.)* Bracket its body with its
   own `Scratch` episode; clear `Inference_Context` predicates before the reset.
   In the current flow every `Get_Bound_Calculus` already nests inside `Compile`'s
   bracket, so this yields no allocation delta today; it is defensive correctness
   that closes a latent dangling-predicate window and covers any future depth-0
   (lazy) resolution.
3. **Source module load. *(Deferred.)*** This is where the standing garbage lives
   — after boot, Prelude load accounts for ~383 k permanent allocations / ~24 MB,
   most of it transient unification churn. But it is not a growing leak: Stages
   1–2 already stop per-operation growth, and load happens rarely (Prelude once,
   plus occasional user modules), so the footprint is roughly constant, not
   climbing. The reason it is hard is fact 3: lazy compilation reads the
   *persistent* `Inference_Context` (`Expr_Types` pattern types, `Subst`) as well
   as node `QT`s, so the survivor set is the whole final type layer **plus** that
   context. A naive bracket-and-reset would free pattern types and the
   substitution and crash the pattern-matching path. Two ways forward, neither
   cheap:

   - **3A — relocate the type layer at load end.** Keep the architecture. Deep-copy
     every `Type` reachable from the persistent context (`Expr_Types`, `Subst`,
     `Type_Env`), the environment metadata (constructor schemes, data types,
     classes, instances), and the node `QT`s into `Permanent`; rewrite every
     holder; reset `Scratch`. Needs a forwarding map and mutators/rebuild for
     ~7 holder kinds. Mechanical but broad, with real dangling risk.
   - **3B — decouple compilation from the context.** Bake everything the compiler
     needs onto nodes at load (apply `Subst` fully; bake pattern types onto
     pattern nodes as expression QTs already are) so `To_Calculus` and the alts
     compiler stop reading `Context.Get_Type` / `Current_Substitution`. The whole
     `Inference_Context` then becomes garbage after load, and reset keeps only the
     baked node types and environment metadata. Touches inference and the alts
     compiler, but removes the design smell that compilation depends on the
     inference context, and shrinks the survivor set to something local.

   Revisit when steady-state footprint becomes a concern; prefer 3B if the
   compile/inference coupling is being reworked anyway.

## Consequences

Stages 1–2 are implemented; self-test and integration suites stay green (135/135).

- Per-operation type-inference churn is reclaimed by an O(1) `Scratch` reset
  instead of accumulating for the life of the process; the REPL hot path relocates
  nothing. A single `1 + 2` evaluates correctly through scratch allocation and
  reset (including lazy dictionary resolution inside the bracket).
- The arena is confined to the acyclic type layer, so there is no forwarding map,
  no cycle handling, and no `Expr_Types` re-keying in stages 1–2. Expression-node
  addresses stay stable because the expression graph remains on the permanent
  heap.
- `Leander.Core.Types` gains a `Report` (permanent size, peak scratch size, total
  and permanent allocation counts), surfaced through the REPL `:report`. This is
  what quantified the deferred Stage 3 target: after boot, permanent ≈ 383 k
  allocations / 24.5 MB (Prelude load), scratch peak ≈ 205 kB per operation.
- `Leander.Allocator`'s mark-and-sweep `Prune`/`Protect` is now redundant for
  `Types` (its `Prune` is a no-op); other Core types still use it.
- Confirmed during implementation: `Evaluate`/`Infer_Type` store nothing Core/Type
  into the environment (safe to reset). Corrected: `Inference_Context.Expr_Types`
  and `Subst` are read by lazy compilation (fact 3), so they are load survivors —
  which is what makes Stage 3 the larger of the three and why it is deferred.
