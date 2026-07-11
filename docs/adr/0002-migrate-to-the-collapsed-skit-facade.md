# ADR 0002: Migrate Leander to the Collapsed Skit Facade

- **Status:** Proposed
- **Date:** 2026-07-09
- **Deciders:** Fraser Wilson

## Context

Skit has been reorganised. Its ADR 0005 collapsed the nine-interface machine
tower, and the implementation went further than that ADR's text: the whole crate
now presents a single concrete facade,
[`Skit.Handles.Handle`](../../skit/src/skit-handles.ads), and the collector,
machine state, and debug printer became **private** (`Skit.Memory`,
`Skit.Machines`, `Skit.Debug`). This was done to unblock ADR 0004 (SPARK the
memory core); see Skit ADR 0004's 2026-07-09 status update.

Leander is the only out-of-crate consumer of Skit, and it does not compile
against the reorganised crate. It still `with`s and calls packages that no
longer exist as a public surface:

| Leander uses | Location | New Skit |
|--------------|----------|----------|
| `Skit.Impl.Machine (Size)` | [leander-handles.adb:130](../../src/leander-handles.adb#L130) | gone → `Skit.Handles.New_Handle` |
| `Skit.Environment` (`.Machine`, `.Bind`, `.Lookup`, `.Parse`) | leander-handles.adb, .ads | gone → operations fold onto `Handle` |
| `Skit.Library.Load_Primitives` | [leander-handles.adb:143](../../src/leander-handles.adb#L143) | gone → client binds primitives itself |
| `Skit.Machine.Reference` | [leander-handles.adb:130](../../src/leander-handles.adb#L130), 302 | gone → `Skit.Handles.Handle` |
| `Skit.Stacks.Abstraction'Class` | [leander.adb:4](../../src/leander.adb#L4), 25, 141 | gone (private `Skit.Machines`) |
| `Skit.Primitives.Abstraction'Class` | [leander.adb:10](../../src/leander.adb#L10), leander-handles.ads | gone → `Primitive_Evaluator` access-to-function |
| `Skit.Terms.Resolver_Interface` | [leander-handles.ads:10](../../src/leander-handles.ads#L10) | gone → `access function (Name : String) return Object` |
| `Skit.Debug.Image` | leander-handles.adb, several | now a **private** package → `Handle.Image` |

The change is not the mechanical two-declaration edit Skit ADR 0005 predicted.
Three of the substitutions are genuine redesigns, not renames.

## What actually blocks the migration

### 1. The Handle exposes no cell or stack operations

`Skit.Handles.Handle` exposes `Evaluate`, `Pop`, `Install`, `Write`, `Image`,
`Primitive`, `Bind`, `Lookup`, `Report`. Leander's foreign-value marshalling
needs `Push`, `Top`, `Apply`, `Left`, `Right`, and `Set` — none of which are on
the Handle:

- `Receive_Value` ([leander-handles.adb:296](../../src/leander-handles.adb#L296))
  builds Skit values: `Push (K); Push (I); Apply; Pop` for a false Boolean, and
  parses a combinator string for a String.
- `Send_Value` ([leander-handles.adb:394](../../src/leander-handles.adb#L394))
  reads a Skit cons list back into Ada by walking `Left`/`Right`.
- `Trace` ([leander-handles.adb:479](../../src/leander-handles.adb#L479)) calls
  `Machine.Set ("trace-eval", ...)`.

Either Skit re-exposes these operations on the Handle, or Leander's marshalling
is reframed onto the operations that do exist (`Install` a term + `Pop`).

### 2. The primitive model lost its per-binding state

Old Skit primitives were tagged values. Leander's `Binding_Instance`
([leander.adb:8-16](../../src/leander.adb#L8-L16)) is a
`Skit.Primitives.Abstraction` carrying per-binding state — the owning `Handle`,
the `Evaluator` closure, and the argument/result `Foreign_Type` arrays — and
dispatches through `Evaluate (This; Stack)`, popping and pushing the machine
stack ([leander.adb:139-155](../../src/leander.adb#L139-L155)).

New Skit primitives are a bare
`Primitive_Evaluator = access function (Arguments : Object_Array) return Object`
([skit.ads:32](../../skit/src/skit.ads#L32)), created via
`Handle.Primitive (Count | Modes, Evaluator)`. An access-to-function carries **no
state** — no `Handle`, no `Arg_Types`, no `Res_Types`. Leander's stateful foreign
bridge cannot be expressed as a plain function pointer. This is the largest piece
of work: the state a `Binding_Instance` held must be recovered some other way.

### 3. Built-in primitives are now the client's job

`Skit.Library.Load_Primitives` no longer exists. The arithmetic and control
primitives (`#add`, `#eq`, `#choose`, `#putchar`, ...) are now bound by the
client, as Skit's own test does
([skit-tests.adb:109-127](../../skit/skit_tests/src/skit-tests.adb#L109-L127)).
Leander must bind them itself at `Create` time.

## Decision drivers

- **Leander must compile against the pinned Skit.** `alire.toml` pins
  `skit = { path = "./skit" }` at the reorganised HEAD; the build is broken until
  Leander follows. This is not optional cleanup.
- **Do not re-inflate Skit's surface without cause.** Skit shrank its public API
  deliberately, to make the core SPARK-provable. Re-exposing `Push`/`Left`/`Set`
  on the Handle to spare Leander a rewrite partially reverses that intent. The
  boundary should reopen only for operations with no reasonable client-side
  expression.
- **The marshalling is the only hard consumer of raw cells.** Everything else in
  Leander (`Compile`, `Evaluate`, `Resolve`) maps cleanly onto
  `Install`/`Pop`/`Image`/`Bind`/`Lookup`. The raw-cell dependency is localised
  to `Send_Value`/`Receive_Value`/`Trace`.
- **The primitive redesign is unavoidable regardless.** No choice on blocker 1
  removes the need to carry per-binding state across a stateless function
  pointer (blocker 2).

## Options considered

### A. Re-expose cell/stack operations on the Handle

Add `Push`, `Top`, `Apply`, `Left`, `Right`, `Set` (or a narrow marshalling
helper) back to `Skit.Handles`. Smallest Leander diff — `Send_Value`/
`Receive_Value` survive almost verbatim.

- Con: widens the surface the SPARK effort just narrowed, and re-exposes raw
  heap-cell access to an external crate — precisely the coupling ADR 0005 set out
  to remove.

### B. Reframe Leander's marshalling onto Install + Pop (proposed)

Express foreign→Skit construction (`Receive_Value`) as `Skit.Terms` terms handed
to `Handle.Install`, and Skit→foreign destruction (`Send_Value`) via the value
already produced by evaluation — Leander already builds combinator strings for
the String case ([leander-handles.adb:326-332](../../src/leander-handles.adb#L326-L332)),
so the pattern exists. Keeps Skit's surface minimal.

- Con: a larger, riskier Leander rewrite; `Send_Value`'s `Left`/`Right` walk
  needs a Skit-side reader if no term-level equivalent is reachable. May still
  force one small read primitive on the Handle.

### C. Blocker 2 — carry primitive state in a handler object

Since a bare function pointer cannot close over state, Skit reintroduces a
client-provided `Primitive_Handler` object dispatched in place. Leander's handler
holds its **per-binding** state — the marshalling `Arg_Types`/`Res_Types` and the
host callback — while the owning `Handle` arrives as a threaded `User_Data`
parameter to the handler's `Evaluate` (not stored, no back-reference, no cycle).
No client-side table, no per-call lookup, no identity. The end-user binding
surface stays trivial (as small as
`type Handler is access procedure (H : Leander.Handles.Handle)`, which Leander
wraps). The mechanism and its SPARK rationale are settled in
[Skit ADR 0006](../../skit/docs/adr/0006-primitive-evaluator-context.md); this
ADR consumes it.

## Decision

Not yet settled. Lean toward **B + C**: reframe marshalling onto the existing
Handle surface (B) rather than re-inflating Skit (A), and carry primitive state in
a `Primitive_Handler` object (C, per Skit ADR 0006). Confirm during migration
whether `Send_Value` can avoid raw cell reads entirely; if one unavoidable reader
remains, add exactly that one operation to the Handle rather than the whole
`Push`/`Top`/`Apply`/`Left`/`Right`/`Set` set.

## Staged migration

Each stage should leave the crate buildable, or as close as the cross-cutting
type changes allow.

1. **Bootstrap the handle.** Replace `Create`'s
   `Skit.Impl.Machine`+`Environment.Create`+`Library.Load_Primitives` with
   `Skit.Handles.New_Handle`, and bind the former `Skit.Library` primitives
   explicitly (blocker 3).
2. **Resolver.** Drop `Skit.Terms.Resolver_Interface` from `Leander.Handles.
   Instance`; pass `Resolve'Access` as the `access function` argument to
   `Handle.Install`. `Instance` no longer needs to be a tagged resolver.
3. **Swap the debug/print calls.** `Skit.Debug.Image (...)` → `Handle.Image`;
   `Machine.Report` → `Handle.Report`.
4. **Primitive handler (blocker 2).** Replace `Binding_Instance` and its
   `Skit.Stacks`-based `Evaluate` with a `Primitive_Handler` extension holding the
   handle, marshalling types, and host callback (Skit ADR 0006); bind through
   `Handle.Primitive`/`Handle.Bind`.
5. **Reframe marshalling (blocker 1).** Rewrite `Send_Value`/`Receive_Value`/
   `Trace` onto `Install`/`Pop` (+ at most one new Handle reader if unavoidable).
6. **Delete dead withs** and confirm `alr build` clean and the integration suite
   green.

## Open questions

- Can `Send_Value`'s `Left`/`Right` cons-list walk be expressed without a
  Skit-side cell reader, or does it force exactly one read operation back onto the
  Handle? Decides how far Option B reaches before touching Skit.
- Does `Trace` (`Machine.Set ("trace-eval", ...)`) have any equivalent on the new
  Handle, or is tracing simply dropped until Skit offers a knob? The Handle has no
  `Set`.
- Where do the former `Skit.Library` primitives' definitions live now — copied
  into Leander, or should Skit expose a `Load_Standard_Primitives (Handle)`
  convenience so every client does not re-bind arithmetic by hand?

## Consequences

To be recorded once implemented. Expected:

- Leander compiles against the reorganised Skit; the build is unbroken.
- Leander's Skit dependency shrinks to `Skit`, `Skit.Handles`, `Skit.Terms`, and
  `Skit.Compiler`; all `Skit.Impl`/`Machine`/`Stacks`/`Primitives`/`Environment`/
  `Library`/`Debug` uses are gone.
- Foreign-value marshalling is expressed against the concrete Handle surface, not
  raw heap cells — keeping Skit's SPARK-motivated boundary intact.
- The stateful foreign-function bridge is re-expressed as a `Primitive_Handler`
  object carrying its own state (Skit ADR 0006), not a stateless function pointer.
