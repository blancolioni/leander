# Type Inference

This document describes how Leander implements Hindley-Milner type
inference with type class predicates, following the approach in
"Typing Haskell in Haskell" (Jones, 1999).

## Overview

Type inference proceeds bottom-up over core expressions. Each
sub-expression is assigned a type (possibly containing fresh type
variables), and unification constraints are solved incrementally by
composing substitutions. Type class constraints are collected as
predicates and reduced after inference completes.

The entry point is `Leander.Handles.Evaluate` (or `Infer_Type`),
which:

1. Parses a string into a syntax expression
2. Desugars to a core expression (`Syntax.To_Core`)
3. Creates an `Inference_Context` from the current type environment
4. Calls `Expressions.Inference.Infer` to infer the type
5. Applies the final substitution to all sub-expression types
   (`Update_Type`)
6. Compiles the typed expression to lambda calculus

## Data structures

### Types (`leander-core-types.ads`)

Types are immutable trees with four variants:

    type Instance_Tag is (TVar, TCon, TGen, TApp);

- **TVar** — Type variable, carrying a `Tyvars.Instance` (name + kind)
- **TCon** — Type constructor, carrying a `Tycons.Instance` (name + kind)
- **TGen** — Generic (quantified) variable, carrying a positional index
- **TApp** — Type application (left applied to right)

Function types are represented as `TApp(TApp(T_Arrow, From), To)`.
There is no special arrow node.

### Substitutions (`leander-core-substitutions.ads`)

A substitution is a list of `(Name, Type)` pairs mapping type variable
names to types. Key operations:

- `Empty` — identity substitution
- `Compose(S1, S2)` — applies S1 after S2
- `Singleton(Name, Type)` — maps one variable
- `Apply(Type, Subst)` — walks a type tree and replaces variables
- `Without(Subst, Names)` — removes bindings for the given names
- `Merge(S1, S2)` — combines two substitutions, failing if they
  disagree on any variable

### Schemes (`leander-core-schemes.ads`)

A type scheme is a quantified type: an array of kinds (one per bound
variable) plus a qualified type. Bound variables in the inner type are
replaced with `TGen(Index)` nodes.

- `To_Scheme(T)` — wraps a monotype (no quantified variables)
- `Quantify(Vs, QT)` — abstracts type variables Vs, replacing them
  with TGen indices in the qualified type
- `Fresh_Instance(Scheme)` — instantiates a scheme by creating fresh
  type variables for each TGen, returning a qualified type

### Qualified types (`leander-core-qualified_types.ads`)

A qualified type pairs a qualifier (list of predicates) with a
monotype. For example, `Eq a => a -> a -> Bool` has the predicate
`Eq a` and the type `a -> a -> Bool`.

### Predicates (`leander-core-predicates.ads`)

A predicate is a pair of (class name, type). For example, `Eq Int`
or `Show a`. Predicates are collected during inference and reduced
(simplified) afterward.

### Type environment (`leander-core-type_env.ads`)

A type environment maps names to type schemes. It is represented as a
linked list of maps — `Compose` creates a new environment that
extends an existing one, forming a scope chain. The environment
supports substitution application (`Apply`), which threads the
substitution through every scheme in the chain.

### Inference context (`leander-core-inference.ads`)

The inference context carries all mutable state during inference:

    type Inference_Context is tagged record
       Success       : Boolean;
       Error_Message : Leander_Name;
       Expr_Types    : Type_Maps.Map;     -- bindings: expression -> type
       Inferred_Type : Type_Reference;    -- final result type
       Type_Env      : Type_Env.Reference;-- current type environment
       Env_Stack     : Env_Stacks.List;   -- saved environments
       Subst         : Substitutions.Instance; -- accumulated substitution
       Predicates    : Predicate_Lists.List;   -- collected constraints
    end record;

The `Env_Stack` implements lexical scoping: `Save_Type_Env` pushes
the current environment before entering a scope, and `Restore_Type_Env`
pops it on exit. Substitutions are composed into `Subst` via
`Save_Substitution`, which composes the new substitution with the
existing one.

## Expression inference (`leander-core-expressions-inference.adb`)

The core of inference is the `TI` function, which pattern-matches on
expression form and returns a substitution:

### Variable (EVar)

Look up the variable's scheme in the type environment. Instantiate
with fresh type variables via `Fresh_Instance`. Bind the expression
to the instantiated type. Predicates from the scheme's qualifier are
saved to the context.

    Sigma := Type_Env.Lookup(Var_Id)
    Q := Sigma.Fresh_Instance
    Bind(E, Q)
    return Empty

### Constructor (ECon)

Identical to variable inference: look up the constructor's scheme
in the type environment, instantiate it, and bind.

### Literal (ELit)

Bind the expression to the literal's intrinsic type (e.g., integer
literals get type `Int`, character literals get type `Char`).

### Application (EApp)

Create a fresh type variable `Tv` for the result. Infer the function
type, then infer the argument type in the substituted environment.
Unify the function type with `Arg_Type -> Tv`. The result type is
`Tv` with the unifying substitution applied.

    Tv := New_TVar
    S1 := TI(Left)
    -- apply S1 to type environment
    S2 := TI(Right)
    S3 := MGU(Left_Type.Apply(S2), Fn(Right_Type, Tv))
    Bind(E, Tv.Apply(S3))
    return S3.Compose(S2).Compose(S1)

### Lambda (ELam)

Create a fresh type variable `Tv` for the parameter. Extend the
type environment with `LVar :: Tv` (as a monotype scheme). Infer the
body type. The lambda's type is `Tv.Apply(S1) -> Body_Type`.

    Tv := New_TVar
    -- extend environment with LVar : To_Scheme(Tv)
    S1 := TI(Body)
    Bind(E, Fn(Tv.Apply(S1), Body_Type))
    return S1

### Let (ELet)

Save the type environment, infer the binding group (which extends
the environment with generalized types), then infer the body in
the extended environment. The let expression's type is the body's
type. Binding group inference is described below.

## Pattern inference (`leander-core-patterns-inference.adb`)

### Variable pattern (PVar)

Create a fresh type variable. Extend the type environment with
the variable bound to a monotype scheme of that variable.

### Literal pattern (PLit)

Bind the pattern to the literal's type.

### Constructor pattern (PCon)

Create fresh type variables `T1..TN` for each constructor argument
and a fresh variable `R` for the result type. Look up the
constructor's scheme, instantiate it, then unify `T1 -> T2 -> ... ->
TN -> R` with the instantiated type. This determines the argument
types and the result type. Extend the type environment with each
constructor argument bound to its type variable.

## Alt inference (`leander-core-alts-inference.adb`)

An alt is a pattern-expression pair (a case branch). Inference:

1. Save the type environment
2. Infer the pattern (which extends the environment with
   pattern-bound variables)
3. Infer the expression in the extended environment
4. Restore the type environment
5. Bind the alt to `Pat_Type -> Expr_Type` (a function from the
   matched type to the result type)

## Binding group inference (`leander-core-binding_groups-inference.adb`)

Binding groups contain implicit bindings (no type signature) and
explicit bindings (with a type signature). They are processed in
dependency order.

### Implicit bindings

For a mutually recursive group of N bindings without type signatures:

1. Create fresh type variables `T1..TN`
2. Extend the type environment with each binding mapped to
   `To_Scheme(Ti)` (monotype — not yet generalized)
3. Infer each binding's alts, unifying each alt's type with `Ti`
4. Apply the accumulated substitution to all `Ti`
5. Compute free variables: `Gs = Tyvars(T1..TN) - Tyvars(Env)`
6. Generalize: `Quantify(Gs, [], Ti)` for each binding
7. Update the type environment with the generalized schemes

This is the standard let-generalization rule: variables not free in
the enclosing environment are universally quantified.

### Explicit bindings

For bindings with a declared type signature:

1. Instantiate the declared scheme with fresh type variables
2. Infer the binding's alts, unifying with the instantiated type
3. Check that the inferred type is at least as general as the
   declared type (currently logged but not rejected)

## Unification (`leander-core-types-unification.adb`)

The `Most_General_Unifier` function implements Robinson's unification
algorithm:

- **TVar vs anything**: bind the variable (after occurs check)
- **Same TCon**: succeed with empty substitution
- **Different TCon**: fail
- **TApp vs TApp**: unify left sides to get S1, then unify
  right sides (with S1 applied) to get S2, return S1 composed
  with S2
- **TGen**: succeed with empty substitution (generic variables
  are treated as opaque)

The occurs check prevents infinite types: if variable `a` appears
inside a type `T`, then `a` cannot unify with `T`.

The `Unify` procedure is the context-aware wrapper: it applies the
current substitution to both sides before calling `Most_General_Unifier`,
then saves the result into the context.

## Type classes (`leander-core-type_classes.adb`)

### Instance resolution

`By_Instance` finds a matching type class instance for a predicate.
It iterates over all known instances of the predicate's class and
attempts to match the instance head against the predicate type using
one-way matching (not full unification). If a match is found, the
instance's qualifier predicates (superclass constraints) are returned,
with the matching substitution applied.

### Entailment

`Entails(Current, Check)` tests whether a predicate `Check` is
entailed by a set of predicates `Current`. It checks:

1. **Superclass entailment**: if any predicate in `Current` has a
   superclass matching `Check`'s class
2. **Instance entailment**: if `Check` can be resolved by an instance,
   and all of that instance's prerequisites are recursively entailed
   by `Current`

### Predicate reduction

After inference, the collected predicates are processed in two steps:

1. **To_Head_Normal_Form** — recursively resolves predicates whose
   type arguments are not in head-normal form (i.e., the type head is
   not a variable or constructor). Each non-HNF predicate is replaced
   by the prerequisites of its matching instance.

2. **Simplify** — removes redundant predicates. A predicate is
   redundant if it is entailed by the remaining predicates (via
   superclass or instance relations).

The reduced predicates become the context of the inferred type. For
example, if inference produces `[Eq a, Ord a]` and `Eq` is a
superclass of `Ord`, simplification removes `Eq a`, yielding
`Ord a => ...`.

## Kinds (`leander-core-kinds.ads`)

Kinds classify types. The kind system is simple:

- `Star` (`*`) — the kind of types
- `Kind_Function(K1, K2)` — the kind of type constructors

Kinds are represented as natural numbers. `Star` is 0; function kinds
are encoded by allocating new indices. Kind checking ensures that type
constructors are applied to the correct number and kind of arguments.

## Full pipeline example

Inferring `map f []`:

1. **Fresh instance**: `map :: forall a b. (a -> b) -> [a] -> [b]`
   instantiates to `(t1 -> t2) -> [t1] -> [t2]`
2. **Apply f**: unify `(t1 -> t2) -> [t1] -> [t2]` with
   `typeof(f) -> Tv1`, yielding `typeof(f) = t1 -> t2` and
   `Tv1 = [t1] -> [t2]`
3. **Apply []**: `[]` instantiates to `forall a. [a]`, giving
   `[t3]`. Unify `[t1] -> [t2]` with `[t3] -> Tv2`, yielding
   `t1 = t3` and `Tv2 = [t2]`
4. **Result**: `[t2]` — a list of whatever `f` returns
