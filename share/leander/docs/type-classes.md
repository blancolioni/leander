# Type Classes in the Lambda Calculus

This document describes how Leander compiles Haskell type classes and
instances into untyped lambda calculus using dictionary passing.

## Overview

Type classes are compiled away entirely before execution. Each class
becomes a family of selector functions (one per method), each instance
becomes a dictionary value, and each constrained use site receives the
dictionary as an extra argument. At runtime, no trace of the class
system remains — only ordinary function application.

## Class declarations

When a class is declared:

```haskell
class Eq a where
    (==), (/=) :: a -> a -> Bool
```

the environment registers two things for each method:

1. **A type scheme** — the method's declared type, with the class
   constraint prepended. For example, `(==)` gets the scheme
   `Eq a => a -> a -> Bool`.

2. **A selector function** — a lambda calculus term that extracts the
   method from a dictionary.

### Selector construction

For a class with N methods, the i-th method (1-indexed) gets the
selector:

    \$inst. $inst (\$1.\$2...\$N. $i)

This applies the dictionary `$inst` to a projection function that
takes N arguments and returns the i-th one — using the same Scott
encoding as algebraic data types.

For `Eq` with methods `(==)` and `(/=)`:

    (==) = \$inst. $inst (\$1.\$2. $1)     -- select first method
    (/=) = \$inst. $inst (\$1.\$2. $2)     -- select second method

The selectors are stored in the environment's value map under the
method name. They are built in `Environment.Type_Class`
(`leander-environment.adb`).

## Instance declarations

When an instance is declared:

```haskell
instance Eq Bool where
    (==) True  = \b -> case b of { True -> True; False -> False }
    (==) False = \b -> case b of { True -> False; False -> True }
    (/=) x y = not (x == y)
```

two things happen:

1. **Instance registration** — the instance is recorded in the
   environment's `Instances` map (keyed by class name) for use during
   type inference and predicate resolution.

2. **Dictionary compilation** — during elaboration, the instance
   methods are compiled to lambda calculus and assembled into a
   dictionary value.

### Dictionary construction

During `Environment.Elaborate`, for each class and each of its
instances, the compiler:

1. Infers types for the instance binding group.
2. Compiles each method to a `Calculus.Tree`.
3. Builds a dictionary by applying a constructor-like function to all
   the compiled methods:

```
dictionary = \$class. $class method_1 method_2 ... method_N
```

where the methods are given in the same order as the class
declaration.

The dictionary is stored in the environment's value map under a
synthetic name derived from the predicate. For `Eq Bool`, the key is
`<Eq Bool>`. This encoding matches the references generated at use
sites (see below).

### Constrained instances

When an instance has constraints:

```haskell
instance (Eq a) => Eq [a] where ...
```

the instance is registered with its constraint predicates. During
type class resolution, satisfying `Eq [a]` produces the prerequisite
`Eq a`, which must itself be satisfied — either by another instance or
by the calling context's constraints.

The dictionary for such an instance is compiled the same way, but the
method bodies may reference the constraint's dictionary via the
predicate-passing mechanism described below.

## Use-site compilation

When a constrained variable is compiled to lambda calculus, the
predicates attached to its qualified type become extra arguments.

### Variable references

In `Expressions.To_Calculus`, the `EVar` case handles predicates:

```
-- For a variable x :: Eq a => a -> a -> Bool
-- with predicates [Eq a]:
x  ==>  x <Eq a>
```

Each predicate produces an `Apply(x, Symbol("<Eq a>"))` node,
threading the dictionary as an argument. The predicate names use the
`<ClassName Type>` convention, matching the dictionary keys in the
environment.

### Value lookup

When the environment compiles a binding to lambda calculus
(`Environment.Lookup`), it wraps the result in a lambda for each
predicate the binding requires:

```
-- If binding f has predicates [Eq a, Show a]:
f  ==>  \<Eq a>.\<Show a>. <compiled body of f>
```

This makes the compiled binding a function that takes dictionary
arguments before its regular arguments.

## End-to-end example: `(==)`

### Class declaration

```haskell
class Eq a where
    (==), (/=) :: a -> a -> Bool
```

Registers:
- `(==) :: Eq a => a -> a -> Bool` in the type environment
- `(==) = \$inst. $inst (\$1.\$2. $1)` in the value map

### Instance declaration

```haskell
instance Eq Bool where
    (==) True  = \b -> case b of { True -> True; False -> False }
    (==) False = \b -> case b of { True -> False; False -> True }
    (/=) x y = not (x == y)
```

Compiles the dictionary:
```
<Eq Bool> = \$class. $class
    (\$v. $v (\b. b True False) (\b. b False True))
    (\x.\y. not ((==) <Eq Bool> x y))
```

Note that `(/=)` references `(==)` via `(==) <Eq Bool>` — the
selector applied to the same dictionary, which will be resolved
at runtime.

### Use site

```haskell
f x y = x == y
```

Type inference infers `f :: Eq a => a -> a -> Bool` with predicate
`Eq a`. Compilation produces:

```
f = \<Eq a>.\x.\y. (==) <Eq a> x y
```

Expanding the selector for `(==)`:

```
f = \<Eq a>.\x.\y. (\$inst. $inst (\$1.\$2. $1)) <Eq a> x y
```

At a concrete call site like `f True False`:
- The predicate `Eq Bool` is resolved to the dictionary `<Eq Bool>`
- The call becomes `f <Eq Bool> True False`
- The selector extracts the first method from the Bool dictionary
- The method receives `True` and `False` and pattern-matches

## Superclass constraints

When a class has superclass constraints:

```haskell
class Eq a => Ord a where
    (<), (<=), (>=), (>) :: a -> a -> Bool
```

the class predicates record that `Eq` is a superclass of `Ord`.
This is used during:

1. **Entailment checking** — `Entails([Ord a], Eq a)` succeeds
   because `Eq` is in the transitive superclass closure of `Ord`.

2. **Predicate reduction** — if both `Eq a` and `Ord a` appear in a
   predicate set, `Simplify` removes `Eq a` because it is entailed
   by `Ord a`.

Superclass resolution is implemented in `Type_Classes.Super_Classes`,
which recursively computes the transitive closure of all superclasses.

## Predicate lifecycle

1. **Collection** — during type inference, each instantiation of a
   polymorphic scheme may produce predicates. These are accumulated
   in the inference context's predicate list.

2. **Reduction** — after inference, `Reduce` normalizes and
   simplifies the predicate set. Head-normal-form conversion
   resolves concrete predicates (like `Eq Int`) to their instances.
   Simplification removes predicates entailed by others.

3. **Compilation** — remaining predicates (those involving type
   variables) become lambda parameters on the compiled term and
   application arguments at use sites.

4. **Resolution** — at compilation time, when the environment looks
   up a value, concrete predicates are resolved to dictionary names
   (e.g., `<Eq Int>`) that are bound in the environment's value map.

## Key source files

| File | Role |
|------|------|
| `leander-core-type_classes.ads/adb` | Class definition, entailment, HNF, simplification |
| `leander-core-type_instances.ads` | Instance records (qualifier + predicate) |
| `leander-core-predicates.ads` | Predicate representation (class name + type) |
| `leander-environment.adb` | Selector construction, dictionary compilation, value lookup with predicate wrapping |
| `leander-core-expressions.adb` | Use-site compilation (EVar predicate application) |
| `leander-syntax-classes.ads/adb` | Builder for parsing class declarations |
| `leander-parser-declarations.adb` | Parsing class and instance declarations |
| `leander-core-binding_groups-inference.adb` | Inference for instance binding groups |
