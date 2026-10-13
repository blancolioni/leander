# IO in the Lambda Calculus

This document describes how Leander implements `IO a` — Haskell's
type for side-effecting computations — using world-passing style
compiled through the same lambda calculus and SKI machine as all other
Haskell code. There is no special runtime support for IO; it is an
ordinary algebraic data type whose effects happen through foreign
primitives.

## The IO type

IO is defined in the Prelude as a regular data type:

```haskell
data IO a = IO (Int -> IOResult a)

data IOResult a = IOSuccess a Int | IOFailure IOError Int
```

An `IO a` value wraps a function from a world token (an `Int`) to a
result: either success, with a value, or failure, with an `IOError`.
Both carry a new world token. The world token is threaded through IO
actions to impose sequencing — each action receives the token produced
by the previous action. A failure hands its token on too, so the
effects that ran before it stay in order with whatever handles it.

`unIO (IO f) = f` unwraps an action.

### Scott encoding

`IO` has one constructor with one field, so the data type builder
produces:

    IO = \$a.\x. $a x

This is the standard Scott encoding for a single-constructor,
single-field type (see structured-types.md). Pattern matching on `IO`
applies the value to a continuation that receives the wrapped
function.

## Core operations

### return

```haskell
returnIO x = IO (\w -> IOSuccess x w)
```

Wraps a pure value in IO. The world token is passed through
unchanged.

### bind (>>=)

```haskell
bindIO a g = IO (\w -> case unIO a w of
                         IOSuccess x w' -> seq w' (unIO (g x) w')
                         IOFailure e w' -> IOFailure e w')
```

Sequences two IO actions:

1. Runs `a` with the current world token `w`
2. If it succeeded, applies the continuation `g` to its result `x`,
   and runs the action that produces with the new world token `w'`
3. If it failed, the failure is the result, and `g` never runs

The world token threading ensures that `a` runs before `g` — the
second action cannot proceed without the world token produced by the
first.

`seq w'` makes `a`'s effects happen *now*, before `g` is even applied.
Without it they would wait until `runIO` forced the last world token,
after the whole program's actions had been worked out. Then a later
action that failed while being worked out, like `print (head [])`,
would stop the program before any earlier output appeared (#151).
Forcing `w'` is cheap: the actions before `a` have already forced
theirs, so it runs only `a`'s own effects.

### (>>)

```haskell
(>>) a b = a >>= \x -> b
```

Sequences two actions, discarding the result of the first.

## Errors

An `IOError` is abstract, as in the Report: `userError` makes one, and
`show` describes it.

```haskell
ioError :: IOError -> IO a
ioError e = IO (\w -> IOFailure e w)

catch :: IO a -> (IOError -> IO a) -> IO a
catch a h = IO (\w -> case unIO a w of
                         IOFailure e w' -> seq w' (unIO (h e) w')
                         r              -> r)
```

`catch` is the Haskell 98 Prelude's. It handles the `IOError`s an
action raises, and nothing else: `error` and a failed pattern match are
bottom, and still end the program. The handler runs with the world
token the failure carried, after every effect that came before it.

The only kind of `IOError` so far is a user error. A primitive that
can fail, such as a future `getChar` at end of file, will return a
sentinel, and its Haskell wrapper will turn that into an `IOFailure`.

## Running IO

The driver (`leander-driver.adb`) executes an IO program by
evaluating `runIO main`:

```haskell
runIO :: IO a -> a
runIO a = case unIO a 1 of
            IOSuccess x w -> seq w x
            IOFailure e w -> seq w (error (show e))
```

This:

1. Applies the action's function to `1` as the initial world token
2. Forces evaluation of the final world token `w` using `seq`
   — this is critical, as it ensures all side effects actually execute
3. Returns the result value `x`, or, for an `IOError` nothing caught,
   raises it as a runtime error after the effects before it

The initial world token is `1`. Each `#primPutChar` call increments
it, producing a chain of distinct tokens that forces sequential
evaluation.

## Side effects: putChar

The only primitive IO operation is character output:

```haskell
putChar :: Char -> IO ()
putChar ch = IO (\w -> IOSuccess () (#primPutChar w 1 ch))
```

This constructs an IO action whose wrapped function:

1. Receives the current world token `w`
2. Calls the foreign primitive `#primPutChar w 1 ch`, which
   performs the actual output and returns `w + 1`
3. Succeeds with `()`, and the incremented world token

### The #primPutChar primitive

Declared as a foreign import:

```haskell
foreign import skit "#putChar" #primPutChar :: Int -> Int -> Char -> Int
```

Takes three arguments: world token, stream index, and character.
In the SKI machine, this is implemented by `Put_Char_Instance`
(`skit-library.adb`):

1. Pops three arguments from the stack: world, stream, character
2. Retrieves the IO interface object from the skit environment
3. Calls `Ada.Wide_Wide_Text_IO.Put` with the character
4. Pushes `world + 1` as the result — the incremented world token

The stream index (always `1` in current code) is available for future
use (e.g., stderr).

## Higher-level IO operations

All other IO operations are built from `putChar`, `return`, and
`(>>=)`:

```haskell
putStr :: [Char] -> IO ()
putStr = mapM_ putChar

putStrLn :: [Char] -> IO ()
putStrLn str = putStr str >> putChar '\n'

print :: Show a => a -> IO ()
print x = putStrLn (show x)
```

Sequencing combinators:

```haskell
sequence :: [IO a] -> IO [a]
sequence = let mcons p q = p >>= \x -> q >>= \y -> return (x:y)
           in foldr mcons (return [])

sequence_ :: [IO a] -> IO ()
sequence_ = foldr (>>) (return ())

mapM :: (a -> IO b) -> [a] -> IO [b]
mapM f as = sequence (map f as)

mapM_ :: (a -> IO b) -> [a] -> IO ()
mapM_ f as = sequence_ (map f as)
```

## The role of seq

The `seq` primitive (foreign import `#seq`) is essential for IO
correctness. In `runIO`:

```haskell
seq w x
```

Without `seq`, lazy evaluation could return `x` without ever
evaluating `w` — and since the world token threading is what
drives side effects, the program's output would never happen. `seq`
forces evaluation of the final world token. Each `bindIO` has already
forced the tokens before it, as its action ran, so by now this only
runs the last action's own effects.

In the SKI machine, `#seq` is implemented as the `Sequence`
primitive. It evaluates its first argument to weak head normal form,
then returns its second argument.

## Compilation example

A program:

```haskell
main :: IO ()
main = putStrLn "Hi"
```

The driver evaluates `runIO main`, which expands roughly as:

```
runIO (putStrLn "Hi")
= runIO (putStr "Hi" >> putChar '\n')
= runIO (mapM_ putChar ['H','i'] >> putChar '\n')
```

Each `putChar` produces an IO action wrapping a function that calls
`#primPutChar` and threads the world token. The `>>=` chain threads
tokens sequentially: world `1` flows into `putChar 'H'`, producing
world `2`, which flows into `putChar 'i'`, producing world `3`, which
flows into `putChar '\n'`, producing world `4`. Finally `runIO`
forces world `4` via `seq`, ensuring all three characters are output.

## Limitations

- **Output only** — the current implementation has `#primPutChar` as
  the sole IO primitive. There is no input (`getChar`, `getLine`,
  `readFile`, etc.).
- **IO errors only** — `catch` handles `IOError`s, not `error` or a
  failed match, which would need exceptions in the Skit machine.
- **Single stream** — the stream index argument to `#primPutChar` is
  always `1`; multiple output streams are not yet used.
