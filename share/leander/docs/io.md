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
data IO a = IO (Int -> (a, Int))
```

An `IO a` value wraps a function from a world token (an `Int`) to a
pair of a result value and a new world token. The world token is
threaded through IO actions to impose sequencing — each action
receives the token produced by the previous action.

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
returnIO x = IO (\w -> (x, w))
```

Wraps a pure value in IO. The world token is passed through
unchanged, and the result is paired with it using the `(,)`
constructor.

### bind (>>=)

```haskell
bindIO (IO f) g = IO (\w -> let xw' = f w
                                 ioh = g (fst xw')
                                 geth (IO h) = h
                             in geth ioh (snd xw'))
```

Sequences two IO actions:

1. Runs `f` with the current world token `w`, producing a pair
   `xw'` of `(result, new_world)`
2. Applies the continuation `g` to the result (`fst xw'`),
   producing a new `IO` action `ioh`
3. Extracts the wrapped function from `ioh` via pattern match
   (`geth`)
4. Applies it to the new world token (`snd xw'`)

The world token threading ensures that `f` runs before `g` — the
second action cannot proceed without the world token produced by the
first.

### (>>)

```haskell
(>>) a b = a >>= \x -> b
```

Sequences two actions, discarding the result of the first.

## Running IO

The driver (`leander-driver.adb`) executes an IO program by
evaluating `runIO main`:

```haskell
runIO :: IO a -> a
runIO a = let getf (IO f) = f
              xw' = getf a 1
          in seq (snd xw') (fst xw')
```

This:

1. Extracts the wrapped function from the IO action via `getf`
2. Applies it to `1` as the initial world token
3. Forces evaluation of the final world token (`snd xw'`) using `seq`
   — this is critical, as it ensures all side effects actually execute
4. Returns the result value (`fst xw'`)

The initial world token is `1`. Each `#primPutChar` call increments
it, producing a chain of distinct tokens that forces sequential
evaluation.

## Side effects: putChar

The only primitive IO operation is character output:

```haskell
putChar :: Char -> IO ()
putChar ch = IO (\w -> ((), #primPutChar w 1 ch))
```

This constructs an IO action whose wrapped function:

1. Receives the current world token `w`
2. Calls the foreign primitive `#primPutChar w 1 ch`, which
   performs the actual output and returns `w + 1`
3. Returns `((), new_world)` — unit result paired with the
   incremented world token

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
seq (snd xw') (fst xw')
```

Without `seq`, lazy evaluation could return `fst xw'` without ever
evaluating `snd xw'` — and since the world token threading is what
drives side effects, the program's output would never happen. `seq`
forces evaluation of the final world token, which transitively forces
all the intermediate world tokens and their associated effects.

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
- **No IO exceptions** — there is no mechanism for IO errors or
  exception handling.
- **Single stream** — the stream index argument to `#primPutChar` is
  always `1`; multiple output streams are not yet used.
