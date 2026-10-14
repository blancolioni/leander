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

Character output writes to a stream by number (see *Input and files*
below); `putChar` writes to standard output, `1`:

```haskell
putChar :: Char -> IO ()
putChar ch = hPutChar 1 ch

hPutChar h ch = IO (\w -> IOSuccess () (#primPutChar w h ch))
```

This constructs an IO action whose wrapped function:

1. Receives the current world token `w`
2. Calls the foreign primitive `#primPutChar w h ch`, which
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
3. Writes the character to that stream through the handle's
   `Leander.IO`: standard output, standard error, or an open file. A
   line feed is written with `New_Line`, so that Text_IO counts the line
   as ended and does not end it again when the stream is closed.
4. Pushes `world + 1` as the result — the incremented world token

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

## Input and files

Every IO primitive takes the world token first, strictly, so that it
happens after the effects before it, and names a stream by number: `0`
is standard input, `1` standard output, `2` standard error, and an opened
file has a number from `3` up. On the Ada side the numbers are
`Leander.IO`'s; a test can supply its own implementation, to stand in for
the terminal (`Handles.Set_IO`).

| Primitive | Does |
|---|---|
| `#putChar w h c` | write `c` to `h`; `w + 1` |
| `#getChar w h` | the next character's code from `h`, or `-1` at its end |
| `#pathChar c n` | add `c` to the path for the next open, a new one when `n` is `0`; `n + 1` |
| `#openFile w mode` | open that path for reading (`0`), writing (`1`) or appending (`2`): the handle, or `-1` for no such file, `-2` for no permission |
| `#closeFile w h` | close `h`; `w + 1` |

A path reaches the machine a character at a time, as `error`'s message
does, because a Haskell string handed to Ada whole would not be forced.

```haskell
hGetCharMaybe h = IO (\w -> let c = #primGetChar w h
                            in seq c (IOSuccess (if c < 0 then Nothing
                                                 else Just (toEnum c))
                                                (w + 1)))
```

The result is only built once the character is read, and `bindIO` looks
at the result straight away, so the read happens before anything that
follows. `getChar`, `getLine`, `readLn` and the rest are built from it.
`getChar` at the end of input fails with an `IOError`
(`Prelude.getChar: end of file`). A carriage return is dropped as it is
read, so that a line ends with `\n` whatever wrote it.

### Lazy input

`getContents`, `interact` and `readFile` read their input as it is used.
That needs one action whose effects are out of sequence with the rest:

```haskell
unsafeInterleaveIO a = IO (\w -> IOSuccess (case unIO a w of
                                               IOSuccess x _ -> x
                                               IOFailure e _ -> error (show e))
                                           w)

lazyRead h = unsafeInterleaveIO
               (hGetCharMaybe h >>= \m -> case m of
                  Nothing -> (if h == 0 then return () else hClose h)
                               >> return []
                  Just c  -> lazyRead h >>= \cs -> return (c : cs))
```

Each cell of the string is an action run only when the cell is wanted,
and the machine overwrites a primitive's application with its result, so
each character is read once however often it is used. A file is closed
when its end is reached. `interact f` writes each character of `f`'s
result as soon as the input it depends on has been read, which needs each
action's effects to happen as it runs (see *bind* above).

The usual surprises of lazy input apply: `readFile f >>= writeFile f`
empties `f` before reading it.

## Limitations

- **IO errors only** — `catch` handles `IOError`s, not `error` or a
  failed match, which would need exceptions in the Skit machine.
- **No `System.IO`** — handles are plain numbers inside the Prelude;
  `Handle`, `hGetLine`, `hClose`, `isEOF`, `stdin` and the rest are not
  exported.
