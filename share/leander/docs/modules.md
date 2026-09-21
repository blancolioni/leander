# Modules

This document describes how Leander resolves module names, decides what a
module may write, and keeps two modules' declarations apart.

## Overview

A module boundary does two jobs, and Leander keeps them separate on
purpose:

- **Scope** — what the programmer of a module is allowed to *name*. This
  is decided while parsing, by import and export declarations.
- **Linkage** — what a compiled binding can *reach*. This is decided by
  what is in the environment, and is deliberately wider.

Keeping them apart is not a stylistic choice. `Con_Data_Type`,
`Data_Type`, `Constructor` and `Get_Class` read their maps with `Element`
and have no fallback to imports, so removing an entry produces
`Constraint_Error` rather than a diagnostic. Nothing is ever removed from
an environment. A name a module may not write is one that was never
inserted under the name it would write.

## Canonical names

A module's own declarations keep their bare names *inside that module*.
They are renamed when another module imports them:

| Written in `Shapes` | Key in `Shapes` | Key in a module that imports it |
|---|---|---|
| `area` | `area` | `Shapes.area` |
| `Square` | `Square` | `Shapes.Square` |

Three kinds of name are never prefixed:

- **The Prelude.** Its prefix is the empty string, so `map` is `map`
  everywhere. This keeps every hardcoded literal in the Ada source
  working — `"Y"`, `"#add"`, `"Bool"`, `"True"` — and keeps existing
  `.skix` images valid.
- **Built-in syntax**: `[]`, `:`, `()`, `(,)`, `->`. The parser emits
  these as literals for every list and tuple form, so they are in scope
  everywhere and no export list mentions them. Real Haskell's Prelude
  does not list them either, for the same reason.
- **Synthetic names**: anything starting `#`, `<`, `$`, or `default:`.
  `Skit.Compiler.Abstract_Variable` matches dictionary lambdas *by
  string*, so binder and reference must stay byte-identical.

Renaming at the import boundary rather than at the declaration site is
what avoids a forward-reference problem. At a use site, a top-level name
this module declares, a lambda binding, and the left-hand side of a
declaration are indistinguishable — a binding's left-hand side is parsed
by `Parse_Atomic_Pattern`, which is `Parse_Atomic_Expression` plus
`To_Pattern`. Rewriting names at use sites therefore rewrites binders too.
Since a module's own names never change, the question never arises.

The consequence is that two modules loaded into one handle still share a
namespace for their own top-level names. Fixing that means resolving names
after the whole module is parsed, which is a renamer rather than a scope
table.

## Import resolution

`Load_Module_By_Name` maps dots to directories, so `Data.List` is
`Data/List.hs`, and searches:

1. the importing file's own directory,
2. each `-i DIR` / `--include=DIR` directory, in the order given,
3. the installed module directory.

A module is named by its own header, not by its file name; the file base
name only has to match the header's last component, and a mismatch is
reported rather than refused. The module cache is keyed by dotted module
name, with a second cache keyed by full path so that loading the same file
twice does not reopen it merely to re-read its header.

A module partway through its own parse is recorded, so an import cycle is
a diagnostic instead of a recursion down to GCS's 500-open-file limit.
`Load_Module_By_Name` returns null for both a cycle and a missing module
and reports neither: the useful source location belongs to the import
declaration that asked, so the caller reports.

Loading one module partway through another is safe — the lexer keeps its
token state per open file — but `Parse_Context` has a single mutable
environment, so it is saved and restored around each nested load.

## What an import brings in

`Environment.Import` always inserts a module's own declarations under
their canonical names, so a qualifier reaches them whatever the import
says. It inserts them under their *bare* names as well only when the
declaration makes them visible:

| Declaration | Bare names inserted |
|---|---|
| `import M` | all of M's exports |
| `import M (a, b)` | `a` and `b` only |
| `import M hiding (a)` | all but `a` |
| `import qualified M` | none |

`T(..)` in an import list is expanded once the module is loaded — only
the exporting module can say what its constructors are.

`qualified`, `hiding` and `as` are contextual identifiers, not tokens.
`Leander.Parser.Lexical`'s `Keywords` string is positional against
`First_Keyword`, so adding a keyword silently shifts every later one. Each
is matched on `Tok_Text` and only when what follows it fits — `qualified`
needs a module name after it, `hiding` needs a `(` — which is Haskell's
own rule and leaves a module genuinely called `qualified` working.

## Qualified names

`Scan_Qualified_Name` assembles a maximal run of adjacent components. It
has to consume before it can classify: `A.B.c` is five tokens against
three of usable lookahead, and `At_Constructor` would read only the first
component. Three details matter:

- A dot joins the run only when written tight against a module-shaped
  component either side. That is Haskell's rule, and it is what leaves
  both `f . g` and `f.g` as composition.
- Adjacency is judged on `Tok_Column + Tok_Text'Length`, because
  `Tok_Info.Finish` is set to 1 when a file is opened and never updated
  per token.
- The run is gated on `Tok_Identifier`, never on token text, which keeps
  `[A..B]` safe: `..` lexes as `Tok_Dot_Dot`.

The lexer splits an identifier when the character group changes, so `M.+`
arrives as `M` then `.+` — one token holding the dot and the operator
together. That shape is handled, which is why `(M.+)` works. Qualified
operators in *infix* position are not supported: `At_Operator` tests the
current token for a symbolic identifier, and in `a M.+ b` that token is
`M`.

A qualified type name resolves to a key for the visibility check, but what
reaches the type expression is the data type's own id. A `TCon` is
identified by name, and the exporting module built its constructors'
schemes against the unprefixed one, so emitting the key would leave
`Shapes.Shape` refusing to unify with `Shape`.

## Export lists

A module with no export list exports everything it declares, which is not
the same as a module with an empty one — `Exports_All` records the
difference. `Is_Exported` settles two cases before consulting any list:

- **never** a `#`-prefixed name, which is an FFI symbol rather than a
  Haskell entity and stays private however the list reads;
- **always** built-in syntax and the synthetic names dictionary passing
  resolves by string.

The list is applied *after* the declarations are parsed: `T(..)` cannot
name its constructors, and an export naming something the module never
declared cannot be spotted, any earlier. By then the parser sits on
`Tok_End_Of_File`, whose column is 0 and outside `Column_Number`, so
`Lexical.Error` raises there — diagnostics of this shape go through
`Leander.Parser.Report` against a location captured when the construct was
written.

The Prelude carries a list of its own, machine-checked by
`Leander.Tests.Modules`: every name it declares that is not `#`-prefixed
and not in an explicit withheld set must be exported, with the missing
ones named in the failure. Adding to the withheld set is how to keep
something back on purpose.

## Deliberate deviations

**`Dump_Module` emits the link surface, not the export list.** An image's
export set is deliberately not filtered through `Is_Exported`. In the
full-coverage path the module's source is never opened, so its environment
has `Bindings = null` and no `Values`, and `Skit_Handle.Lookup` — primed
from exactly these names — is the only way anything in the image can be
reached. An export list says what another module may write; it has no
business deciding what the loader can find.

**Fixities and type synonyms stay process-global.** `Leander.Parser.Expressions`'s fixity map and `Leander.Core.Type_Synonyms`' table are
package-level state, because expansion happens inside `Syntax.Types.To_Core`, which has no environment to consult. So `infixl 6 <+>` and
`type String = [Char]` are visible in every module regardless of any
import or export list, and `hiding` will not affect parsing. Scoping them
is a separate piece of work. Any import-carried fixity must go through
`Leander.Parser.Add_Fixity` → `Set_Fixity`, never
`Expressions.Add_Fixity`, which calls `Warning` and raises outside a lexer
session.

**Inherited names travel one import further than Haskell would
re-export.** `Import` applies a module's export list to its own
declarations only; what that module itself inherited passes through.
Harmless while every module imports the Prelude for itself.

**The implicit Prelude import is unconditional.** It is a synthesised
`import Prelude` rather than a special case, but it is not suppressed when
a module writes its own, so `import Prelude ()` cannot restrict it.

## Known limits

- A module's `.skix` embeds everything reachable from its exports by graph
  reachability, including the Prelude cells it uses. Not a regression, but
  a visible size problem now that any module can be dumped.
- `Max_Source_Files` is 10,000 and `Next_File` increments on every `Open`,
  including `Open_String`. Recursive loading consumes slots faster than
  before.
- Staleness is decided by mtime alone, so editing a module's export list
  against a newer-but-stale `.skix` loads the old set. No `.skix` is
  committed.

## Key source files

| File | Role |
|------|------|
| `leander-parser.ads/adb` | Module resolution, include paths, the module cache, cycle detection, `Scan_Qualified_Name` |
| `leander-parser-modules.adb` | Export-list and `import` grammar, contextual keywords, applying an export list |
| `leander-syntax-modules.ads/adb` | Builders holding a parsed export list and import declaration |
| `leander-scopes.ads/adb` | Per-module alias table; resolving a qualified name to a canonical one |
| `leander-environment.adb` | `Import` (canonical and bare keys, visibility, export filtering), `Is_Exported`, `Declares`, `Local_Name` |
| `leander-core-type_env.ads/adb` | `Iterate` and `Builder.Insert`, used to lay a restricted import down as a single link |
| `leander-handles.adb` | The image's export set and the `__leander_meta__` payload carrying export state |
| `leander-errors.ads/adb` | Whether anything has been reported, for tests that assert a module is rejected |
