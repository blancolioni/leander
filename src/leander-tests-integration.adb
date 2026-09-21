with Ada.Exceptions;
with Leander.Handles;
with Leander.Syntax;
with Skit;

package body Leander.Tests.Integration is

   Test_Root : constant String :=
     "./share/leander/tests/integration/";

   Modules_Root : constant String := Test_Root & "modules/";
   --  The multi-module fixtures live in their own directory because an
   --  import resolves against the importing file's directory first, which
   --  needs a directory that is not shared with unrelated fixtures.

   procedure Test_Eval
     (Expression     : String;
      Expected_Type  : String;
      Expected_Value : String;
      Handle         : Leander.Handle);

   procedure Test_Module
     (Label          : String;
      Module_Path    : String;
      Expression     : String;
      Expected_Value : String;
      Handle         : Leander.Handle);

   ---------------
   -- Test_Eval --
   ---------------

   procedure Test_Eval
     (Expression     : String;
      Expected_Type  : String;
      Expected_Value : String;
      Handle         : Leander.Handle)
   is
      pragma Unreferenced (Expected_Type);
      Value : constant String := Handle.Evaluate (Expression);
   begin
      Test (Expression,
            Expected_Value,
            Value);
   exception
      when E : others =>
         Error (Expression,
                Ada.Exceptions.Exception_Message (E));
   end Test_Eval;

   -----------------
   -- Test_Module --
   -----------------

   procedure Test_Module
     (Label          : String;
      Module_Path    : String;
      Expression     : String;
      Expected_Value : String;
      Handle         : Leander.Handle)
   is
   begin
      Handle.Load_Module (Module_Path);
      declare
         Value : constant String :=
                   Handle.Evaluate (Expression);
      begin
         Test (Label,
               Expected_Value, Value);
      end;
   exception
      when E : others =>
         Error (Label,
                Ada.Exceptions.Exception_Message (E));
   end Test_Module;

   ---------------
   -- Test_Main --
   ---------------

   procedure Test_Main
     (Label       : String;
      Module_Path : String)
   is
      H      : Leander.Handle :=
                 Leander.Create (256 * 1024);
   begin
      H.Load_Module (Module_Path);
      declare
         Result : constant String :=
                    H.Evaluate ("runIO main");
      begin
         if Result /= "I" then
            Fail (Label, "I", Result);
         else
            Test (Label, Pass => True);
         end if;
      end;
      H.Close;
   exception
      when E : others =>
         Error (Label,
                Ada.Exceptions.Exception_Message (E));
   end Test_Main;

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
      Handle : Leander.Handle := Leander.Create (256 * 1024);
   begin

      --  Phase 1: Expression-level tests
      --  Type class method dispatch (Eq)

      Test_Eval ("1 == 1", "Bool", "K",
                 Handle);
      Test_Eval ("1 == 2", "Bool", "K I",
                 Handle);
      Test_Eval ("True == True", "Bool", "K",
                 Handle);
      Test_Eval ("True == False", "Bool", "K I",
                 Handle);

      --  Let expressions

      Test_Eval ("let x = 3 in x + 1", "Int", "4",
                 Handle);

      --  Case expressions on Int

      Test_Eval
        ("case 3 of { 1 -> 10; 2 -> 20; _ -> 30 }",
         "Int", "30",
         Handle);
      Test_Eval
        ("case 2 of { 1 -> 10; 2 -> 20; _ -> 30 }",
         "Int", "20",
         Handle);

      --  A catch-all alternative fills every constructor slot that no
      --  alternative names, and the Scott encoding applies each branch to
      --  that constructor's fields.  The filler must therefore bind exactly
      --  as many as the slot's constructor takes: none for a nullary slot,
      --  and for a named catch-all over a slot with fields, the whole
      --  scrutinee rebuilt from them.

      Test_Eval
        ("case True of { False -> 1; _ -> 2 }",
         "Int", "2",
         Handle);
      Test_Eval
        ("case [] of { (x:xs) -> 1; y -> 2 }",
         "Int", "2",
         Handle);
      Test_Eval
        ("(case [] of { (x:xs) -> 1; y -> 2 }) + 40",
         "Int", "42",
         Handle);
      Test_Eval
        ("case [1,2] of { [] -> 0; ys -> length ys }",
         "Int", "2",
         Handle);
      Test_Eval
        ("case (Just 3) of { Nothing -> 1; y -> 2 }",
         "Int", "2",
         Handle);

      --  If-then-else

      Test_Eval ("if True then 5 else 10",
                 "Int", "5",
                 Handle);
      Test_Eval ("if False then 5 else 10",
                 "Int", "10",
                 Handle);

      --  Higher-order with operator sections

      Test_Eval ("foldr (+) 0 [1,2,3]",
                 "Int", "6",
                 Handle);

      --  List operations

      Test_Eval
        ("sum (concat [[1,2],[3,4]])",
         "Int", "10",
         Handle);
      Test_Eval
        ("sum ([1,2] ++ [3,4])",
         "Int", "10",
         Handle);

      --  Equality with tuple projections

      Test_Eval ("fst (3,4) == 3", "Bool", "K",
                 Handle);
      Test_Eval ("snd (3,4) == 4", "Bool", "K",
                 Handle);

      --  uncurry

      Test_Eval ("uncurry (+) (3,4)",
                 "Int", "7",
                 Handle);

      --  A do-block pattern bind whose continuation is left polymorphic.
      --  The desugaring binds the pattern in a synthetic binding group;
      --  generalising that group used to freshen the continuation's type
      --  variables away from the bind site, stranding its Monad predicate
      --  as an unresolvable <Monad $t> (issue #70).

      Test_Eval
        ("(do { Just x <- Just (Just 1); return x }) == Just 1",
         "Bool", "K",
         Handle);

      --  The same change must not stop an ordinary source-level 'let' from
      --  generalising: i is used here at both Bool and Int.

      Test_Eval ("let i x = x in if i True then i 1 else 2",
                 "Int", "1",
                 Handle);

      --  A local binding whose body raises a class constraint.  Its scheme
      --  has to carry the predicate so the use site can hand it a
      --  dictionary, and the binding has to be elaborated with a matching
      --  dictionary parameter; without both, the constraint was stranded on
      --  a type variable nothing resolved and the expression quietly
      --  evaluated to garbage.

      Test_Eval ("let eq2 x y = x == y in eq2 1 1",
                 "Bool", "K",
                 Handle);
      Test_Eval ("let eq2 x y = x == y in eq2 1 2",
                 "Bool", "K I",
                 Handle);

      --  The same, for a class with no defaulting to fall back on: the
      --  monad is fixed only by the use site.

      Test_Eval ("let ret x = return x in ret 1 == Just 1",
                 "Bool", "K",
                 Handle);

      --  seq forcing a value whose Scott encoding is a bare, wholly
      --  unapplied combinator (unit is exactly I).  Regression: the
      --  machine used to strand that head instead of pushing it, so the
      --  pending #primSeq call read past its own arguments.

      Test_Eval ("seq () (1 + 2)",
                 "Int", "3",
                 Handle);

      --  A dot only joins a qualified name when it is written tight
      --  against a module-shaped component either side, so composition
      --  keeps working whether or not it is spaced.
      Test_Eval ("(negate . negate) 5",
                 "Int", "5",
                 Handle);
      Test_Eval ("(negate.negate) 5",
                 "Int", "5",
                 Handle);

      --  Phase 2: Module tests (non-IO)
      --  Simple function definition

      Test_Module
        ("module: function",
         Test_Root & "test_01_function.hs",
         "double 21", "42",
         Handle);

      --  Bool pattern matching in module

      Test_Module
        ("module: bool pattern",
         Test_Root & "test_02_bool_pattern.hs",
         "toggle True", "K I",
         Handle);

      --  Local data type definition

      Test_Module
        ("module: data type",
         Test_Root & "test_03_data_type.hs",
         "colorNum Green", "2",
         Handle);

      --  Type class usage in module

      --  newtype: a value of a newtype IS the value of its field, with no
      --  wrapper of any kind, so `Age 65` must evaluate to the bare integer
      --  65 rather than to a Scott-encoded closure over it (issue #52).

      Test_Module
        ("module: newtype value has no wrapper",
         Test_Root & "test_18_newtype.hs",
         "bare", "65",
         Handle);
      Test_Module
        ("module: newtype field pattern binds the value itself",
         Test_Root & "test_18_newtype.hs",
         "unwrapped", "65",
         Handle);
      Test_Module
        ("module: newtype rewrapped in a case alternative",
         Test_Root & "test_18_newtype.hs",
         "bumped", "66",
         Handle);
      Test_Module
        ("module: parametric newtype nested over another newtype",
         Test_Root & "test_18_newtype.hs",
         "nested", "7",
         Handle);
      Test_Module
        ("module: newtype patterns in several argument positions",
         Test_Root & "test_18_newtype.hs",
         "compared", "K",
         Handle);
      Test_Module
        ("module: newtype over a structured field",
         Test_Root & "test_18_newtype.hs",
         "inTuple", "3",
         Handle);
      Test_Module
        ("module: deriving Eq on a newtype",
         Test_Root & "test_18_newtype.hs",
         "derivedEq", "K",
         Handle);
      Test_Module
        ("module: deriving Eq on a newtype, unequal",
         Test_Root & "test_18_newtype.hs",
         "derivedNe", "K I",
         Handle);
      Test_Module
        ("module: a class instance for a newtype",
         Test_Root & "test_18_newtype.hs",
         "viaClass", "9",
         Handle);
      Test_Module
        ("module: newtype over a function field",
         Test_Root & "test_18_newtype.hs",
         "funcField", "42",
         Handle);

      --  Type synonyms (issue #51).  A synonym is a compile-time rewrite
      --  with no runtime trace, so every one of these must behave exactly
      --  as the type it stands for.

      Test_Module
        ("module: synonym is transparent to the type it names",
         Test_Root & "test_19_type_synonym.hs",
         "viaSynonym", "42",
         Handle);
      Test_Module
        ("module: synonym with a parameter",
         Test_Root & "test_19_type_synonym.hs",
         "dupped", "14",
         Handle);

      --  Guards

      Test_Module
        ("module: a guard set ending in otherwise takes the first true arm",
         Test_Root & "test_20_guards.hs",
         "gdPosOne", "1",
         Handle);
      Test_Module
        ("module: a middle guard arm",
         Test_Root & "test_20_guards.hs",
         "gdNegOne", "-1",
         Handle);
      Test_Module
        ("module: the otherwise arm",
         Test_Root & "test_20_guards.hs",
         "gdZero", "0",
         Handle);
      Test_Module
        ("module: every guard failing continues at the next equation",
         Test_Root & "test_20_guards.hs",
         "gdZeroed", "2",
         Handle);
      Test_Module
        ("module: a guard succeeding stops before the next equation",
         Test_Root & "test_20_guards.hs",
         "gdBig", "1",
         Handle);
      Test_Module
        ("module: falling through two equations to the catch-all",
         Test_Root & "test_20_guards.hs",
         "gdSmall", "3",
         Handle);
      Test_Module
        ("module: a guard on a constructor pattern succeeds",
         Test_Root & "test_20_guards.hs",
         "gdSomeBig", "9",
         Handle);
      Test_Module
        ("module: a guard on a constructor pattern falls through",
         Test_Root & "test_20_guards.hs",
         "gdSomeSmall", "0",
         Handle);
      Test_Module
        ("module: a constructor that matches no alternative still falls through",
         Test_Root & "test_20_guards.hs",
         "gdNoneAtAll", "0",
         Handle);
      Test_Module
        ("module: comma-separated guards conjoin",
         Test_Root & "test_20_guards.hs",
         "gdInRange", "1",
         Handle);
      Test_Module
        ("module: a leading comma guard failing falls through",
         Test_Root & "test_20_guards.hs",
         "gdTooSmall", "0",
         Handle);
      Test_Module
        ("module: a trailing comma guard failing falls through",
         Test_Root & "test_20_guards.hs",
         "gdTooBig", "0",
         Handle);
      Test_Module
        ("module: a staged guarded function can still recurse",
         Test_Root & "test_20_guards.hs",
         "gdCounted", "42",
         Handle);
      Test_Module
        ("module: a guard on a multi-argument function",
         Test_Root & "test_20_guards.hs",
         "gdFirstBigger", "1",
         Handle);
      Test_Module
        ("module: a multi-argument guard falling to a literal equation",
         Test_Root & "test_20_guards.hs",
         "gdFirstZero", "2",
         Handle);
      Test_Module
        ("module: a multi-argument guard falling to the catch-all",
         Test_Root & "test_20_guards.hs",
         "gdNeither", "3",
         Handle);
      Test_Module
        ("module: a guard on a case alternative",
         Test_Root & "test_20_guards.hs",
         "gdOver10", "1",
         Handle);
      Test_Module
        ("module: a second guard arm on a case alternative",
         Test_Root & "test_20_guards.hs",
         "gdOver5", "2",
         Handle);
      Test_Module
        ("module: a case alternative falling through its guards",
         Test_Root & "test_20_guards.hs",
         "gdIsZero", "3",
         Handle);
      Test_Module
        ("module: a case alternative falling through to the catch-all",
         Test_Root & "test_20_guards.hs",
         "gdOther", "4",
         Handle);
      Test_Module
        ("module: a class-constrained guard keeps its dictionary",
         Test_Root & "test_20_guards.hs",
         "gdOrdBigger", "1",
         Handle);
      Test_Module
        ("module: a class-constrained guard falls through",
         Test_Root & "test_20_guards.hs",
         "gdOrdSmaller", "0",
         Handle);
      Test_Module
        ("module: the same guarded function at another instance",
         Test_Root & "test_20_guards.hs",
         "gdOrdChar", "1",
         Handle);
      Test_Module
        ("module: a guard reads a where binding",
         Test_Root & "test_20_guards.hs",
         "gdSteppedBig", "18",
         Handle);
      Test_Module
        ("module: a where binding is shared with the otherwise arm",
         Test_Root & "test_20_guards.hs",
         "gdSteppedSmall", "0",
         Handle);

      Test_Module
        ("module: synonym with two parameters",
         Test_Root & "test_19_type_synonym.hs",
         "keyed", "3",
         Handle);
      Test_Module
        ("module: synonym defined in terms of another synonym",
         Test_Root & "test_19_type_synonym.hs",
         "swapped", "2",
         Handle);
      Test_Module
        ("module: a synonym the Prelude declared (String)",
         Test_Root & "test_19_type_synonym.hs",
         "shouted", "3",
         Handle);
      Test_Module
        ("module: synonyms in data fields",
         Test_Root & "test_19_type_synonym.hs",
         "aged", "44",
         Handle);
      Test_Module
        ("module: synonym in a newtype field, class and instance head",
         Test_Root & "test_19_type_synonym.hs",
         "sized", "2",
         Handle);

      Test_Module
        ("module: type class",
         Test_Root & "test_04_type_class.hs",
         "same 3 3", "K",
         Handle);

      --  Derived Eq instances

      Test_Module
        ("module: deriving Eq nullary match",
         Test_Root & "test_16_deriving_eq.hs",
         "colorEq", "K",
         Handle);
      Test_Module
        ("module: deriving Eq nullary mismatch",
         Test_Root & "test_16_deriving_eq.hs",
         "colorNe", "K I",
         Handle);
      Test_Module
        ("module: deriving Eq nullary /=",
         Test_Root & "test_16_deriving_eq.hs",
         "colorBl", "K",
         Handle);
      Test_Module
        ("module: deriving Eq args match",
         Test_Root & "test_16_deriving_eq.hs",
         "pairEq", "K",
         Handle);
      Test_Module
        ("module: deriving Eq args mismatch last",
         Test_Root & "test_16_deriving_eq.hs",
         "pairNe1", "K I",
         Handle);
      Test_Module
        ("module: deriving Eq args mismatch first",
         Test_Root & "test_16_deriving_eq.hs",
         "pairNe2", "K I",
         Handle);
      Test_Module
        ("module: deriving Eq mixed nullary",
         Test_Root & "test_16_deriving_eq.hs",
         "mixLL", "K",
         Handle);
      Test_Module
        ("module: deriving Eq mixed arity",
         Test_Root & "test_16_deriving_eq.hs",
         "mixRR", "K",
         Handle);
      Test_Module
        ("module: deriving Eq cross constructor",
         Test_Root & "test_16_deriving_eq.hs",
         "mixLR", "K I",
         Handle);
      Test_Module
        ("module: deriving Eq same constructor diff args",
         Test_Root & "test_16_deriving_eq.hs",
         "mixRdif", "K I",
         Handle);
      Test_Module
        ("module: deriving Eq recursive match",
         Test_Root & "test_16_deriving_eq.hs",
         "recEq", "K",
         Handle);
      Test_Module
        ("module: deriving Eq recursive mismatch",
         Test_Root & "test_16_deriving_eq.hs",
         "recNe", "K I",
         Handle);
      Test_Module
        ("module: deriving Eq recursive minimal",
         Test_Root & "test_16_deriving_eq.hs",
         "recNil", "K",
         Handle);

      --  A downstream module's own instance of a Prelude class (Eq),
      --  providing only "==" and relying on the class's default "/="
      --  (issue #65 follow-up: default methods compile once, generically,
      --  per class -- rather than being spliced into each instance's own
      --  AST -- so this works whether Prelude itself came from source or
      --  from a .skix image; see leander-tests-images.adb for the .skix
      --  case).

      Test_Module
        ("module: instance omits a defaulted method",
         Test_Root & "test_17_default_method.hs",
         "Foo 1 /= Foo 2", "K",
         Handle);
      Test_Module
        ("module: instance omits a defaulted method (equal)",
         Test_Root & "test_17_default_method.hs",
         "Foo 1 /= Foo 1", "K I",
         Handle);

      --  Phase 3: IO module tests
      --  Minimal IO

      Test_Module
        ("module: IO return",
         Test_Root & "test_05_io_return.hs",
         "runIO main", "I",
         Handle);

      --  IO with putChar

      Test_Module
        ("module: IO putChar",
         Test_Root & "test_06_io_putchar.hs",
         "runIO main", "I",
         Handle);

      --  IO with putStr

      Test_Module
        ("module: IO putStr",
         Test_Root & "test_07_io_putstr.hs",
         "runIO main", "I",
         Handle);

      --  Do notation

      Test_Module
        ("module: do notation",
         Test_Root & "test_08_do_notation.hs",
         "runIO main", "I",
         Handle);

      --  ($) and (++) operators

      Test_Module
        ("module: ($) and (++)",
         Test_Root & "test_09_dollar_concat.hs",
         "runIO main", "I",
         Handle);

      --  Tuples in IO context

      Test_Module
        ("module: tuples in IO",
         Test_Root & "test_10_tuples.hs",
         "runIO main", "I",
         Handle);

      --  mapM_ with list

      Test_Module
        ("module: mapM_",
         Test_Root & "test_11_mapM.hs",
         "runIO main", "I",
         Handle);

      --  Full test (near RunTests complexity)

      Test_Module
        ("module: full integration",
         Test_Root & "test_12_full.hs",
         "runIO main", "I",
         Handle);

      --  Phase 4: Handle API tests (--main path)
      --  These use the same code path as
      --  bin/leander --main=<file>

      Test_Main
        ("--main: IO return",
         Test_Root & "test_05_io_return.hs");
      Test_Main
        ("--main: do notation",
         Test_Root & "test_08_do_notation.hs");
      Test_Main
        ("--main: ($) and (++)",
         Test_Root & "test_09_dollar_concat.hs");
      Test_Main
        ("--main: mapM_",
         Test_Root & "test_11_mapM.hs");
      Test_Main
        ("--main: full integration",
         Test_Root & "test_12_full.hs");
      Test_Main
        ("--main: RunTests",
         "./share/leander/tests/RunTests.hs");

      --  Cross-module imports. UseShapes names Shapes, which sits beside
      --  it; UseData names Data.List, which is Data/List.hs under the same
      --  directory. Both exercise a whole module being loaded and imported
      --  partway through the importer's own parse.

      Test_Module
        ("module: a function imported from another module",
         Modules_Root & "UseShapes.hs",
         "area (Square 4)", "16",
         Handle);

      Test_Module
        ("module: a constructor imported from another module",
         Modules_Root & "UseShapes.hs",
         "area (Circle 2)", "12",
         Handle);

      Test_Module
        ("module: a dotted module name is imported from a subdirectory",
         Modules_Root & "UseData.hs",
         "doubled", "42",
         Handle);

      --  Every import shape the grammar accepts, in one module. "tally"
      --  comes in through the import list that names it; "area" is
      --  hidden and the qualified import contributes nothing unqualified,
      --  so this also checks that a restricted import still lets the
      --  module compile around what it did bring in.
      Test_Module
        ("module: qualified, aliased and selective imports all parse",
         Modules_Root & "AllForms.hs",
         "value", "6",
         Handle);

      Test_Module
        ("module: a qualified function name",
         Modules_Root & "Qualified.hs",
         "qualSquareArea", "25",
         Handle);

      Test_Module
        ("module: a qualified constructor and type name",
         Modules_Root & "Qualified.hs",
         "qualCircleArea", "27",
         Handle);

      --  A module's own declaration wins over an import of the same
      --  name, and the imported one is still reachable by qualifier --
      --  the two are under different keys once the import has renamed
      --  Tagged's own names.
      Test_Module
        ("module: a local declaration shadows an imported name",
         Modules_Root & "Shadowing.hs",
         "mine", "2",
         Handle);

      Test_Module
        ("module: the shadowed import is still reachable qualified",
         Modules_Root & "Shadowing.hs",
         "theirs", "1",
         Handle);

      Test_Module
        ("module: a qualified-only import is usable through its alias",
         Modules_Root & "Enforced.hs",
         "enforcedArea", "12",
         Handle);

      Test_Main
        ("--main: a module that imports another",
         Modules_Root & "UseShapes.hs");

      --  Minimal crash reproducer:
      --  a module-level binding that uses (==)
      --  causes a stack underflow in the SKI machine
      --  because the type class dictionary is not
      --  resolved during elaboration

      Test_Main
        ("--main: x = 1 == 1",
         Test_Root & "test_13_minimal_crash.hs");

      Test_Main
        ("--main: operator declaration",
         Test_Root & "Test14_Operators.hs");

      Test_Main
        ("--main: operator declaration",
         Test_Root & "Test15_ConcatOp.hs");

      --  Phase 5: Foreign string marshalling roundtrip.
      --  Set_Slot (String) -> Receive_Value (builds the SKI cons list)
      --  -> Send_Value (walks the spine back) -> Get_Slot (String).
      --  The Haskell-level suites never reach this FFI path, which is why
      --  the off-by-one in Receive_Value's cons build went undetected.

      declare
         FH : constant Leander.Handles.Reference :=
                Leander.Handles.Create (256 * 1024, null);

         procedure Roundtrip (Label, S : String);

         ---------------
         -- Roundtrip --
         ---------------

         procedure Roundtrip (Label, S : String) is
         begin
            FH.Set_Slot (1, S);
            declare
               Marshalled : constant Skit.Object := FH.Receive_Value (1);
            begin
               FH.Send_Value (2, Leander.String_Type, Marshalled);
            end;
            Test (Label, S, FH.Get_Slot (2));
         exception
            when E : others =>
               Error (Label, Ada.Exceptions.Exception_Message (E));
         end Roundtrip;

      begin
         Roundtrip ("foreign string roundtrip (empty)", "");
         Roundtrip ("foreign string roundtrip (one char)", "x");
         Roundtrip ("foreign string roundtrip (hello)", "hello");
         FH.Close;
      end;

      Leander.Syntax.Prune;

      Handle.Close;

   end Run_Tests;

end Leander.Tests.Integration;
