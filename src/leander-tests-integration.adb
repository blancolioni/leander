with Ada.Exceptions;
with Leander.Handles;
with Leander.Syntax;

package body Leander.Tests.Integration is

   Test_Root : constant String :=
     "./share/leander/tests/integration/";

   procedure Test_Eval
     (Expression     : String;
      Expected_Type  : String;
      Expected_Value : String;
      Handle         : in out Leander.Handles.Instance);

   procedure Test_Module
     (Label          : String;
      Module_Path    : String;
      Expression     : String;
      Expected_Value : String;
      Handle         : in out Leander.Handles.Instance);

   ---------------
   -- Test_Eval --
   ---------------

   procedure Test_Eval
     (Expression     : String;
      Expected_Type  : String;
      Expected_Value : String;
      Handle         : in out Leander.Handles.Instance)
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
      Handle         : in out Leander.Handles.Instance)
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
      H      : Leander.Handles.Instance :=
                 Leander.Handles.Create (256 * 1024);
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
      Handle : Leander.Handles.Instance := Leander.Handles.Create (256 * 1024);
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

      Leander.Syntax.Prune;

   end Run_Tests;

end Leander.Tests.Integration;
