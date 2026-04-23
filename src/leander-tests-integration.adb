with Ada.Exceptions;
with Leander.Calculus;
with Leander.Core.Expressions;
with Leander.Core.Expressions.Inference;
with Leander.Core.Inference;
with Leander.Environment;
with Leander.Handles;
with Leander.Parser;
with Leander.Syntax;
with Leander.Syntax.Expressions;
with Skit.Compiler;
with Skit.Debug;
with Skit.Environment;
with Skit.Impl;
with Skit.Library;
with Skit.Machine;

package body Leander.Tests.Integration is

   Test_Root : constant String :=
     "./share/leander/tests/integration/";

   procedure Test_Eval
     (Expression     : String;
      Expected_Type  : String;
      Expected_Value : String;
      Context        : Leander.Parser.Parse_Context;
      Skit_Env       : Skit.Environment.Reference;
      Env            : Leander.Environment.Reference);

   procedure Test_Module
     (Label          : String;
      Module_Path    : String;
      Expression     : String;
      Expected_Value : String;
      Context        : in out
        Leander.Parser.Parse_Context'Class;
      Skit_Env       : Skit.Environment.Reference);

   ---------------
   -- Test_Eval --
   ---------------

   procedure Test_Eval
     (Expression     : String;
      Expected_Type  : String;
      Expected_Value : String;
      Context        : Leander.Parser.Parse_Context;
      Skit_Env       : Skit.Environment.Reference;
      Env            : Leander.Environment.Reference)
   is
      use Leander.Core.Inference;
      use Leander.Core.Expressions.Inference;
   begin
      declare
         Syntax : constant
           Leander.Syntax.Expressions.Reference :=
             Context.Parse_Expression (Expression);
         Core   : constant
           Leander.Core.Expressions.Reference :=
             Syntax.To_Core;
         Result : Inference_Context :=
                    Initial_Context (Env.Type_Env);
      begin
         Infer (Result, Core);
         if not Result.OK then
            Fail (Expression, Expected_Type,
                  "type inference failed: "
                  & Result.Error_Message);
         else
            Result.Update_Type (Core);
            declare
               Inferred_Type : constant String :=
                 Result.Get_Type
                   (Core).Generate.Show;
            begin
               if Inferred_Type /= Expected_Type then
                  Fail (Expression, Expected_Type,
                        Inferred_Type);
               else
                  declare
                     Tree : constant
                       Leander.Calculus.Tree :=
                         Core.To_Calculus
                           (Result, Env);
                  begin
                     Leander.Calculus.Compile
                       (Tree, Env, Skit_Env);
                     Skit.Compiler.Compile
                       (Skit_Env.Machine);
                     Skit_Env.Machine.Evaluate;
                     declare
                        Value : constant String :=
                          Skit.Debug.Image
                            (Skit_Env.Machine.Top,
                             Skit_Env.Machine);
                     begin
                        Test (Expression,
                              Expected_Value,
                              Value);
                     end;
                  end;
               end if;
            end;
         end if;
      end;
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
      Context        : in out
        Leander.Parser.Parse_Context'Class;
      Skit_Env       : Skit.Environment.Reference)
   is
      use Leander.Core.Inference;
      use Leander.Core.Expressions.Inference;
   begin
      declare
         Test_Env : constant
           Leander.Environment.Reference :=
             Context.Load_Module (Module_Path);
         Syntax   : constant
           Leander.Syntax.Expressions.Reference :=
             Context.Parse_Expression (Expression);
         Core     : constant
           Leander.Core.Expressions.Reference :=
             Syntax.To_Core;
         Result   : Inference_Context :=
           Initial_Context (Test_Env.Type_Env);
      begin
         Infer (Result, Core);
         if not Result.OK then
            Fail (Label, Expected_Value,
                  "type inference: "
                  & Result.Error_Message);
         else
            Result.Update_Type (Core);
            declare
               Tree : constant
                 Leander.Calculus.Tree :=
                   Core.To_Calculus
                     (Result, Test_Env);
            begin
               Leander.Calculus.Compile
                 (Tree, Test_Env, Skit_Env);
               Skit.Compiler.Compile
                 (Skit_Env.Machine);
               Skit_Env.Machine.Evaluate;
               declare
                  Value : constant String :=
                    Skit.Debug.Image
                      (Skit_Env.Machine.Top,
                       Skit_Env.Machine);
               begin
                  Test (Label,
                        Expected_Value, Value);
               end;
            end;
         end if;
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
      H      : Leander.Handles.Handle :=
                 Leander.Handles.Create (64 * 1024);
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
      Context : Leander.Parser.Parse_Context;
      Prelude : constant Leander.Environment.Reference :=
                  Context.Load_Module
                    ("./share/leander/modules/Prelude.hs");
      Machine : constant Skit.Machine.Reference :=
                  Skit.Impl.Machine (64 * 1024);
      Env     : constant Skit.Environment.Reference :=
                  Skit.Environment.Create
                    (Machine);
   begin
      Skit.Library.Load_Primitives (Env);

      --  Phase 1: Expression-level tests
      --  Type class method dispatch (Eq)

      Test_Eval ("1 == 1", "Bool", "K",
                 Context, Env, Prelude);
      Test_Eval ("1 == 2", "Bool", "K I",
                 Context, Env, Prelude);
      Test_Eval ("True == True", "Bool", "K",
                 Context, Env, Prelude);
      Test_Eval ("True == False", "Bool", "K I",
                 Context, Env, Prelude);

      --  Let expressions

      Test_Eval ("let x = 3 in x + 1", "Int", "4",
                 Context, Env, Prelude);

      --  Case expressions on Int

      Test_Eval
        ("case 3 of { 1 -> 10; 2 -> 20; _ -> 30 }",
         "Int", "30",
         Context, Env, Prelude);
      Test_Eval
        ("case 2 of { 1 -> 10; 2 -> 20; _ -> 30 }",
         "Int", "20",
         Context, Env, Prelude);

      --  If-then-else

      Test_Eval ("if True then 5 else 10",
                 "Int", "5",
                 Context, Env, Prelude);
      Test_Eval ("if False then 5 else 10",
                 "Int", "10",
                 Context, Env, Prelude);

      --  Higher-order with operator sections

      Test_Eval ("foldr (+) 0 [1,2,3]",
                 "Int", "6",
                 Context, Env, Prelude);

      --  List operations

      Test_Eval
        ("sum (concat [[1,2],[3,4]])",
         "Int", "10",
         Context, Env, Prelude);
      Test_Eval
        ("sum ([1,2] ++ [3,4])",
         "Int", "10",
         Context, Env, Prelude);

      --  Equality with tuple projections

      Test_Eval ("fst (3,4) == 3", "Bool", "K",
                 Context, Env, Prelude);
      Test_Eval ("snd (3,4) == 4", "Bool", "K",
                 Context, Env, Prelude);

      --  uncurry

      Test_Eval ("uncurry (+) (3,4)",
                 "Int", "7",
                 Context, Env, Prelude);

      --  Phase 2: Module tests (non-IO)
      --  Simple function definition

      Test_Module
        ("module: function",
         Test_Root & "test_01_function.hs",
         "double 21", "42",
         Context, Env);

      --  Bool pattern matching in module

      Test_Module
        ("module: bool pattern",
         Test_Root & "test_02_bool_pattern.hs",
         "toggle True", "K I",
         Context, Env);

      --  Local data type definition

      Test_Module
        ("module: data type",
         Test_Root & "test_03_data_type.hs",
         "colorNum Green", "2",
         Context, Env);

      --  Type class usage in module

      Test_Module
        ("module: type class",
         Test_Root & "test_04_type_class.hs",
         "same 3 3", "K",
         Context, Env);

      --  Derived Eq instances

      Test_Module
        ("module: deriving Eq nullary match",
         Test_Root & "test_16_deriving_eq.hs",
         "colorEq", "K",
         Context, Env);
      Test_Module
        ("module: deriving Eq nullary mismatch",
         Test_Root & "test_16_deriving_eq.hs",
         "colorNe", "K I",
         Context, Env);
      Test_Module
        ("module: deriving Eq nullary /=",
         Test_Root & "test_16_deriving_eq.hs",
         "colorBl", "K",
         Context, Env);
      Test_Module
        ("module: deriving Eq args match",
         Test_Root & "test_16_deriving_eq.hs",
         "pairEq", "K",
         Context, Env);
      Test_Module
        ("module: deriving Eq args mismatch last",
         Test_Root & "test_16_deriving_eq.hs",
         "pairNe1", "K I",
         Context, Env);
      Test_Module
        ("module: deriving Eq args mismatch first",
         Test_Root & "test_16_deriving_eq.hs",
         "pairNe2", "K I",
         Context, Env);
      Test_Module
        ("module: deriving Eq mixed nullary",
         Test_Root & "test_16_deriving_eq.hs",
         "mixLL", "K",
         Context, Env);
      Test_Module
        ("module: deriving Eq mixed arity",
         Test_Root & "test_16_deriving_eq.hs",
         "mixRR", "K",
         Context, Env);
      Test_Module
        ("module: deriving Eq cross constructor",
         Test_Root & "test_16_deriving_eq.hs",
         "mixLR", "K I",
         Context, Env);
      Test_Module
        ("module: deriving Eq same constructor diff args",
         Test_Root & "test_16_deriving_eq.hs",
         "mixRdif", "K I",
         Context, Env);
      Test_Module
        ("module: deriving Eq recursive match",
         Test_Root & "test_16_deriving_eq.hs",
         "recEq", "K",
         Context, Env);
      Test_Module
        ("module: deriving Eq recursive mismatch",
         Test_Root & "test_16_deriving_eq.hs",
         "recNe", "K I",
         Context, Env);
      Test_Module
        ("module: deriving Eq recursive minimal",
         Test_Root & "test_16_deriving_eq.hs",
         "recNil", "K",
         Context, Env);

      --  Phase 3: IO module tests
      --  Minimal IO

      Test_Module
        ("module: IO return",
         Test_Root & "test_05_io_return.hs",
         "runIO main", "I",
         Context, Env);

      --  IO with putChar

      Test_Module
        ("module: IO putChar",
         Test_Root & "test_06_io_putchar.hs",
         "runIO main", "I",
         Context, Env);

      --  IO with putStr

      Test_Module
        ("module: IO putStr",
         Test_Root & "test_07_io_putstr.hs",
         "runIO main", "I",
         Context, Env);

      --  Do notation

      Test_Module
        ("module: do notation",
         Test_Root & "test_08_do_notation.hs",
         "runIO main", "I",
         Context, Env);

      --  ($) and (++) operators

      Test_Module
        ("module: ($) and (++)",
         Test_Root & "test_09_dollar_concat.hs",
         "runIO main", "I",
         Context, Env);

      --  Tuples in IO context

      Test_Module
        ("module: tuples in IO",
         Test_Root & "test_10_tuples.hs",
         "runIO main", "I",
         Context, Env);

      --  mapM_ with list

      Test_Module
        ("module: mapM_",
         Test_Root & "test_11_mapM.hs",
         "runIO main", "I",
         Context, Env);

      --  Full test (near RunTests complexity)

      Test_Module
        ("module: full integration",
         Test_Root & "test_12_full.hs",
         "runIO main", "I",
         Context, Env);

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
