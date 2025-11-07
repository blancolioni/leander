with Ada.Exceptions;
with Ada.Text_IO;
with Leander.Calculus;
with Leander.Core.Expressions;
with Leander.Core.Expressions.Inference;
with Leander.Core.Inference;
with Leander.Environment;
with Leander.Parser;
with Leander.Syntax;
with Leander.Syntax.Expressions;
with Skit.Compiler;
with Skit.Debug;
with Skit.Environment;
with Skit.Impl;
with Skit.Library;
with Skit.Machine;

package body Leander.Tests.Evaluation is

   procedure Test
     (Expression     : String;
      Expected_Type  : String;
      Expected_Value : String;
      Skit_Env       : Skit.Environment.Reference;
      Prelude        : Leander.Environment.Reference);

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
      Prelude : constant Leander.Environment.Reference :=
                  Leander.Parser.Load_Module
                    ("./share/leander/modules/Prelude.hs");
      Machine : constant Skit.Machine.Reference :=
                  Skit.Impl.Machine (8 * 1024);
      Env     : constant Skit.Environment.Reference :=
                  Skit.Environment.Create
                    (Machine);
   begin
      Skit.Library.Load_Primitives (Env);
      Test ("1", "Int", "1", Env, Prelude);
      Test ("null []", "Bool", "K", Env, Prelude);
      Test ("null [1]", "Bool", "K I", Env, Prelude);
      Test ("length []", "Int", "0", Env, Prelude);
      Test ("length [1]", "Int", "1", Env, Prelude);
      Test ("length [1,2,3,4]", "Int", "4", Env, Prelude);
      Test ("zero 0", "Bool", "K", Env, Prelude);
      Test ("zero 1", "Bool", "K I", Env, Prelude);
      Test ("zero 100", "Bool", "K I", Env, Prelude);
      Test ("small 3", "Bool", "K", Env, Prelude);
      Test ("small 5", "Bool", "K I", Env, Prelude);
      Test ("id 123", "Int", "123", Env, Prelude);
      Test ("const 3 []", "Int", "3", Env, Prelude);
      Test ("2 * 3 + 4", "Int", "10", Env, Prelude);
      Test ("2 + 3 * 4", "Int", "14", Env, Prelude);
      Test ("2 - 3 * 4", "Int", "-10", Env, Prelude);
      Test ("seq (#trace 42) 4", "Int", "4", Env, Prelude);
      Test ("sum [1,2,3,4]", "Int", "10", Env, Prelude);
      Test ("sum (map succ [1,2,3])", "Int", "9", Env, Prelude);
      Test ("length (take 10 [1,2,3])", "Int", "3", Env, Prelude);
      Test ("length (take 2 [1,2,3])", "Int", "2", Env, Prelude);
      Test ("sum (do { x <- [42]; return x })", "Int", "42", Env, Prelude);
      Test ("sum (do { let x = 42; return x })", "Int", "42", Env, Prelude);
      Machine.Report;
   end Run_Tests;

   ----------
   -- Test --
   ----------

   procedure Test
     (Expression     : String;
      Expected_Type  : String;
      Expected_Value : String;
      Skit_Env       : Skit.Environment.Reference;
      Prelude        : Leander.Environment.Reference)
   is
      use Leander.Core.Inference;
      use Leander.Core.Expressions.Inference;
      Syntax : constant Leander.Syntax.Expressions.Reference :=
                 Leander.Parser.Parse_Expression (Expression);
      Core   : constant Leander.Core.Expressions.Reference :=
                 Syntax.To_Core;
      Result : Inference_Context :=
                 Initial_Context (Prelude.Type_Env);
   begin
      Infer (Result, Core);
      if not Result.OK then
         Fail (Core.Show, Expected_Type, "type inference failed");
         Ada.Text_IO.Put_Line (Result.Error_Message);
      else
         declare
            Inferred_Type : constant String :=
                              Result.Get_Type (Core).Generate.Show;
         begin
            if Inferred_Type /= Expected_Type then
               Fail (Core.Show, Expected_Type, Inferred_Type);
            else
               declare
                  Tree : constant Leander.Calculus.Tree :=
                           Core.To_Calculus (Result, Prelude);
               begin
                  Leander.Calculus.Compile
                    (Tree, Prelude, Skit_Env);

                  Skit.Compiler.Compile (Skit_Env.Machine);
                  Skit_Env.Machine.Evaluate;
                  declare
                     Value : constant String :=
                               Skit.Debug.Image
                                 (Skit_Env.Machine.Top, Skit_Env.Machine);
                  begin
                     Test (Core.Show, Expected_Value, Value);
                  end;
               end;
            end if;
         end;
      end if;
   exception
      when E : others =>
         Error (Core.Show, Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Ada.Exceptions.Exception_Information (E));
   end Test;

end Leander.Tests.Evaluation;
