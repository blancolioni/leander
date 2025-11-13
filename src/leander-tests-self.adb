with Ada.Exceptions;
with Ada.Text_IO;

with Leander.Calculus;
with Leander.Core.Expressions.Inference;
with Leander.Core.Inference;
with Leander.Environment;
with Leander.Syntax.Expressions;

with Skit.Compiler;

package body Leander.Tests.Self is

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests
     (Path      : String;
      Context   : in out Leander.Parser.Parse_Context'Class;
      Skit_Env  : Skit.Environment.Reference)
   is
      use Leander.Core.Inference;
      use Leander.Core.Expressions.Inference;
      Test_Env : constant Leander.Environment.Reference :=
                   Context.Load_Module (Path);
      Syntax   : constant Leander.Syntax.Expressions.Reference :=
                   Context.Parse_Expression ("runIO main");
      Core     : constant Leander.Core.Expressions.Reference :=
                   Syntax.To_Core;
      Result   : Inference_Context :=
                   Initial_Context (Test_Env.Type_Env);
   begin
      Infer (Result, Core);
      if not Result.OK then
         Ada.Text_IO.Put_Line (Result.Error_Message);
      else
         declare
            Tree : constant Leander.Calculus.Tree :=
                     Core.To_Calculus (Result, Test_Env);
         begin
            Leander.Calculus.Compile
              (Tree, Test_Env, Skit_Env);

            Skit.Compiler.Compile (Skit_Env.Machine);
            Skit_Env.Machine.Evaluate;
            Skit_Env.Machine.Drop;
         end;
      end if;
   exception
      when E : others =>
         Error (Core.Show, Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Ada.Exceptions.Exception_Information (E));
   end Run_Tests;


end Leander.Tests.Self;
