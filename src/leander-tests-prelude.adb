with Ada.Exceptions;
with Ada.Text_IO;
with Leander.Core.Expressions;
with Leander.Core.Expressions.Inference;
with Leander.Core.Inference;
with Leander.Environment;
with Leander.Parser;
with Leander.Syntax;
with Leander.Syntax.Expressions;

package body Leander.Tests.Prelude is

   procedure Test
     (Expression    : String;
      Expected_Type : String;
      Context       : Leander.Parser.Parse_Context;
      Prelude       : Leander.Environment.Reference);

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
      Context : Leander.Parser.Parse_Context;
      Prelude : constant Leander.Environment.Reference :=
                  Context.Load_Module
                    ("./share/leander/modules/Prelude.hs");
   begin
      Test ("1", "Int", Context, Prelude);
      Test ("True", "Bool", Context, Prelude);
      Test ("False", "Bool", Context, Prelude);
      Test ("[]", "[a]", Context, Prelude);
      Test ("(:) 1 []", "[Int]", Context, Prelude);
      Test ("let x = 1 in x", "Int", Context, Prelude);
      Test ("let x = [] in x", "[a]", Context, Prelude);
      Test ("let {x=1;y=x} in x", "Int", Context, Prelude);
      Test ("let {x=1;y=x} in y", "Int", Context, Prelude);
      Test ("let {x=y;y=1} in x", "Int", Context, Prelude);
      Test ("null", "[a] -> Bool", Context, Prelude);
      Test ("null []", "Bool", Context, Prelude);
   end Run_Tests;

   ----------
   -- Test --
   ----------

   procedure Test
     (Expression    : String;
      Expected_Type : String;
      Context       : Leander.Parser.Parse_Context;
      Prelude       : Leander.Environment.Reference)
   is
      use Leander.Core.Inference;
      use Leander.Core.Expressions.Inference;
      Syntax : constant Leander.Syntax.Expressions.Reference :=
                 Context.Parse_Expression (Expression);
      Core   : constant Leander.Core.Expressions.Reference :=
                 Syntax.To_Core;
      Result : Inference_Context :=
                 Initial_Context (Prelude.Type_Env);
   begin
      Infer (Result, Core);
      if not Result.OK then
         Fail (Core.Show, Expected_Type, "type inference failed");
         Ada.Text_IO.Put_Line (Result.Error_Message);
         raise Program_Error;
      else
         declare
            Inferred_Type : constant String :=
                              Result.Get_Type (Core).Generate.Show;
         begin
            Test (Core.Show, Expected_Type, Inferred_Type);
         end;
      end if;
   exception
      when E : others =>
         Error (Core.Show, Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Ada.Exceptions.Exception_Information (E));
   end Test;

end Leander.Tests.Prelude;
