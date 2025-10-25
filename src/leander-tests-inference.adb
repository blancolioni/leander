with Ada.Exceptions;
with Ada.Text_IO;
with Leander.Core.Expressions;
with Leander.Core.Expressions.Inference;
with Leander.Core.Inference;
with Leander.Core.Literals;
with Leander.Core.Type_Env;

package body Leander.Tests.Inference is

   procedure Test
     (Expression : Leander.Core.Expressions.Reference;
      Expected   : String);

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
      use Leander.Core, Leander.Core.Expressions;
      function Int (Img : String) return Leander.Core.Literals.Instance
      is (Leander.Core.Literals.Integer_Literal (Img));
   begin
      Test (Literal (Int ("1")), "Int");
      Test (Lambda
            (To_Varid ("x"),
               Variable (To_Varid ("x"))),
            "a->a");
      Test (Lambda
            (To_Varid ("x"),
               Lambda
                 (To_Varid ("y"),
                  Variable (To_Varid ("x")))),
            "a->b->a");
   end Run_Tests;

   ----------
   -- Test --
   ----------

   procedure Test
     (Expression : Leander.Core.Expressions.Reference;
      Expected   : String)
   is
      Env_Builder : Core.Type_Env.Builder;
      Type_Env    : constant Core.Type_Env.Reference :=
                      Env_Builder.Get_Type_Env;
      Result      : Core.Inference.Inference_Context :=
                      Core.Inference.Initial_Context (Type_Env);
   begin
      Core.Expressions.Inference.Infer (Result, Expression);

      if not Result.OK then
         Fail (Expression.Show, Expected, "type inference Failed");
         Ada.Text_IO.Put_Line (Result.Error_Message);
      else
         declare
            Inferred_Type : constant String :=
                              Result.Get_Type (Expression).Generate.Show;
         begin
            Test (Expression.Show, Expected, Inferred_Type);
         end;
      end if;
   exception
      when E : others =>
         Error (Expression.Show, Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Ada.Exceptions.Exception_Information (E));
   end Test;

end Leander.Tests.Inference;
