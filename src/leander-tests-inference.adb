with Ada.Exceptions;
with Ada.Text_IO;

with Leander.Core.Assumptions;
with Leander.Core.Bindings;
with Leander.Core.Expressions;
with Leander.Core.Kinds;
with Leander.Core.Literals;
with Leander.Core.Schemes;
with Leander.Core.Tycons;
with Leander.Core.Types;
with Leander.Core.Tyvars;

with Leander.Inference;
with Leander.Logging;

package body Leander.Tests.Inference is

   procedure Test
     (Expression : Leander.Core.Expressions.Reference;
      Expected   : String);

   ---------
   -- Run --
   ---------

   procedure Run is
      use Leander.Core, Leander.Core.Expressions;
   begin
      Leander.Logging.Start_Logging;
      Test (Literal (Core.Literals.Integer_Literal ("1")), "Int");
      Test
        (Constructor
           (Id ("Nothing"),
            Core.Types.TCon
              (Core.Tycons.Tycon
                   (Id ("Maybe"),
                    Core.Kinds.KFun (Core.Kinds.Star, Core.Kinds.Star)))
            .Application
              (Core.Types.TVar
                   (Core.Tyvars.Tyvar
                        (Id ("a"),
                         Core.Kinds.Star)))),
         "Maybe a");
      Test
        (Lambda
           (Core.Id ("x"),
            Literal (Core.Literals.Integer_Literal ("1"))),
         "a->Int");
      Test
        (Let
           (Bindings.Bind
                (Id ("x"),
                 Literal (Literals.Integer_Literal ("123"))),
            Variable (Id ("x"))),
         "Int");

      Test
        (Variable (Id ("+")), "Int->Int->Int");
      Test
        (Lambda
           (Core.Id ("x"),
            Apply
              (Apply
                   (Variable (Id ("+")),
                    Variable (Id ("x"))),
               Variable (Id ("x")))),
         "Int->Int");

      Test
        (Let
           (Bindings.Bind
                (Id ("id"),
                 Lambda
                   (Core.Id ("x"),
                    Variable (Core.Id ("x")))),
            Apply
              (Variable (Id ("id")),
               Variable (Id ("id")))),
         "a->a");
      Leander.Logging.Stop_Logging;
   end Run;

   ----------
   -- Test --
   ----------

   procedure Test
     (Expression : Leander.Core.Expressions.Reference;
      Expected   : String)
   is
      Assumptions : constant Core.Assumptions.Reference :=
                      Core.Assumptions.Assumption
                        (Core.Id ("+"),
                         Core.Schemes.To_Scheme
                           (Core.Types.T_Int.Fn
                              (Core.Types.T_Int.Fn
                                 (Core.Types.T_Int))));
   begin
      Ada.Text_IO.Put (Expression.Show & " :: " & Expected);
      Leander.Logging.Log ("TEST", Expression.Show & " :: " & Expected);
      Ada.Text_IO.Set_Col (40);
      declare
         T : constant Core.Types.Reference :=
               Leander.Inference.Infer_Expression_Type
                 (Assumptions, Expression);
      begin
         if T.Show = Expected then
            Ada.Text_IO.Put_Line ("PASS");
         else
            Ada.Text_IO.Put_Line ("FAIL: " & T.Show);
         end if;
      end;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           ("ERROR: " & Ada.Exceptions.Exception_Message (E));
   end Test;

end Leander.Tests.Inference;
