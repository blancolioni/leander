with Ada.Exceptions;
with Ada.Text_IO;

with Leander.Core.Assumptions;
with Leander.Core.Bindings;
with Leander.Core.Binding_Groups;
with Leander.Core.Expressions;
with Leander.Core.Kinds;
with Leander.Core.Literals;
with Leander.Core.Schemes;
with Leander.Core.Tycons;
with Leander.Core.Types;
with Leander.Core.Tyvars;

with Leander.Inference;
with Leander.Logging;

with Leander.Parser;

package body Leander.Tests.Inference is

   procedure Test
     (Expression : Leander.Core.Expressions.Reference;
      Expected   : String);

   procedure Test
     (Expression    : String;
      Expected_Type : String);

   ---------
   -- Run --
   ---------

   procedure Run is
      use Leander.Core, Leander.Core.Expressions;
   begin
      Leander.Logging.Start_Logging;
      Test ("1", "Int");
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
           (Binding_Groups.Binding_Group
                ([Bindings.Container
                 ([Bindings.Bind
                    (Id ("x"),
                       Literal (Literals.Integer_Literal ("123")))])]),
            Variable (Id ("x"))),
         "Int");

      Test ("+", "Int->Int->Int");
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
           (Binding_Groups.Binding_Group
                ([Bindings.Container
                 ([Bindings.Bind
                    (Id ("id"),
                       Lambda
                         (Core.Id ("x"),
                          Variable (Core.Id ("x"))))])]),
            Apply
              (Variable (Id ("id")),
               Variable (Id ("id")))),
         "a->a");

      Test
        (Let
           (Binding_Groups.Binding_Group
                ([Bindings.Container
                 ([Bindings.Bind
                    (Id ("f"),
                       Lambda
                         (Core.Id ("x"),
                          Apply
                            (Apply
                                 (Variable (Core.Id ("+")),
                                  Variable (Core.Id ("x"))),
                             Apply
                               (Variable (Core.Id ("f")),
                                Apply
                                  (Apply
                                       (Variable (Core.Id ("+")),
                                        Variable (Core.Id ("x"))),
                                   Literal
                                     (Core.Literals.Integer_Literal ("1"))
                                  )
                               )
                            )
                         )
                      )
                   ])
                ]),
            Apply
              (Variable (Id ("f")),
               Literal (Core.Literals.Integer_Literal ("1")))),
         "Int");

      declare
         Expr : constant String :=
                  "let f x = 2 * g x" & Character'Val (10)
                & "    g y = f (y + 1)" & Character'Val (10)
                & "in g 2";
      begin
         Test (Expr, "Int");
      end;

      Leander.Logging.Stop_Logging;
   end Run;

   ----------
   -- Test --
   ----------

   procedure Test
     (Expression    : String;
      Expected_Type : String)
   is
   begin
      Test (Leander.Parser.Parse_Expression (Expression),
            Expected_Type);
   exception
      when Leander.Parser.Parse_Error =>
         Ada.Text_IO.Put (Expression & " :: " & Expected_Type);
         Ada.Text_IO.Set_Col (60);
         Ada.Text_IO.Put_Line ("FAIL: parse error");
   end Test;

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
      Ada.Text_IO.Set_Col (60);
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
