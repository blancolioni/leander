with Leander.Tests.Inference;

procedure Leander.Driver is
   --  use Leander.Core;
   --  use Leander.Core.Expressions;
   --  E : constant Expressions.Reference :=
   --        Lambda (Id ("x"),
   --                Apply
   --                  (Apply
   --                     (Variable (Id ("+")),
   --                      Variable (Id ("x"))),
   --                   Literal (Literals.Integer_Literal ("1"))));
   --  T : constant Types.Reference :=
   --        Leander.Inference.Infer_Expression_Type
   --          (Leander.Core.Assumptions.Empty,
   --           E);
begin
   Leander.Tests.Inference.Run;
   --  Ada.Text_IO.Put_Line
   --    (E.Show & " :: " & T.Show);
end Leander.Driver;
