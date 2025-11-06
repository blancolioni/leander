with Leander.Core.Expressions;
with Leander.Core.Literals;
with Leander.Source;
package body Leander.Tests.Expressions is

   procedure Test
     (Name       : String;
      Expected   : String;
      Expression : Leander.Core.Expressions.Reference);

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
      use Leander.Core.Expressions;
      function Int (Img : String) return Leander.Core.Literals.Instance
      is (Leander.Core.Literals.Integer_Literal (Img));

      Loc : constant Leander.Source.Source_Location :=
              Leander.Source.No_Location;
   begin
      Test ("literal-integer", "1",
            Literal (Loc, Int ("1")));
      Test ("\x -> x",
            "\x -> x",
            Lambda
              (Loc, Core.To_Varid ("x"),
               Variable (Loc, Core.To_Varid ("x"))));
      Test ("(\x -> x) 1",
            "(\x -> x) 1",
            Application
              (Loc, Lambda
                 (Loc, Core.To_Varid ("x"),
                  Variable (Loc, Core.To_Varid ("x"))),
               Literal (Loc, Int ("1"))));
   end Run_Tests;

   ----------
   -- Test --
   ----------

   procedure Test
     (Name       : String;
      Expected   : String;
      Expression : Leander.Core.Expressions.Reference)
   is
   begin
      Test (Name, Expected, Expression.Show);
   end Test;

end Leander.Tests.Expressions;
