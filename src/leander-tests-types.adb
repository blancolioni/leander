with Leander.Core.Kinds;
with Leander.Core.Types;
with Leander.Core.Tyvars;

package body Leander.Tests.Types is

   procedure Test
     (Name     : String;
      Expected : String;
      Ty       : Leander.Core.Types.Reference);

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
      use Leander.Core.Types;
   begin
      Test ("Unit", "()", T_Unit);
      Test ("Char", "Char", T_Char);
      Test ("Int", "Int", T_Int);
      Test ("Integer", "Integer", T_Integer);
      Test ("Float", "Float", T_Float);
      Test ("Double", "Double", T_Double);
      Test ("List", "[]", T_List);
      Test ("Arrow", "(->)", T_Arrow);
      Test ("Pair", "(,)", T_Pair);
      Test ("String", "[Char]", T_String);
      Test ("list-image", "[Int]",
            List_Of (T_Int));
      Test ("arrow-image", "Int->Char",
            Fn (T_Int, T_Char));
      Test ("show-Image", "Double->[Char]",
            Fn (T_Double, List_Of (T_Char)));

      declare
         function TVar (Varid : String) return Reference
         is (Core.Types.TVar
             (Core.Tyvars.Tyvar
                (Core.To_Varid (Varid),
                   Core.Kinds.Star)));
      begin
         Test ("const-type", "a->b->a",
               Fn (TVar ("a"),
                 Fn (TVar ("b"), TVar ("a"))));
         Test ("call-type", "(a->b)->a->b",
               Fn (Fn (TVar ("a"), TVar ("b")),
                 Fn (TVar ("a"), TVar ("b"))));
      end;
   end Run_Tests;

   ----------
   -- Test --
   ----------

   procedure Test
     (Name     : String;
      Expected : String;
      Ty       : Leander.Core.Types.Reference)
   is
   begin
      Test (Name, Expected, Ty.Show);
   end Test;

end Leander.Tests.Types;
