with Leander.Core.Kinds;
with Leander.Core.Tycons;

package body Leander.Tests.Tycons is

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
   begin
      Test ("Conid", "Int",
            Leander.Core.Tycons.Tycon
              (Leander.Core.To_Conid ("Int"),
               Leander.Core.Kinds.Star)
            .Show);
      Test ("Kind", "*->*",
            Leander.Core.Kinds.Show
              (Leander.Core.Tycons.Tycon
                 (Leander.Core.To_Conid ("[]"),
                  Leander.Core.Kinds.Kind_Function
                    (Leander.Core.Kinds.Star,
                     Leander.Core.Kinds.Star))
                 .Get_Kind));
   end Run_Tests;

end Leander.Tests.Tycons;
