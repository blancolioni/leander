with Leander.Core.Kinds;
with Leander.Core.Tyvars;

package body Leander.Tests.Tyvars is

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
   begin
      Test ("varid", "a",
            Leander.Core.Tyvars.Tyvar
              (Leander.Core.To_Varid ("a"),
               Leander.Core.Kinds.Star)
            .Show);
      Test ("Kind", "*->*",
            Leander.Core.Kinds.Show
              (Leander.Core.Tyvars.Tyvar
                 (Leander.Core.To_Varid ("m"),
                  Leander.Core.Kinds.Kind_Function
                    (Leander.Core.Kinds.Star,
                     Leander.Core.Kinds.Star))
                 .Get_Kind));
   end Run_Tests;

end Leander.Tests.Tyvars;
