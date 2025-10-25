with Leander.Core.Kinds;

package body Leander.Tests.Kinds is

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
      use Leander.Core.Kinds;
   begin
      Test ("star kind", "*", Show (Star));
      Test ("simple kind", "*->*", Show (Kind_Function (Star, Star)));
      Test ("triple kind", "*->(*->*)",
            Show (Kind_Function (Star, Kind_Function (Star, Star))));
      Test ("triple kind (2)", "*->*->*",
            Show (Kind_Function (Kind_Function (Star, Star), Star)));
      Test ("star is star", "TRUE", Boolean'Image (Is_Star (Star)));
      Test ("fn is star", "FALSE",
            Boolean'Image (Is_Star (Kind_Function (Star, Star))));
   end Run_Tests;

end Leander.Tests.Kinds;
