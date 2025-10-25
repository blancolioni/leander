with Leander.Core.Expressions;
with Leander.Core.Schemes;
with Leander.Core.Type_Env;
with Leander.Core.Types;

package body Leander.Core is

   function To_Conid (S : String) return Conid
   is (Conid (Leander.Names.To_Leander_Name (S)));

   function To_Varid (S : String) return Varid
   is (Varid (Leander.Names.To_Leander_Name (S)));

   -----------
   -- Prune --
   -----------

   procedure Prune is
   begin
      Leander.Core.Schemes.Prune;
      Leander.Core.Type_Env.Prune;
      Leander.Core.Types.Prune;
      Leander.Core.Expressions.Prune;
   end Prune;

end Leander.Core;
