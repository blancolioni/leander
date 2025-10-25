with Leander.Names;

package Leander.Core is

   type Conid is new Leander.Names.Leander_Name;
   function To_Conid (S : String) return Conid;

   type Varid is new Leander.Names.Leander_Name;
   function To_Varid (S : String) return Varid;

   procedure Prune;

end Leander.Core;
