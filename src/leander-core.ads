with Leander.Names;
with Leander.Source;

package Leander.Core is

   type Conid is new Leander.Names.Leander_Name;
   function To_Conid (S : String) return Conid;

   type Varid is new Leander.Names.Leander_Name;
   function To_Varid (S : String) return Varid;

   type Varid_Array is array (Positive range <>) of Varid;

   type Abstraction is interface and Leander.Source.Has_Source_Location;

   procedure Prune;

   type Declaration_Context is
     (Binding_Context,
      Class_Context,
      Instance_Context,
      Data_Context);

end Leander.Core;
