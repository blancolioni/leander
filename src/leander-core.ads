with Leander.Names;

package Leander.Core is

   type Conid is new Leander.Names.Leander_Name;
   function To_Conid (S : String) return Conid;

   type Conid_Array is array (Positive range <>) of Conid;

   type Varid is new Leander.Names.Leander_Name;
   function To_Varid (S : String) return Varid;

   type Varid_Array is array (Positive range <>) of Varid;

   type Abstraction is interface;

   procedure Prune;

   type Declaration_Context is
     (Binding_Context,
      Class_Context,
      Instance_Context,
      Data_Context);

private

   type Core_Reference is access constant Abstraction'Class;

   function Allocate
     (This : Abstraction'Class)
      return Core_Reference;

end Leander.Core;
