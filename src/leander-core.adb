private with Leander.Allocator;
with Leander.Core.Expressions;
with Leander.Core.Schemes;
with Leander.Core.Type_Env;
with Leander.Core.Types;

package body Leander.Core is

   function To_Conid (S : String) return Conid
   is (Conid (Leander.Names.To_Leander_Name (S)));

   function To_Varid (S : String) return Varid
   is (Varid (Leander.Names.To_Leander_Name (S)));

   type Variable_Reference is access all Abstraction'Class;

   package Allocator is
     new Leander.Allocator ("core", Abstraction'Class, Variable_Reference);

   --------------
   -- Allocate --
   --------------

   function Allocate
     (This : Abstraction'Class)
      return Core_Reference
   is
   begin
      return Core_Reference (Allocator.Allocate (This));
   end Allocate;

   -----------
   -- Prune --
   -----------

   procedure Prune is
   begin
      Allocator.Prune;
      Leander.Core.Schemes.Prune;
      Leander.Core.Type_Env.Prune;
      Leander.Core.Types.Prune;
      Leander.Core.Expressions.Prune;
   end Prune;

end Leander.Core;
