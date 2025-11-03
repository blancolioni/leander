with Leander.Allocator;

package body Leander.Core.Alts is

   type Variable_Reference is access all Instance;

   package Allocator is
     new Leander.Allocator ("expressions", Instance, Variable_Reference);

   --------------
   -- Allocate --
   --------------

   function Allocate
     (This : Instance)
      return Reference
   is
   begin
      return Reference (Allocator.Allocate (This));
   end Allocate;

   ---------
   -- Alt --
   ---------

   function Alt
     (Pat  : Leander.Core.Patterns.Reference;
      Expr : Leander.Core.Expressions.Reference)
      return Reference
   is
   begin
      return Allocate
        (Instance'
           (Id => Typeable.New_Id,
            Pat => Nullable_Pattern_Reference (Pat),
            Expr => Expr));
   end Alt;

   ---------
   -- Alt --
   ---------

   function Alt
     (Expr : Leander.Core.Expressions.Reference)
      return Reference
   is
   begin
      return Allocate
        (Instance'
           (Id => Typeable.New_Id,
            Pat => null,
            Expr => Expr));
   end Alt;

   -------------------
   -- Has_Reference --
   -------------------

   function Has_Reference
     (This : Instance;
      Name : Varid)
      return Boolean
   is
   begin
      return (This.Pat = null or else not This.Pat.Has_Reference (Name))
        and then This.Expr.Has_Reference (Name);
   end Has_Reference;

   -----------
   -- Prune --
   -----------

   procedure Prune is
   begin
      Allocator.Prune;
   end Prune;

   ----------
   -- Show --
   ----------

   overriding function Show (This : Instance) return String is
   begin
      return (if This.Pat = null
              then This.Expr.Show
              else This.Pat.Show & " -> " & This.Expr.Show);
   end Show;

end Leander.Core.Alts;
