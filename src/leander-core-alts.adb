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
     (Pats : Leander.Core.Patterns.Reference_Array;
      Expr : Leander.Core.Expressions.Reference)
      return Reference
   is
   begin
      return Allocate (Instance'(Pats'Length, Typeable.New_Id, Pats, Expr));
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
      return (for all Pat of This.Pats => not Pat.Has_Reference (Name))
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
      function Show_Pats (Index : Positive) return String
      is ((if Index = 1 then "" else " ")
          & This.Pats (Index).Show
          & (if Index < This.Pat_Count
            then Show_Pats (Index + 1)
            else ""));
   begin
      if This.Pat_Count = 0 then
         return This.Expr.Show;
      else
         return Show_Pats (1) & "->" & This.Expr.Show;
      end if;
   end Show;

end Leander.Core.Alts;
