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
           (Id   => Typeable.New_Id,
            Pat  => Nullable_Pattern_Reference (Pat),
            Expr => Expr,
            QT   => null));
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
           (Id   => Typeable.New_Id,
            Pat  => null,
            Expr => Expr,
            QT   => null));
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

   ------------------------
   -- Set_Qualified_Type --
   ------------------------

   overriding procedure Set_Qualified_Type
     (This : in out Instance;
      QT   : Leander.Core.Qualified_Types.Reference)
   is
   begin
      This.QT := Nullable_Qualified_Type_Reference (QT);
   end Set_Qualified_Type;

   ----------
   -- Show --
   ----------

   overriding function Show (This : Instance) return String is
   begin
      return (if This.Pat = null
              then This.Expr.Show
              else This.Pat.Show & " -> " & This.Expr.Show);
   end Show;

   --------------
   -- Traverse --
   --------------

   overriding procedure Traverse
     (This    : not null access constant Instance;
      Process : not null access
        procedure (This : not null access constant
                     Leander.Traverseable.Abstraction'Class))
   is
   begin
      Process (This);
      if This.Pat /= null then
         This.Pat.Traverse (Process);
      end if;
      This.Expr.Traverse (Process);
   end Traverse;

   ---------------------
   -- Update_Traverse --
   ---------------------

   overriding procedure Update_Traverse
     (This    : not null access Instance;
      Process : not null access
        procedure (This : not null access
                     Leander.Traverseable.Abstraction'Class))
   is
   begin
      Process (This);
      if This.Pat /= null then
         This.Pat.Update_Traverse (Process);
      end if;
      This.Expr.Update_Traverse (Process);
   end Update_Traverse;

end Leander.Core.Alts;
