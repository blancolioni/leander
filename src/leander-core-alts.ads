with Leander.Core.Expressions;
with Leander.Core.Patterns;
with Leander.Core.Qualified_Types;
with Leander.Core.Typeable;
with Leander.Showable;
with Leander.Traverseable;

package Leander.Core.Alts is

   type Instance is
     new Leander.Showable.Abstraction
     and Leander.Core.Qualified_Types.Has_Qualified_Type
     and Leander.Core.Typeable.Abstraction
     and Leander.Traverseable.Abstraction
   with private;

   type Reference is access all Instance'Class;
   type Reference_Array is array (Positive range <>) of Reference;

   function Alt
     (Pat  : Leander.Core.Patterns.Reference;
      Expr : Leander.Core.Expressions.Reference)
      return Reference;

   function Alt
     (Expr : Leander.Core.Expressions.Reference)
      return Reference;

   function Has_Pattern
     (This : Instance)
      return Boolean;

   function Pattern
     (This : Instance)
      return Leander.Core.Patterns.Reference
     with Pre => This.Has_Pattern;

   function Expression
     (This : Instance)
      return Leander.Core.Expressions.Reference;

   function Has_Reference
     (This : Instance;
      Name : Varid)
      return Boolean;

   procedure Prune;

private

   type Nullable_Pattern_Reference is
     access all Leander.Core.Patterns.Instance'Class;

   type Nullable_Qualified_Type_Reference is
     access constant Leander.Core.Qualified_Types.Instance'Class;

   type Instance is
     new Leander.Showable.Abstraction
     and Leander.Core.Qualified_Types.Has_Qualified_Type
     and Leander.Core.Typeable.Abstraction
     and Leander.Traverseable.Abstraction with
      record
         Id   : Leander.Core.Typeable.Typeable_Id;
         Pat  : Nullable_Pattern_Reference;
         Expr : Leander.Core.Expressions.Reference;
         QT   : Nullable_Qualified_Type_Reference;
      end record;

   overriding function Show (This : Instance) return String;

   overriding procedure Traverse
     (This    : not null access constant Instance;
      Process : not null access
        procedure (This : not null access constant
                     Leander.Traverseable.Abstraction'Class));

   overriding procedure Update_Traverse
     (This    : not null access Instance;
      Process : not null access
        procedure (This : not null access
                     Leander.Traverseable.Abstraction'Class));

   overriding function Get_Id
     (This : Instance)
      return Leander.Core.Typeable.Typeable_Id
   is (This.Id);

   overriding function Has_Qualified_Type_Value
     (This : Instance)
      return Boolean
   is (This.QT /= null);

   overriding function Qualified_Type
     (This : Instance)
      return Leander.Core.Qualified_Types.Reference
   is (Leander.Core.Qualified_Types.Reference (This.QT));

   overriding procedure Set_Qualified_Type
     (This : in out Instance;
      QT   : Leander.Core.Qualified_Types.Reference);

   function Has_Pattern
     (This : Instance)
      return Boolean
   is (This.Pat /= null);

   function Pattern
     (This : Instance)
      return Leander.Core.Patterns.Reference
   is (Leander.Core.Patterns.Reference (This.Pat));

   function Expression
     (This : Instance)
      return Leander.Core.Expressions.Reference
   is (This.Expr);

end Leander.Core.Alts;
