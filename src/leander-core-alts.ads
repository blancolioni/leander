with Leander.Core.Expressions;
with Leander.Core.Patterns;
with Leander.Core.Typeable;
with Leander.Showable;

package Leander.Core.Alts is

   type Instance is
     new Leander.Showable.Abstraction
     and Leander.Core.Typeable.Abstraction
   with private;

   type Reference is access constant Instance'Class;
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
     access constant Leander.Core.Patterns.Instance'Class;

   type Instance is
     new Leander.Showable.Abstraction
     and Leander.Core.Typeable.Abstraction with
      record
         Id   : Leander.Core.Typeable.Typeable_Id;
         Pat  : Nullable_Pattern_Reference;
         Expr : Leander.Core.Expressions.Reference;
      end record;

   overriding function Show (This : Instance) return String;

   overriding function Get_Id
     (This : Instance)
      return Leander.Core.Typeable.Typeable_Id
   is (This.Id);

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
