with Leander.Core.Expressions;
with Leander.Core.Patterns;
with Leander.Core.Typeable;
with Leander.Showable;

package Leander.Core.Alts is

   type Instance (<>) is
     new Leander.Showable.Abstraction
     and Leander.Core.Typeable.Abstraction
   with private;

   type Reference is access constant Instance'Class;
   type Reference_Array is array (Positive range <>) of Reference;

   function Alt
     (Pats : Leander.Core.Patterns.Reference_Array;
      Expr : Leander.Core.Expressions.Reference)
      return Reference;

   function Patterns
     (This : Instance)
      return Leander.Core.Patterns.Reference_Array;

   function Expression
     (This : Instance)
      return Leander.Core.Expressions.Reference;

   function Has_Reference
     (This : Instance;
      Name : Varid)
      return Boolean;

   procedure Prune;

private

   type Instance (Pat_Count : Natural) is
     new Leander.Showable.Abstraction
     and Leander.Core.Typeable.Abstraction with
      record
         Id   : Leander.Core.Typeable.Typeable_Id;
         Pats : Leander.Core.Patterns.Reference_Array (1 .. Pat_Count);
         Expr : Leander.Core.Expressions.Reference;
      end record;

   overriding function Show (This : Instance) return String;

   overriding function Get_Id
     (This : Instance)
      return Leander.Core.Typeable.Typeable_Id
   is (This.Id);

   function Patterns
     (This : Instance)
      return Leander.Core.Patterns.Reference_Array
   is (This.Pats);

   function Expression
     (This : Instance)
      return Leander.Core.Expressions.Reference
   is (This.Expr);

end Leander.Core.Alts;
