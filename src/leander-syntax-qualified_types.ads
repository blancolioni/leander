with Leander.Core.Predicates;
with Leander.Core.Qualified_Types;
with Leander.Syntax.Types;

package Leander.Syntax.Qualified_Types is

   subtype Parent is Leander.Syntax.Instance;
   type Instance (<>) is new Parent with private;
   type Reference is access constant Instance'Class;

   function Predicates
     (This : Instance'Class)
      return Leander.Core.Predicates.Predicate_Array;

   function Get_Type
     (This : Instance'Class)
      return Leander.Syntax.Types.Reference;

   function To_Core
     (This : Instance'Class)
      return Leander.Core.Qualified_Types.Reference;

   function Qualified_Type
     (Predicates : Leander.Core.Predicates.Predicate_Array;
      With_Type  : Leander.Syntax.Types.Reference)
      return Reference;

private

   type Instance (Predicate_Count : Natural) is new Parent with
      record
         Predicates : Leander.Core.Predicates.Predicate_Array
           (1 .. Predicate_Count);
         T          : Leander.Syntax.Types.Reference;
      end record;

   function Predicates
     (This : Instance'Class)
      return Leander.Core.Predicates.Predicate_Array
   is (This.Predicates);

   function Get_Type
     (This : Instance'Class)
      return Leander.Syntax.Types.Reference
   is (This.T);

end Leander.Syntax.Qualified_Types;
