package body Leander.Syntax.Qualified_Types is

   function Allocate
     (This : Instance)
      return Reference
   is
   begin
      return Reference (Syntax.Allocate (This));
   end Allocate;

   --------------------
   -- Qualified_Type --
   --------------------

   function Qualified_Type
     (Predicates : Leander.Core.Predicates.Predicate_Array;
      With_Type  : Leander.Syntax.Types.Reference)
      return Reference
   is
   begin
      return Allocate (Instance'
               (Predicate_Count => Predicates'Length,
                Location        => With_Type.Location,
                Predicates      => Predicates,
                T               => With_Type));
   end Qualified_Type;

   -------------
   -- To_Core --
   -------------

   function To_Core
     (This : Instance'Class)
      return Leander.Core.Qualified_Types.Reference
   is
   begin
      return Leander.Core.Qualified_Types.Qualified_Type
        (This.Predicates, This.T.To_Core);
   end To_Core;

end Leander.Syntax.Qualified_Types;
