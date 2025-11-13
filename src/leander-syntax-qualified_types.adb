with Leander.Core.Kinds;
with Leander.Core.Predicates;
with Leander.Core.Types;
with Leander.Core.Tyvars;

package body Leander.Syntax.Qualified_Types is

   --------------------
   -- Qualified_Type --
   --------------------

   function Qualified_Type
     (Predicates : Predicate_Array;
      With_Type  : Leander.Syntax.Types.Reference)
      return Reference
   is
   begin
      return Reference
        (Allocate (Instance'
             (Predicate_Count => Predicates'Length,
              Location        => With_Type.Location,
              Predicates      => Predicates,
              T               => With_Type)));
   end Qualified_Type;

   -------------
   -- To_Core --
   -------------

   function To_Core
     (This : Instance'Class)
      return Leander.Core.Qualified_Types.Reference
   is
      Predicates : constant Leander.Core.Predicates.Predicate_Array :=
                     [for P of This.Predicates =>
                                     Leander.Core.Predicates.Predicate
                        (Core.To_String (P.Class),
                         Leander.Core.Types.TVar
                           (Leander.Core.Tyvars.Tyvar
                              (P.Variable, Leander.Core.Kinds.Star)))];
   begin
      return Leander.Core.Qualified_Types.Qualified_Type
        (Predicates, This.T.To_Core);
   end To_Core;

end Leander.Syntax.Qualified_Types;
