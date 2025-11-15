private with Ada.Containers.Doubly_Linked_Lists;
with Leander.Core.Predicates;
private with Leander.Core.Substitutions;
with Leander.Core.Tyvars;
with Leander.Core.Types;
with Leander.Showable;

package Leander.Core.Qualifiers is

   type Instance is
     new Leander.Core.Abstraction
     and Leander.Showable.Abstraction
     and Leander.Core.Tyvars.Container_Abstraction
   with private;

   type Reference is access constant Instance'Class;

   function Empty return Reference;

   function Instantiate
     (This : not null access constant Instance'Class;
      Refs : Leander.Core.Types.Type_Array)
      return Reference;

   function Predicates
     (This : Instance'Class)
      return Leander.Core.Predicates.Predicate_Array;

   function Qualify
     (Predicates : Leander.Core.Predicates.Predicate_Array)
     return Reference;

private

   package Predicate_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Leander.Core.Predicates.Instance,
        Leander.Core.Predicates."=");

   type Instance is
     new Leander.Core.Abstraction
     and Leander.Showable.Abstraction
     and Leander.Core.Tyvars.Container_Abstraction with
      record
         Predicates : Predicate_Lists.List;
      end record;

   overriding function Show (This : Instance) return String;

   overriding function Contains
     (This  : Instance;
      Tyvar : Leander.Core.Tyvars.Instance'Class)
      return Boolean
   is (for some P of This.Predicates => P.Get_Type.Contains (Tyvar));

   overriding function Get_Tyvars
     (This  : Instance)
      return Leander.Core.Tyvars.Tyvar_Array;

   overriding function Apply
     (This  : not null access constant Instance;
      Subst : Leander.Core.Substitutions.Instance'Class)
      return access constant Instance;

   function Predicates
     (This : Instance'Class)
      return Leander.Core.Predicates.Predicate_Array
   is ([for P of This.Predicates => P]);

   Local_Empty : aliased constant Instance := (Predicates => []);
   function Empty return Reference is (Local_Empty'Access);

end Leander.Core.Qualifiers;
