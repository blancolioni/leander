with Leander.Core.Predicates;
with Leander.Core.Qualifiers;

package Leander.Core.Type_Instances is

   type Instance is new Leander.Core.Abstraction with private;
   type Reference is access constant Instance'Class;
   type Reference_Array is array (Positive range <>) of Reference;

   function Qualifier
     (This : Instance'Class)
      return Leander.Core.Qualifiers.Reference;

   function Predicate
     (This : Instance'Class)
     return Leander.Core.Predicates.Instance;

   function Make_Instance
     (Qualifier : Leander.Core.Qualifiers.Reference;
      Predicate : Leander.Core.Predicates.Instance)
      return Reference;

   function Make_Instance
     (Predicates : Leander.Core.Predicates.Predicate_Array;
      Predicate  : Leander.Core.Predicates.Instance)
      return Reference;

private

   type Instance is new Leander.Core.Abstraction with
      record
         Qualifier : Leander.Core.Qualifiers.Reference;
         Predicate : Leander.Core.Predicates.Instance;
      end record;

   function Qualifier
     (This : Instance'Class)
      return Leander.Core.Qualifiers.Reference
   is (This.Qualifier);

   function Predicate
     (This : Instance'Class)
      return Leander.Core.Predicates.Instance
   is (This.Predicate);

   function Make_Instance
     (Predicates : Leander.Core.Predicates.Predicate_Array;
      Predicate  : Leander.Core.Predicates.Instance)
      return Reference
   is (Make_Instance
       (Leander.Core.Qualifiers.Qualify (Predicates),
          Predicate));

end Leander.Core.Type_instances;
