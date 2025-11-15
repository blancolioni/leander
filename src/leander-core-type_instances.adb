package body Leander.Core.Type_Instances is

   function Allocate
     (This : Instance)
      return Reference
   is (Reference (Core.Allocate (This)));

   -------------------
   -- Make_Instance --
   -------------------

   function Make_Instance
     (Qualifier : Leander.Core.Qualifiers.Reference;
      Predicate : Leander.Core.Predicates.Instance)
      return Reference
   is
   begin
      return Allocate
        (Instance'
           (Qualifier, Predicate));
   end Make_Instance;

end Leander.Core.Type_Instances;
