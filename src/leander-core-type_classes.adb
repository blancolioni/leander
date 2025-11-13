package body Leander.Core.Type_Classes is

   ----------------
   -- Type_Class --
   ----------------

   function Type_Class
     (Class_Name    : Conid;
      Variable_Name : Varid;
      Predicates    : Leander.Core.Predicates.Predicate_Array;
      Bindings      : Leander.Core.Binding_Groups.Reference)
      return Reference
   is
   begin
      return new Instance'
        (Predicate_Count  => Predicates'Length,
         Class_Id         => Class_Name,
         Var_Id           => Variable_Name,
         Predicates       => Predicates,
         Bindings         => Bindings);
   end Type_Class;

end Leander.Core.Type_Classes;
