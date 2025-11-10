package body Leander.Core.Type_Classes is

   ----------------
   -- Type_Class --
   ----------------

   function Type_Class
     (Class_Name    : Conid;
      Variable_Name : Varid;
      Constraints   : Leander.Core.Constraints.Instance_Array;
      Bindings      : Leander.Core.Binding_Groups.Reference)
      return Reference
   is
   begin
      return new Instance'
        (Constraint_Count => Constraints'Length,
         Class_Id         => Class_Name,
         Var_Id           => Variable_Name,
         Constraints      => Constraints,
         Bindings         => Bindings);
   end Type_Class;

end Leander.Core.Type_Classes;
