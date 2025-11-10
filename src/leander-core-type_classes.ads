with Leander.Core.Binding_Groups;
with Leander.Core.Constraints;

package Leander.Core.Type_Classes is

   type Instance (<>) is tagged private;
   type Reference is access constant Instance'Class;

   function Id (This : Instance'Class) return Conid;

   function Type_Class
     (Class_Name : Conid;
      Variable_Name : Varid;
      Constraints   : Leander.Core.Constraints.Instance_Array;
      Bindings      : Leander.Core.Binding_Groups.Reference)
      return Reference;

private

   type Instance (Constraint_Count : Natural) is tagged
      record
         Class_Id : Conid;
         Var_Id   : Varid;
         Constraints : Core.Constraints.Instance_Array
           (1 .. Constraint_Count);
         Bindings    : Leander.Core.Binding_Groups.Reference;
      end record;

   function Id (This : Instance'Class) return Conid
   is (This.Class_Id);

end Leander.Core.Type_Classes;
