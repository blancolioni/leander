with Leander.Core.Binding_Groups;
with Leander.Core.Constraints;
with Leander.Core.Type_Classes;

package Leander.Syntax.Classes is

   type Builder_Instance is tagged private;

   procedure Start_Class
     (This          : in out Builder_Instance'Class;
      Name          : String;
      Variable_Name : String);

   procedure Add_Constraint
     (This          : in out Builder_Instance'Class;
      Class_Name    : String;
      Variable_Name : String);

   procedure Add_Bindings
     (This      : in out Builder_Instance'Class;
      Bindings  : Leander.Core.Binding_Groups.Reference);

   function Get_Class
     (This : Builder_Instance'Class)
      return Leander.Core.Type_Classes.Reference;

private

   type Builder_Instance is tagged
      record
         Class_Name    : Leander.Core.Conid;
         Variable_Name : Leander.Core.Varid;
         Constraints   : Leander.Core.Constraints.Constraint_Set;
         Bindings      : Leander.Core.Binding_Groups.Reference;
      end record;

end Leander.Syntax.Classes;
