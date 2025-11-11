package body Leander.Syntax.Classes is

   ------------------
   -- Add_Bindings --
   ------------------

   procedure Add_Bindings
     (This      : in out Builder_Instance'Class;
      Bindings  : Leander.Core.Binding_Groups.Reference)
   is
   begin
      This.Bindings := Bindings;
   end Add_Bindings;

   --------------------
   -- Add_Constraint --
   --------------------

   procedure Add_Constraint
     (This          : in out Builder_Instance'Class;
      Class_Name    : String;
      Variable_Name : String)
   is
   begin
      This.Constraints :=
        This.Constraints.Constrain (Class_Name, Variable_Name);
   end Add_Constraint;

   ---------------
   -- Get_Class --
   ---------------

   function Get_Class
     (This : Builder_Instance'Class)
      return Leander.Core.Type_Classes.Reference
   is
   begin
      return Leander.Core.Type_Classes.Type_Class
        (Class_Name    => This.Class_Name,
         Variable_Name => This.Variable_Name,
         Constraints   => This.Constraints,
         Bindings      => This.Bindings);
   end Get_Class;

   -----------------
   -- Start_Class --
   -----------------

   procedure Start_Class
     (This          : in out Builder_Instance'Class;
      Name          : String;
      Variable_Name : String)
   is
   begin
      This.Class_Name := Core.To_Conid (Name);
      This.Variable_Name := Core.To_Varid (Variable_Name);
   end Start_Class;

end Leander.Syntax.Classes;
