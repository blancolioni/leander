with Leander.Core.Kinds;
with Leander.Core.Types;
with Leander.Core.Tyvars;

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
      This.Constraints.Append
        (Leander.Core.Predicates.Predicate
           (Class_Name,
            Leander.Core.Types.TVar
              (Leander.Core.Tyvars.Tyvar
                   (Core.To_Varid (Variable_Name),
                    Leander.Core.Kinds.Star))));
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
         Predicates    => [for P of This.Constraints => P],
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
