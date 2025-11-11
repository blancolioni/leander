private with Ada.Containers.Doubly_Linked_Lists;
with Leander.Showable;

package Leander.Core.Constraints is

   type Instance is new Leander.Showable.Abstraction
   with private;

   function Constraint
     (Class_Name    : String;
      Variable_Name : String)
      return Instance;

   type Constraint_Set is new Leander.Showable.Abstraction with private;

   function Empty return Constraint_Set;

   function Constrain
     (This          : Constraint_Set;
      Class_Name    : String;
      Variable_Name : String)
      return Constraint_Set;

private

   type Instance is new Leander.Showable.Abstraction with
      record
         Class_Name : Conid;
         Var_Name   : Varid;
      end record;

   overriding function Show (This : Instance) return String;

   package Constraint_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Instance);

   type Constraint_Set is new Leander.Showable.Abstraction with
      record
         Constraints : Constraint_Lists.List;
      end record;

   overriding function Show (This : Constraint_Set) return String;

   function Empty return Constraint_Set is (Constraints => []);

end Leander.Core.Constraints;
