with Leander.Showable;

package Leander.Core.Constraints is

   type Instance is new Leander.Showable.Abstraction
   with private;

   type Instance_Array is array (Positive range <>) of Instance;

   function Constraint
     (Class_Name    : String;
      Variable_Name : String)
      return Instance;

private

   type Instance is new Leander.Showable.Abstraction with
      record
         Class_Name : Conid;
         Var_Name   : Varid;
      end record;

   overriding function Show (This : Instance) return String;

end Leander.Core.Constraints;
