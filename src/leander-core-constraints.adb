package body Leander.Core.Constraints is

   ----------------
   -- Constraint --
   ----------------

   function Constraint
     (Class_Name    : String;
      Variable_Name : String)
      return Instance
   is
   begin
      return Instance'
        (To_Conid (Class_Name), To_Varid (Variable_Name));
   end Constraint;

   ----------
   -- Show --
   ----------

   overriding function Show (This : Instance) return String is
   begin
      return To_String (This.Class_Name) & " " & To_String (This.Var_Name);
   end Show;


end Leander.core.Constraints;
