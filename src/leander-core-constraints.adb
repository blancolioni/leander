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

   ----------------
   -- Constraint --
   ----------------

   function Constrain
     (This          : Constraint_Set;
      Class_Name    : String;
      Variable_Name : String)
      return Constraint_Set
   is
   begin
      return Result : Constraint_Set := This do
         Result.Constraints.Append (Constraint (Class_Name, Variable_Name));
      end return;
   end Constrain;

   ----------
   -- Show --
   ----------

   overriding function Show (This : Instance) return String is
   begin
      return To_String (This.Class_Name) & " " & To_String (This.Var_Name);
   end Show;

   ----------
   -- Show --
   ----------

   overriding function Show (This : Constraint_Set) return String is

      function Show (Position : Constraint_Lists.Cursor) return String;

      ----------
      -- Show --
      ----------

      function Show (Position : Constraint_Lists.Cursor) return String is
         use Constraint_Lists;
      begin
         if Has_Element (Next (Position)) then
            return Element (Position).Show & "," & Show (Next (Position));
         else
            return Element (Position).Show;
         end if;
      end Show;

   begin
      case This.Constraints.Length is
         when 0 =>
            return "";
         when 1 =>
            return This.Constraints.First_Element.Show;
         when others =>
            return "(" & Show (This.Constraints.First) & ")";
      end case;
   end Show;

end Leander.core.Constraints;
