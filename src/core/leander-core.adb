package body Leander.Core is

   ----------
   -- Show --
   ----------

   overriding function Show
     (Node : Core_Node)
      return String
   is
   begin
      case Node.Class is
         when Constructor | Variable | Literal =>
            return -Node.Name;
         when Lambda =>
            return "\" & (-Node.Name) & " ->";
         when Algebraic_Case =>
            return "acase";
         when Primitive_Case =>
            return "pcase";
      end case;
   end Show;

end Leander.Core;
