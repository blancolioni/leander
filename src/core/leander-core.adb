package body Leander.Core is

   Next_New_Variable : Positive := 1;

   ------------------
   -- New_Variable --
   ------------------

   function New_Variable
     (Source : Leander.Source.Source_Reference)
      return Core_Node
   is
      Name : constant String := "v" & Integer'Image (-Next_New_Variable);
   begin
      Next_New_Variable := Next_New_Variable + 1;
      return Variable (Source, Name);
   end New_Variable;

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
         when Let =>
            return "let {";
         when Algebraic_Case =>
            return "acase {";
         when Primitive_Case =>
            return "pcase {";
      end case;
   end Show;

end Leander.Core;
