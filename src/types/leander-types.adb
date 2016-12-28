package body Leander.Types is

   overriding function "=" (Left, Right : Type_Node) return Boolean is
   begin
      if Left.Class = Right.Class then
         case Left.Class is
            when Binding =>
               return Left.Index = Right.Index;
            when Constructor =>
               return Left.Constructor_Name = Right.Constructor_Name;
            when Variable =>
               return Left.Variable_Name = Right.Variable_Name;
         end case;
      else
         return False;
      end if;
   end "=";

   ----------------------------------
   -- Create_Variable_From_Binding --
   ----------------------------------

   overriding function Create_Variable_From_Binding
     (Node  : Type_Node;
      Index : Positive)
      return Type_Node
   is
      pragma Unreferenced (Node);
      Result : Type_Node;
   begin
      Result.Class := Variable;
      Result.Name :=
        Ada.Strings.Unbounded.To_Unbounded_String
          ((1 => Character'Val (Index + Character'Pos ('a') - 1)));
      return Result;
   end Create_Variable_From_Binding;

   -----------------------
   -- Set_Binding_Index --
   -----------------------

   overriding procedure Set_Binding_Index
     (Node  : in out Type_Node;
      Index : Positive)
   is
      Img : String := Positive'Image (Index);
   begin
      Img (Img'First) := '_';
      Node.Class := Binding;
      Node.Name := Ada.Strings.Unbounded.To_Unbounded_String (Img);
      Node.Index := Index;
   end Set_Binding_Index;

end Leander.Types;
