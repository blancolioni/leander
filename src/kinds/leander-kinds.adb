package body Leander.Kinds is

   ----------------------------------
   -- Create_Variable_From_Binding --
   ----------------------------------

   overriding function Create_Variable_From_Binding
     (Node  : Kind_Annotation;
      Index : Positive)
      return Kind_Annotation
   is
      pragma Unreferenced (Index);
   begin
      return Node;
   end Create_Variable_From_Binding;

   ----------------------------
   -- Set_Anonymous_Variable --
   ----------------------------

   overriding procedure Set_Binding_Index
     (Node  : in out Kind_Node;
      Index : Positive)
   is
   begin
      Node.Class := Binding;
      Node.Index := Index;
   end Set_Binding_Index;

end Leander.Kinds;
