package body Leander.Kinds is

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
