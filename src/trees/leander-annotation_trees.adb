package body Leander.Annotation_Trees is

   ------------------
   -- Replace_Node --
   ------------------

   procedure Replace_Node
     (Tree : Tree_Type'Class;
      Node : Node_Type)
   is
   begin
      Tree.Node.Node := Node;
   end Replace_Node;

   --------------------
   -- Set_Annotation --
   --------------------

   procedure Set_Annotation
     (Tree       : Tree_Type;
      Annotation : Annotation_Type)
   is
   begin
      Tree.Node.Annotation := Annotation;
      Tree.Node.Has_Annotation := True;
   end Set_Annotation;

   -----------------------
   -- Set_Binding_Index --
   -----------------------

   overriding procedure Set_Binding_Index
     (Tree   : in out Tree_Type;
      Index  : Positive)
   is
   begin
      Tree.Node.Node.Set_Binding_Index (Index);
   end Set_Binding_Index;

   ----------
   -- Show --
   ----------

   overriding function Show (Tree : Tree_Type) return String is
      function Is_Map (T : Tree_Type) return Boolean
      is (T.Is_Application
          and then T.Left.Is_Application
          and then T.Left.Left.Is_Constructor
          and then T.Left.Left.Constructor_Name = "->");

   begin
      if Is_Empty (Tree) then
         return "<>";
      elsif Is_Leaf (Tree) then
         return Get_Node (Tree).Show;
      elsif Is_Map (Tree) then
         if Is_Map (Tree.Left.Right) then
            return "(" & Tree.Left.Right.Show & ") -> " & Tree.Right.Show;
         else
            return Tree.Left.Right.Show & " -> " & Tree.Right.Show;
         end if;
      elsif Is_Leaf (Right (Tree)) then
         return Show (Left (Tree)) & " " & Show (Right (Tree));
      else
         return Show (Left (Tree)) & " (" & Show (Right (Tree)) & ")";
      end if;
   end Show;

   ---------------------------
   -- Show_With_Annotations --
   ---------------------------

   function Show_With_Annotations (Tree : Tree_Type) return String is
      Annotation_Img : constant String :=
                         (if Tree.Has_Annotation
                          then "::" & Tree.Annotation.Show
                          else "");
      Tree_Img       : constant String :=
                         (if Is_Empty (Tree)
                          then "<>"
                          elsif Is_Leaf (Tree)
                          then Get_Node (Tree).Show
                          elsif Is_Leaf (Right (Tree))
                          then Show_With_Annotations (Left (Tree))
                          & " " & Show_With_Annotations (Right (Tree))
                          else Show_With_Annotations (Left (Tree))
                          & " (" & Show_With_Annotations (Right (Tree)) & ")");
   begin
      return "(" & Tree_Img & Annotation_Img & ")";
   end Show_With_Annotations;

end Leander.Annotation_Trees;
