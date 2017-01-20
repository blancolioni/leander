package body Leander.Annotation_Trees is

   ----------------
   -- Clean_Copy --
   ----------------

   function Clean_Copy (Tree : Tree_Type) return Tree_Type is
   begin
      if Tree.Is_Empty then
         return Empty;
      elsif Tree.Is_Leaf then
         declare
            Result : constant Tree_Type :=
                       Leaf (Tree.Node.Node);
         begin
            if Tree.Is_Primitive then
               Result.Set_Annotation (Tree.Annotation);
            end if;
            return Result;
         end;
      else
         return Apply (Tree.Left.Clean_Copy, Tree.Right.Clean_Copy);
      end if;
   end Clean_Copy;

   -----------------------
   -- Merge_Constraints --
   -----------------------

   overriding procedure Merge_Constraints
     (Left, Right : in out Tree_Type)
   is
   begin
      Left.Merge_Tree_Constraints (Right);
   end Merge_Constraints;

   ----------------------------
   -- Merge_Tree_Constraints --
   ----------------------------

   procedure Merge_Tree_Constraints
     (Left, Right : Tree_Type)
   is
   begin
      if Left.Node.Leaf and then Right.Node.Leaf then
         Left.Node.Node.Merge_Constraints (Right.Node.Node);
      end if;
   end Merge_Tree_Constraints;

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
      else
         declare
            Show_Left : constant String := Show (Left (Tree));
         begin
            if Show_Left (Show_Left'Last) = '{' then
               return Show_Left & Show (Right (Tree)) & "}";
            elsif Is_Leaf (Right (Tree)) then
               return Show_Left & " " & Show (Right (Tree));
            else
               return Show_Left & " (" & Show (Right (Tree)) & ")";
            end if;
         end;
      end if;
   end Show;

   ---------------------------
   -- Show_With_Annotations --
   ---------------------------

   function Show_With_Annotations (Tree : Tree_Type) return String is
      Annotation_Img : constant String :=
                         (if Tree.Has_Annotation
                          then "::" & Tree.Annotation.Show
                          else "::?");
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
