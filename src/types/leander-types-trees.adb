with Leander.Set_Of_Names;

package body Leander.Types.Trees is

   ----------------------
   -- Scan_Constraints --
   ----------------------

   procedure Scan_Constraints
     (Tree : Tree_Type;
      Process : not null access
        procedure (Constraint : Type_Constraint'Class;
                   Variable   : String))
   is
      Done : Set_Of_Names.Set;

      procedure Scan_Tree (T : Tree_Type);

      ---------------
      -- Scan_Tree --
      ---------------

      procedure Scan_Tree (T : Tree_Type) is
      begin
         if T.Is_Leaf then
            if T.Is_Variable
              and then not Done.Contains (T.Variable_Name)
            then
               Done.Insert (T.Variable_Name);
               for Constraint of T.Get_Node.Constraints loop
                  Process (Constraint, T.Variable_Name);
               end loop;
            end if;
         elsif T.Is_Application then
            Scan_Tree (T.Left);
            Scan_Tree (T.Right);
         end if;
      end Scan_Tree;

   begin
      Scan_Tree (Tree);
   end Scan_Constraints;

   ---------------
   -- Show_Type --
   ---------------

   function Show_Type
     (Tree : Tree_Type)
      return String
   is
      use Ada.Strings.Unbounded;
      Constraint_String : Unbounded_String;
      Constraint_Count  : Natural := 0;
      Type_Image        : constant String := Tree.Show;

      procedure Add_Constraint_Image
        (Constraint : Type_Constraint'Class;
         Variable   : String);

      --------------------------
      -- Add_Constraint_Image --
      --------------------------

      procedure Add_Constraint_Image
        (Constraint : Type_Constraint'Class;
         Variable   : String)
      is
         Constraint_Image : constant String :=
                              Constraint.Show & " " & Variable;
      begin
         Constraint_Count := Constraint_Count + 1;
         if Constraint_Count = 1 then
            Constraint_String := To_Unbounded_String (Constraint_Image);
         else
            Constraint_String := Constraint_String
              & "," & Constraint_Image;
         end if;
      end Add_Constraint_Image;

   begin
      Scan_Constraints (Tree, Add_Constraint_Image'Access);
      if Constraint_Count = 1 then
         return To_String (Constraint_String) & " => " & Type_Image;
      elsif Constraint_Count > 1 then
         return "(" & To_String (Constraint_String) & ") => " & Type_Image;
      else
         return Type_Image;
      end if;
   end Show_Type;

end Leander.Types.Trees;
