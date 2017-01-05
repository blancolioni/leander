package body Leander.Types is

   ---------
   -- "=" --
   ---------

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

   --------------------
   -- Add_Constraint --
   --------------------

   procedure Add_Constraint
     (Node       : in out Type_Node;
      Constraint : Type_Constraint'Class)
   is
   begin
      if not Node.Constraints.Contains (Constraint) then
         Node.Constraints.Append (Constraint);
      end if;
   end Add_Constraint;

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
   -- Merge_Constraints --
   -----------------------

   overriding procedure Merge_Constraints
     (Left, Right : in out Type_Node)
   is
      New_List : Constraint_Lists.List;
   begin
      for Constraint of Left.Constraints loop
         New_List.Append (Constraint);
      end loop;
      for Constraint of Right.Constraints loop
         if not New_List.Contains (Constraint) then
            New_List.Append (Constraint);
         end if;
      end loop;
      Minimise (New_List);
      Left.Constraints := New_List;
      Right.Constraints := New_List;
   end Merge_Constraints;

   --------------
   -- Minimise --
   --------------

   procedure Minimise
     (List : in out Constraint_Lists.List)
   is
      New_List : Constraint_Lists.List;
   begin
      for Constraint of List loop
         declare
            OK : Boolean := True;
         begin
            for Check of List loop
               if Constraint /= Check
                 and then Check.Is_Subset_Of (Constraint)
               then
                  OK := False;
                  exit;
               end if;
            end loop;
            if OK then
               New_List.Append (Constraint);
            end if;
         end;
      end loop;
      List := New_List;
   end Minimise;

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

   ----------------------
   -- Show_Constraints --
   ----------------------

   function Show_Constraints (List : Constraint_Lists.List) return String is
   begin
      if List.Is_Empty then
         return "";
      elsif Natural (List.Length) = 1 then
         return List.First_Element.Show & " => ";
      else
         declare
            use Ada.Strings.Unbounded;
            Result : Unbounded_String;
         begin
            for Constraint of List loop
               if Result /= Null_Unbounded_String then
                  Result := Result & ",";
               end if;
               Result := Result & Constraint.Show;
            end loop;
            return "(" & To_String (Result) & ") => ";
         end;
      end if;
   end Show_Constraints;

end Leander.Types;
