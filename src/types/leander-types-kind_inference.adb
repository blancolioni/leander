with Ada.Strings.Fixed.Hash;

with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;

with Leander.Types.Bindings;
with Leander.Types.Class_Constraints.Annotation;

with Leander.Logging;

package body Leander.Types.Kind_Inference is

   type Array_Of_Annotations is
     array (Positive range <>) of Leander.Kinds.Trees.Tree_Type;

   package Binding_Vectors is
     new Ada.Containers.Vectors
       (Positive, Leander.Kinds.Trees.Tree_Type, Leander.Kinds.Trees."=");

   package Binding_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Positive,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=");

   function Scan_Variable_Annotations
     (Tree     : Trees.Tree_Type;
      Env      : Leander.Environments.Environment)
      return Array_Of_Annotations;

   function Unify
     (Left_Annotation  : Leander.Kinds.Trees.Tree_Type;
      Right_Annotation : Leander.Kinds.Trees.Tree_Type;
      Bindings         : in out Array_Of_Annotations)
      return Leander.Kinds.Trees.Tree_Type;

   function Bind
     (Annotation    : Leander.Kinds.Trees.Tree_Type;
      Bindings      : in out Array_Of_Annotations;
      Next_Variable : in out Natural)
      return Leander.Kinds.Trees.Tree_Type;

   function Dereference
     (Annotation : Leander.Kinds.Trees.Tree_Type;
      Bindings   : Array_Of_Annotations)
      return Leander.Kinds.Trees.Tree_Type;

   --------------
   -- Annotate --
   --------------

   procedure Annotate
     (Env       : Leander.Environments.Environment;
      Root_Type : Leander.Types.Trees.Tree_Type)
   is
      Vars : Array_Of_Annotations :=
               Scan_Variable_Annotations
                 (Root_Type, Env);

      Next_Variable : Natural := 0;

      procedure Unify
        (Tree       : Trees.Tree_Type);

      procedure Bind
        (Tree       : Trees.Tree_Type);

      ----------
      -- Bind --
      ----------

      procedure Bind
        (Tree       : Trees.Tree_Type)
      is
      begin
         --           Leander.Logging.Log
         --             ("bind: " & Tree.Show_With_Annotations);
         if Tree.Is_Application then
            Bind (Tree.Left);
            Bind (Tree.Right);
         end if;
         if Tree.Has_Annotation then
            Tree.Set_Annotation
              (Bind (Tree.Annotation, Vars, Next_Variable));
         end if;
      end Bind;

      -----------
      -- Unify --
      -----------

      procedure Unify
        (Tree       : Trees.Tree_Type)
      is
      begin
--           Leander.Logging.Log
--             ("enter unify: " & Tree.Show_With_Annotations);
         if Tree.Is_Application then
            Unify (Tree.Right);
            declare
               Left_A  : constant Leander.Kinds.Trees.Tree_Type :=
                           Map_Operator.Apply
                             (Tree.Right.Annotation)
                             .Apply (Tree.Annotation);
               Unified : constant Leander.Kinds.Trees.Tree_Type :=
                           Unify (Left_A, Tree.Left.Annotation, Vars);
            begin
               if Unified.Is_Empty then
                  raise Constraint_Error with "kind error: "
                    & Tree.Show & ": "
                    & Left_A.Show
                    & " with " & Tree.Left.Annotation.Show;
               end if;
               Tree.Left.Set_Annotation
                 (Unified);
               Unify (Tree.Left);
            end;
         elsif Tree.Is_Variable then
            if Env.Has_Type_Variable_Binding (Tree.Variable_Name) then
               Env.Type_Variable_Binding (Tree.Variable_Name).Set_Annotation
                 (Tree.Annotation);
            end if;
         end if;
--           Leander.Logging.Log
--             ("leave unify: " & Tree.Show_With_Annotations);
      end Unify;

   begin
      Unify (Root_Type);
      Bind (Root_Type);
   end Annotate;

   ----------
   -- Bind --
   ----------

   function Bind
     (Annotation    : Leander.Kinds.Trees.Tree_Type;
      Bindings      : in out Array_Of_Annotations;
      Next_Variable : in out Natural)
      return Leander.Kinds.Trees.Tree_Type
   is
      use Leander.Kinds.Trees;
      A : constant Tree_Type :=
            Dereference (Annotation, Bindings);
   begin
      if A.Is_Leaf then
         if A.Is_Binding then
            if Bindings (A.Binding_Index).Is_Binding then
               declare
                  Index    : constant Positive := A.Binding_Index;
                  Var_Type : Leander.Kinds.Trees.Tree_Type;
               begin
                  Next_Variable := Next_Variable + 1;
                  Var_Type :=
                    A.Create_Variable_From_Binding (Next_Variable);
                  Bindings (Index).Merge_Constraints
                    (Var_Type);
                  Bindings (Index) := Var_Type;
               end;
            end if;
            return Bindings (A.Binding_Index);
         else
            return A;
         end if;
      else
         declare
            Left  : constant Tree_Type :=
                      Bind (A.Left, Bindings, Next_Variable);
            Right : constant Tree_Type :=
                      Bind (A.Right, Bindings, Next_Variable);
         begin
            return Left.Apply (Right);
         end;
      end if;
   end Bind;

   -----------------
   -- Dereference --
   -----------------

   function Dereference
     (Annotation : Leander.Kinds.Trees.Tree_Type;
      Bindings   : Array_Of_Annotations)
      return Leander.Kinds.Trees.Tree_Type
   is
   begin
      if Annotation.Is_Application then
         return Annotation;
      elsif Annotation.Is_Binding then
         declare
            use type Leander.Kinds.Trees.Tree_Type;
            Value : constant Leander.Kinds.Trees.Tree_Type :=
                      Bindings (Annotation.Binding_Index);
         begin
            if Value = Annotation then
               return Value;
            else
               return Dereference (Value, Bindings);
            end if;
         end;
      else
         return Annotation;
      end if;
   end Dereference;

   -----------------
   -- Infer_Kinds --
   -----------------

   procedure Infer_Kinds
     (Env       : Leander.Environments.Environment)
   is
      procedure Annotate_Class
        (Binding : Leander.Types.Class_Constraints.Class_Constraint'Class);

      procedure Annotate_Type_Body
        (Name    : String;
         Binding : Leander.Types.Bindings.Type_Binding'Class);

      procedure Annotate_Type_Declaration
        (Name    : String;
         Binding : Leander.Types.Bindings.Type_Binding'Class);

      --------------------
      -- Annotate_Class --
      --------------------

      procedure Annotate_Class
        (Binding : Leander.Types.Class_Constraints.Class_Constraint'Class)
      is
      begin
         Leander.Types.Class_Constraints.Annotation.Annotate (Binding, Env);
      end Annotate_Class;

      ------------------------
      -- Annotate_Type_Body --
      ------------------------

      procedure Annotate_Type_Body
        (Name    : String;
         Binding : Leander.Types.Bindings.Type_Binding'Class)
      is
         pragma Unreferenced (Name);
         Type_Env : Leander.Environments.Environment;
         Type_Pat : Leander.Types.Trees.Tree_Type :=
                      Binding.Type_Pattern;
      begin
         if Binding.Is_Algebraic then
            Type_Env.Create_Temporary_Environment
              (Parent => Env,
               Name   => Binding.Type_Name);
            while Type_Pat.Is_Application loop
               Type_Env.Insert_Type_Variable
                 (Type_Pat.Right.Variable_Name,
                  Type_Pat.Right);
               Type_Pat := Type_Pat.Left;
            end loop;
            for I in 1 .. Binding.Constructor_Count loop
               Annotate (Type_Env, Binding.Constructor_Type (I));
               Leander.Logging.Log
                 (Binding.Constructor_Name (I)
                  & " :: "
                  & Binding.Constructor_Type (I).Show);
            end loop;
         end if;
      end Annotate_Type_Body;

      -------------------------------
      -- Annotate_Type_Declaration --
      -------------------------------

      procedure Annotate_Type_Declaration
        (Name    : String;
         Binding : Leander.Types.Bindings.Type_Binding'Class)
      is
      begin
         if Binding.Is_Algebraic  then
            Binding.Annotate_Type_Constructor;
         end if;
         Leander.Logging.Log
           (Name & " :: " & Binding.Kind.Show);
      end Annotate_Type_Declaration;

   begin
      Env.Scan_Local_Type_Bindings
        (Annotate_Type_Declaration'Access);
      Env.Scan_Local_Type_Bindings
        (Annotate_Type_Body'Access);
      Env.Scan_Local_Class_Bindings
        (Annotate_Class'Access);
   end Infer_Kinds;

   -------------------------------
   -- Scan_Variable_Annotations --
   -------------------------------

   function Scan_Variable_Annotations
     (Tree     : Trees.Tree_Type;
      Env      : Leander.Environments.Environment)
      return Array_Of_Annotations
   is
      Vector : Binding_Vectors.Vector;
      Map    : Binding_Maps.Map;

      procedure Add_Binding;

      procedure Scan_Variables
        (Root   : Trees.Tree_Type);

      function Unbind
        (Bound_Type : Leander.Kinds.Trees.Tree_Type)
         return Leander.Kinds.Trees.Tree_Type;

      -----------------
      -- Add_Binding --
      -----------------

      procedure Add_Binding is
         Binding : constant Leander.Kinds.Trees.Tree_Type :=
                     Leander.Kinds.Trees.Leaf
                       (Leander.Kinds.Binding
                          (Vector.Last_Index + 1));
      begin
         Vector.Append (Binding);
      end Add_Binding;

      --------------------
      -- Scan_Variables --
      --------------------

      procedure Scan_Variables
        (Root   : Trees.Tree_Type)
      is

         procedure Set_Named_Binding (Name : String);

         -----------------------
         -- Set_Named_Binding --
         -----------------------

         procedure Set_Named_Binding (Name : String) is
         begin
            if not Map.Contains (Name) then
               Add_Binding;
               Map.Insert (Name, Vector.Last_Index);
            end if;

            Root.Set_Annotation
              (Vector.Element (Map.Element (Name)));
         end Set_Named_Binding;

      begin
         if Root.Is_Application then
            Add_Binding;
            Root.Set_Annotation (Vector.Last_Element);
            Scan_Variables (Root.Left);
            Scan_Variables (Root.Right);
         elsif Root.Is_Variable then
            declare
               Name : constant String := Root.Variable_Name;
            begin
               if Env.Has_Type_Variable_Binding (Name) then
                  Root.Set_Annotation
                    (Unbind
                       (Env.Type_Variable_Binding (Name).Annotation));
               else
                  Set_Named_Binding (Name);
               end if;
            end;
         elsif Root.Is_Constructor then
            declare
               Name : constant String := Root.Constructor_Name;
            begin
               if Env.Has_Type_Constructor_Binding (Name) then
                  Root.Set_Annotation
                    (Unbind (Env.Type_Constructor_Binding (Name).Kind));
               else
                  raise Constraint_Error with
                    "undefined: " & Name;
               end if;
            end;
         end if;
      end Scan_Variables;

      ------------
      -- Unbind --
      ------------

      function Unbind
        (Bound_Type : Leander.Kinds.Trees.Tree_Type)
         return Leander.Kinds.Trees.Tree_Type
      is
         Local_Map : Binding_Maps.Map;

         function Go (T : Leander.Kinds.Trees.Tree_Type)
                      return Leander.Kinds.Trees.Tree_Type;

         --------
         -- Go --
         --------

         function Go (T : Leander.Kinds.Trees.Tree_Type)
                      return Leander.Kinds.Trees.Tree_Type
         is
         begin
            if T.Is_Application then
               return Go (T.Left).Apply (Go (T.Right));
            elsif T.Is_Variable then
               if Local_Map.Contains (T.Variable_Name) then
                  return Vector (Local_Map.Element (T.Variable_Name));
               else
                  Add_Binding;
                  T.Merge_Tree_Constraints (Vector.Last_Element);
                  Local_Map.Insert (T.Variable_Name, Vector.Last_Index);
                  return Vector.Last_Element;
               end if;
            else
               return T;
            end if;
         end Go;

         T : constant Leander.Kinds.Trees.Tree_Type := Go (Bound_Type);
      begin
         return T;
      end Unbind;

   begin
      Scan_Variables (Tree);
      declare
         Result : Array_Of_Annotations (1 .. Vector.Last_Index);
      begin
         for I in Result'Range loop
            Result (I) := Vector (I);
         end loop;
         return Result;
      end;
   end Scan_Variable_Annotations;

   -----------
   -- Unify --
   -----------

   function Unify
     (Left_Annotation  : Leander.Kinds.Trees.Tree_Type;
      Right_Annotation : Leander.Kinds.Trees.Tree_Type;
      Bindings         : in out Array_Of_Annotations)
      return Leander.Kinds.Trees.Tree_Type
   is
      use Leander.Kinds.Trees;
      Left    : constant Leander.Kinds.Trees.Tree_Type :=
                  Dereference (Left_Annotation, Bindings);
      Right   : constant Leander.Kinds.Trees.Tree_Type :=
                  Dereference (Right_Annotation, Bindings);
   begin
      --        Leander.Logging.Log
      --          ("Unify: " & Left.Show & " with " & Right.Show);
      if Left.Is_Binding then
         Left.Merge_Tree_Constraints (Right);
         Bindings (Left.Binding_Index) := Right;
         return Right;
      elsif Right.Is_Binding then
         Left.Merge_Tree_Constraints (Right);
         Bindings (Right.Binding_Index) := Left;
         return Left;
      elsif Left.Is_Leaf then
         if Right.Is_Leaf and then Left = Right then
            return Left;
         else
            return Empty;
         end if;
      elsif Right.Is_Leaf then
         return Empty;
      else
         declare
            New_Left  : constant Tree_Type :=
                          Unify (Left.Left, Right.Left, Bindings);
            New_Right : constant Tree_Type :=
                          Unify (Left.Right, Right.Right, Bindings);
         begin
            if New_Left.Is_Empty or else New_Right.Is_Empty then
               return Empty;
            else
               return New_Left.Apply (New_Right);
            end if;
         end;
      end if;

   end Unify;

end Leander.Types.Kind_Inference;
