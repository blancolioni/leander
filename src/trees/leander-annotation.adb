with Ada.Strings.Fixed.Hash;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;

package body Leander.Annotation is

   type Array_Of_Annotations is
     array (Positive range <>) of Trees.Annotation_Type;

   function Scan_Variable_Annotations
     (Tree : Trees.Tree_Type)
      return Array_Of_Annotations;

   package Binding_Vectors is
     new Ada.Containers.Vectors (Positive, Trees.Annotation_Type, Trees."=");

   package Binding_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Positive,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=");

   function Unify
     (Left_Annotation  : Trees.Annotation_Type;
      Right_Annotation : Trees.Annotation_Type;
      Bindings         : in out Array_Of_Annotations)
      return Trees.Annotation_Type;

   function Bind
     (Annotation    : Trees.Annotation_Type;
      Bindings      : in out Array_Of_Annotations;
      Next_Variable : in out Natural)
      return Trees.Annotation_Type;

   function Dereference
     (Annotation : Trees.Annotation_Type;
      Bindings   : Array_Of_Annotations)
      return Trees.Annotation_Type;

   procedure Scan_Variables
     (Root   : Trees.Tree_Type;
      Vector : in out Binding_Vectors.Vector;
      Map    : in out Binding_Maps.Map);

   --------------
   -- Annotate --
   --------------

   procedure Annotate
     (Tree : Trees.Tree_Type;
      Env  : Leander.Environments.Environment)
   is
      pragma Unreferenced (Env);

      Vars : Array_Of_Annotations :=
        Scan_Variable_Annotations (Tree);

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
         if Tree.Is_Application then
            Unify (Tree.Right);

            declare
               Left_A  : constant Trees.Annotation_Type :=
                           Map_Operator.Apply
                             (Tree.Right.Annotation)
                             .Apply (Tree.Annotation);
            begin
               Tree.Left.Set_Annotation
                 (Unify (Left_A, Tree.Left.Annotation, Vars));
               Unify (Tree.Left);
            end;

         end if;
      end Unify;

   begin
      Unify (Tree);
      Bind (Tree);
   end Annotate;

   ----------
   -- Bind --
   ----------

   function Bind
     (Annotation    : Trees.Annotation_Type;
      Bindings      : in out Array_Of_Annotations;
      Next_Variable : in out Natural)
      return Trees.Annotation_Type
   is
      use Trees;
      A : constant Trees.Annotation_Type :=
            Dereference (Annotation, Bindings);
   begin
      if A.Is_Leaf then
         if A.Is_Binding then
            if Bindings (A.Binding_Index).Is_Binding then
               declare
                  Index    : constant Positive := A.Binding_Index;
               begin
                  Next_Variable := Next_Variable + 1;
                  Bindings (Index) :=
                    A.Create_Variable_From_Binding (Next_Variable);
               end;
            end if;
            return Bindings (A.Binding_Index);
         else
            return A;
         end if;
      else
         declare
            Left  : constant Annotation_Type :=
                      Bind (A.Left, Bindings, Next_Variable);
            Right : constant Annotation_Type :=
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
     (Annotation : Trees.Annotation_Type;
      Bindings   : Array_Of_Annotations)
      return Trees.Annotation_Type
   is
   begin
      if Annotation.Is_Application then
         return Annotation;
      elsif Annotation.Is_Binding then
         declare
            use type Trees.Annotation_Type;
            Value : constant Trees.Annotation_Type :=
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

   -------------------------------
   -- Scan_Variable_Annotations --
   -------------------------------

   function Scan_Variable_Annotations
     (Tree : Trees.Tree_Type)
      return Array_Of_Annotations
   is
      Vector : Binding_Vectors.Vector;
      Map    : Binding_Maps.Map;
   begin
      Scan_Variables (Tree, Vector, Map);
      declare
         Result : Array_Of_Annotations (1 .. Vector.Last_Index);
      begin
         for I in Result'Range loop
            Result (I) := Vector (I);
         end loop;
         return Result;
      end;
   end Scan_Variable_Annotations;

   --------------------
   -- Scan_Variables --
   --------------------

   procedure Scan_Variables
     (Root   : Trees.Tree_Type;
      Vector : in out Binding_Vectors.Vector;
      Map    : in out Binding_Maps.Map)
   is
      procedure Add_Binding;

      -----------------
      -- Add_Binding --
      -----------------

      procedure Add_Binding is
         Binding : Trees.Annotation_Type := Variable_Annotation;
      begin
         Binding.Set_Binding_Index (Vector.Last_Index + 1);
         Vector.Append (Binding);
      end Add_Binding;

   begin
      if Root.Is_Application then
         Add_Binding;
         Root.Set_Annotation (Vector.Last_Element);
         Scan_Variables (Root.Left, Vector, Map);
         Scan_Variables (Root.Right, Vector, Map);
      else
         if Root.Is_Variable then
            declare
               Name : constant String := Root.Variable_Name;
            begin
               if not Map.Contains (Name) then
                  Add_Binding;
                  Map.Insert (Name, Vector.Last_Index);
               end if;
               Root.Set_Annotation
                 (Vector.Element (Map.Element (Name)));
            end;
         end if;
      end if;
   end Scan_Variables;

   -----------
   -- Unify --
   -----------

   function Unify
     (Left_Annotation  : Trees.Annotation_Type;
      Right_Annotation : Trees.Annotation_Type;
      Bindings         : in out Array_Of_Annotations)
      return Trees.Annotation_Type
   is
      use Trees;
      Left    : constant Trees.Annotation_Type :=
                  Dereference (Left_Annotation, Bindings);
      Right   : constant Trees.Annotation_Type :=
                  Dereference (Right_Annotation, Bindings);
   begin
      if Left.Is_Binding then
         Bindings (Left.Binding_Index) := Right;
         return Right;
      elsif Right.Is_Binding then
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
            New_Left  : constant Annotation_Type :=
                          Unify (Left.Left, Right.Left, Bindings);
            New_Right : constant Annotation_Type :=
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

end Leander.Annotation;
