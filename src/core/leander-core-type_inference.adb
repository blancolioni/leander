with Ada.Text_IO;

with Ada.Strings.Fixed.Hash;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Sets;

with Leander.Core.Trees;

with Leander.Errors;

package body Leander.Core.Type_Inference is

   type Array_Of_Annotations is
     array (Positive range <>) of Leander.Types.Trees.Tree_Type;

   package Binding_Vectors is
     new Ada.Containers.Vectors
       (Positive, Leander.Types.Trees.Tree_Type, Leander.Types.Trees."=");

   package Binding_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Positive,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=");

   package Set_Of_Names is
     new Ada.Containers.Indefinite_Hashed_Sets
       (Element_Type        => String,
        Hash                => Ada.Strings.Fixed.Hash,
        Equivalent_Elements => "=");

   package Dependency_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Set_Of_Names.Set,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=",
        "="             => Set_Of_Names."=");

   package List_Of_Local_Bindings is
     new Ada.Containers.Doubly_Linked_Lists
       (Leander.Types.Trees.Tree_Type, Leander.Types.Trees."=");

   package Local_Binding_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => List_Of_Local_Bindings.List,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=",
        "="             => List_Of_Local_Bindings."=");

   procedure Annotate
     (Tree     : Trees.Tree_Type;
      Bindings : Leander.Core.Bindings.Binding_List;
      Types    : Leander.Types.Bindings.Type_Binding_List;
      Cons     : Leander.Types.Bindings.Constructor_Binding_List;
      Env      : Leander.Environments.Environment);

   function Scan_Variable_Annotations
     (Tree : Trees.Tree_Type;
      Bindings : Leander.Core.Bindings.Binding_List;
      Types    : Leander.Types.Bindings.Type_Binding_List;
      Cons     : Leander.Types.Bindings.Constructor_Binding_List;
      Env      : Leander.Environments.Environment)
      return Array_Of_Annotations;

   function Unify
     (Left_Annotation  : Leander.Types.Trees.Tree_Type;
      Right_Annotation : Leander.Types.Trees.Tree_Type;
      Bindings         : in out Array_Of_Annotations)
      return Leander.Types.Trees.Tree_Type;

   function Bind
     (Annotation    : Leander.Types.Trees.Tree_Type;
      Bindings      : in out Array_Of_Annotations;
      Next_Variable : in out Natural)
      return Leander.Types.Trees.Tree_Type;

   function Dereference
     (Annotation : Leander.Types.Trees.Tree_Type;
      Bindings   : Array_Of_Annotations)
      return Leander.Types.Trees.Tree_Type;

   --------------
   -- Annotate --
   --------------

   procedure Annotate
     (Tree     : Trees.Tree_Type;
      Bindings : Leander.Core.Bindings.Binding_List;
      Types    : Leander.Types.Bindings.Type_Binding_List;
      Cons     : Leander.Types.Bindings.Constructor_Binding_List;
      Env      : Leander.Environments.Environment)
   is
      Vars : Array_Of_Annotations :=
               Scan_Variable_Annotations
                 (Tree, Bindings, Types, Cons, Env);

      Next_Variable : Natural := 0;

      procedure Unify
        (Tree       : Trees.Tree_Type);

      procedure Unify_Algebraic_Case
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
--           Ada.Text_IO.Put_Line
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
--           Ada.Text_IO.Put_Line
--             ("enter unify: " & Tree.Show_With_Annotations);
         if Tree.Is_Application then
            if Tree.Left.Is_Leaf
              and then Tree.Left.Get_Node.Class = Algebraic_Case
            then
               Unify_Algebraic_Case (Tree);
            else
               Unify (Tree.Right);
               if Tree.Left.Is_Leaf
                 and then Tree.Left.Get_Node.Class = Lambda
               then
                  Unify (Tree.Left);
                  declare
                     Tree_A : constant Leander.Types.Trees.Tree_Type :=
                                Map_Operator.Apply
                                  (Tree.Left.Annotation)
                                  .Apply (Tree.Right.Annotation);
                  begin
                     Tree.Set_Annotation
                       (Unify (Tree_A, Tree.Annotation, Vars));
                  end;
               else
                  declare
                     Left_A  : constant Leander.Types.Trees.Tree_Type :=
                                 Map_Operator.Apply
                                   (Tree.Right.Annotation)
                                   .Apply (Tree.Annotation);
                     Unified : constant Leander.Types.Trees.Tree_Type :=
                                 Unify (Left_A, Tree.Left.Annotation, Vars);
                  begin
                     if Unified.Is_Empty then
                        Leander.Errors.Error
                          (Tree.Head.Source,
                           "type error");
                        Leander.Errors.Error
                          (Tree.Left.Get_Node.Source,
                           Tree.Left.Show & " :: "
                           & Tree.Left.Annotation.Show);
                        Leander.Errors.Error
                          (Tree.Left.Get_Node.Source,
                           Tree.Left.Show & " :: "
                           & Left_A.Show);
                     end if;
                     Tree.Left.Set_Annotation
                       (Unified);
                     Unify (Tree.Left);
                  end;
               end if;
            end if;
         end if;
--           Ada.Text_IO.Put_Line
--             ("exit unify: " & Tree.Show_With_Annotations);
      end Unify;

      --------------------------
      -- Unify_Algebraic_Case --
      --------------------------

      procedure Unify_Algebraic_Case
        (Tree       : Trees.Tree_Type)
      is
         Case_Expr   : constant Trees.Tree_Type := Tree.Right.Left;
         Result_Type : constant Leander.Types.Trees.Tree_Type :=
                         Tree.Annotation;
         It          : Trees.Tree_Type := Tree.Right.Right;
      begin
         Unify (Case_Expr);
         while not It.Is_Empty loop
            declare
               Alt : constant Trees.Tree_Type := It.Left;
               Pat : constant Trees.Tree_Type := Alt.Left;
               Exp : constant Trees.Tree_Type := Alt.Right;
            begin
               It := It.Right;
               Unify (Pat);
               Pat.Set_Annotation
                 (Unify (Pat.Annotation, Case_Expr.Annotation, Vars));
               Unify (Exp);
               Exp.Set_Annotation
                 (Unify (Result_Type, Exp.Annotation, Vars));
            end;
         end loop;
      end Unify_Algebraic_Case;

   begin
      Unify (Tree);
      Bind (Tree);
   end Annotate;

   ----------
   -- Bind --
   ----------

   function Bind
     (Annotation    : Leander.Types.Trees.Tree_Type;
      Bindings      : in out Array_Of_Annotations;
      Next_Variable : in out Natural)
      return Leander.Types.Trees.Tree_Type
   is
      use Leander.Types.Trees;
      A : constant Tree_Type :=
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
     (Annotation : Leander.Types.Trees.Tree_Type;
      Bindings   : Array_Of_Annotations)
      return Leander.Types.Trees.Tree_Type
   is
   begin
      if Annotation.Is_Application then
         return Annotation;
      elsif Annotation.Is_Binding then
         declare
            use type Leander.Types.Trees.Tree_Type;
            Value : constant Leander.Types.Trees.Tree_Type :=
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
   -- Infer_Types --
   -----------------

   procedure Infer_Types
     (Bindings : Leander.Core.Bindings.Binding_List;
      Types    : Leander.Types.Bindings.Type_Binding_List;
      Cons     : Leander.Types.Bindings.Constructor_Binding_List;
      Env      : Leander.Environments.Environment)
   is

      Dependencies : Dependency_Maps.Map;

      procedure Add_Binding
        (Name : String;
         Tree : Leander.Core.Trees.Tree_Type);

      procedure Scan_Dependencies
        (Tree           : Leander.Core.Trees.Tree_Type;
         Local_Bindings : Set_Of_Names.Set;
         Set            : in out Set_Of_Names.Set);

      -----------------
      -- Add_Binding --
      -----------------

      procedure Add_Binding
        (Name : String;
         Tree : Leander.Core.Trees.Tree_Type)
      is
         Local : Set_Of_Names.Set;
         Deps  : Set_Of_Names.Set;
      begin
         Local.Insert (Name);
         Scan_Dependencies (Tree, Local, Deps);
         Dependencies.Insert (Name, Deps);
      end Add_Binding;

      -----------------------
      -- Scan_Dependencies --
      -----------------------

      procedure Scan_Dependencies
        (Tree           : Leander.Core.Trees.Tree_Type;
         Local_Bindings : Set_Of_Names.Set;
         Set            : in out Set_Of_Names.Set)
      is
      begin
         if Tree.Is_Application then
            if Tree.Left.Is_Leaf
              and then Tree.Left.Get_Node.Class = Lambda
            then
               declare
                  Name : constant String := -Tree.Left.Get_Node.Name;
                  New_Bindings : Set_Of_Names.Set := Local_Bindings;
               begin
                  if not New_Bindings.Contains (Name) then
                     New_Bindings.Insert (Name);
                  end if;
                  Scan_Dependencies (Tree.Right, New_Bindings, Set);
               end;
            else
               Scan_Dependencies (Tree.Left, Local_Bindings, Set);
               Scan_Dependencies (Tree.Right, Local_Bindings, Set);
            end if;
         elsif Tree.Is_Variable then
            if not Local_Bindings.Contains (Tree.Variable_Name)
              and then not Set.Contains (Tree.Variable_Name)
              and then Bindings.Has_Binding (Tree.Variable_Name)
            then
               Set.Insert (Tree.Variable_Name);
            end if;
         end if;
      end Scan_Dependencies;

   begin
      Bindings.Scan (Add_Binding'Access);

      for Position in Dependencies.Iterate loop
         Ada.Text_IO.Put_Line ("annotating: "
                                 & Dependency_Maps.Key (Position));
         if Dependency_Maps.Element (Position).Is_Empty then
            Annotate
              (Tree     => Bindings.Binding (Dependency_Maps.Key (Position)),
               Bindings => Bindings,
               Types    => Types,
               Cons     => Cons,
               Env      => Env);
            Ada.Text_IO.Put_Line
              (Dependency_Maps.Key (Position)
               & " :: "
               & Bindings.Binding
                 (Dependency_Maps.Key (Position)).Annotation.Show);

         end if;
      end loop;

      for Position in Dependencies.Iterate loop
         declare
            Ready : Boolean := True;
            Name  : constant String := Dependency_Maps.Key (Position);
         begin
            if not Bindings.Binding (Name).Has_Annotation then
               for Dep_Name of Dependency_Maps.Element (Position) loop
                  if not Bindings.Binding (Dep_Name).Has_Annotation then
                     Ready := False;
                     exit;
                  end if;
               end loop;
               if Ready then
--                    Ada.Text_IO.Put_Line
--                      ("Annotating: " & Name);
                  Annotate
                    (Tree     => Bindings.Binding (Name),
                     Bindings => Bindings,
                     Types    => Types,
                     Cons     => Cons,
                     Env      => Env);
                  Ada.Text_IO.Put_Line
                    (Name
                     & " :: "
                     & Bindings.Binding (Name).Annotation.Show);
               end if;
            end if;
         end;
      end loop;
   end Infer_Types;

   -------------------------------
   -- Scan_Variable_Annotations --
   -------------------------------

   function Scan_Variable_Annotations
     (Tree     : Trees.Tree_Type;
      Bindings : Leander.Core.Bindings.Binding_List;
      Types    : Leander.Types.Bindings.Type_Binding_List;
      Cons     : Leander.Types.Bindings.Constructor_Binding_List;
      Env      : Leander.Environments.Environment)
      return Array_Of_Annotations
   is
      pragma Unreferenced (Types);
      Vector : Binding_Vectors.Vector;
      Map    : Binding_Maps.Map;
      Bound  : Local_Binding_Maps.Map;

      procedure Add_Binding;

      procedure Scan_Variables
        (Root   : Trees.Tree_Type);

      procedure Scan_Algebraic_Alts
        (Root : Trees.Tree_Type);

      function Unbind
        (Bound_Type : Leander.Types.Trees.Tree_Type)
         return Leander.Types.Trees.Tree_Type;

      procedure Enter_Local_Binding
        (Name : String;
         Tree : Trees.Tree_Type);

      procedure Leave_Local_Binding
        (Name : String);

      -----------------
      -- Add_Binding --
      -----------------

      procedure Add_Binding is
         Binding : constant Leander.Types.Trees.Tree_Type :=
                     Leander.Types.Trees.Leaf
                       (Leander.Types.Binding
                          (Vector.Last_Index + 1));
      begin
         Vector.Append (Binding);
      end Add_Binding;

      -------------------------
      -- Enter_Local_Binding --
      -------------------------

      procedure Enter_Local_Binding
        (Name : String;
         Tree : Trees.Tree_Type)
      is
      begin
         if not Bound.Contains (Name) then
            Bound.Insert (Name, List_Of_Local_Bindings.Empty_List);
         end if;

         Add_Binding;
         Bound (Name).Append (Vector.Last_Element);
         Tree.Set_Annotation (Vector.Last_Element);
      end Enter_Local_Binding;

      -------------------------
      -- Leave_Local_Binding --
      -------------------------

      procedure Leave_Local_Binding
        (Name : String)
      is
      begin
         Bound (Name).Delete_Last;
      end Leave_Local_Binding;

      -------------------------
      -- Scan_Algebraic_Alts --
      -------------------------

      procedure Scan_Algebraic_Alts
        (Root : Trees.Tree_Type)
      is
         It : Trees.Tree_Type := Root;
      begin
         while not It.Is_Empty loop
            declare
               Alt : constant Trees.Tree_Type := It.Left;
               Pat : constant Trees.Tree_Type := Alt.Left;
               Exp : constant Trees.Tree_Type := Alt.Right;
            begin
               It := It.Right;

               if Pat.Is_Variable then
                  Enter_Local_Binding (Pat.Variable_Name, Pat);
               else
                  declare
                     Pat_It : Trees.Tree_Type := Pat;
                  begin
                     while Pat_It.Is_Application loop
                        Ada.Text_IO.Put_Line
                          ("pat: " & Pat_It.Show);
                        Add_Binding;
                        Pat_It.Set_Annotation (Vector.Last_Element);
                        Enter_Local_Binding (Pat_It.Right.Variable_Name,
                                             Pat_It.Right);
                        Pat_It := Pat_It.Left;
                     end loop;
                  end;
               end if;
               Scan_Variables (Exp);
               if Pat.Is_Variable then
                  Leave_Local_Binding (Pat.Variable_Name);
               else
                  declare
                     Pat_It : Trees.Tree_Type := Pat;
                  begin
                     while Pat_It.Is_Application loop
                        Leave_Local_Binding (Pat_It.Right.Variable_Name);
                        Pat_It := Pat_It.Left;
                     end loop;
                     pragma Assert (Pat_It.Is_Constructor);
                     declare
                        Name : constant String := Pat_It.Constructor_Name;
                     begin
                        if Cons.Has_Binding (Name) then
                           Pat_It.Set_Annotation
                             (Unbind (Cons.Binding (Name).Constructor_Type));
                        elsif Env.Has_Expression_Binding (Name) then
                           Pat_It.Set_Annotation
                             (Unbind
                                (Env.Constructor_Binding (Name)
                                 .Constructor_Type));
                        else
                           Leander.Errors.Error
                             (Root.Get_Node.Source,
                              "undefined: " & Name);
                           Add_Binding;
                           Pat_It.Set_Annotation (Vector.Last_Element);
                        end if;
                     end;

                  end;
               end if;
            end;
         end loop;
      end Scan_Algebraic_Alts;

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
            if Bound.Contains (Name)
              and then not Bound (Name).Is_Empty
            then
               Root.Set_Annotation
                 (Bound (Name).Last_Element);
            else
               if not Map.Contains (Name) then
                  Add_Binding;
                  Map.Insert (Name, Vector.Last_Index);
               end if;
               Root.Set_Annotation
                 (Vector.Element (Map.Element (Name)));
            end if;
         end Set_Named_Binding;

      begin
         if Root.Is_Application then
            Add_Binding;
            Root.Set_Annotation (Vector.Last_Element);
            Scan_Variables (Root.Left);
            if Root.Left.Is_Leaf
              and then Root.Left.Get_Node.Class = Algebraic_Case
            then
               Scan_Variables (Root.Right.Left);
               Scan_Algebraic_Alts (Root.Right.Right);
            else
               if Root.Left.Is_Leaf
                 and then Root.Left.Get_Node.Class = Lambda
               then
                  Enter_Local_Binding
                    (-Root.Left.Get_Node.Name,
                     Root.Left);
               end if;
               Scan_Variables (Root.Right);
               if Root.Left.Is_Leaf
                 and then Root.Left.Get_Node.Class = Lambda
               then
                  Leave_Local_Binding
                    (-Root.Left.Get_Node.Name);
               end if;
            end if;
         elsif Root.Is_Variable then
            declare
               Name : constant String := Root.Variable_Name;
            begin
               if not Bound.Contains (Name)
                 or else Bound (Name).Is_Empty
               then
                  if Bindings.Has_Binding (Name) then
                     declare
                        Tree : constant Trees.Tree_Type :=
                                 Bindings.Binding (Name);
                     begin
                        Root.Set_Annotation
                          (Unbind (Tree.Annotation));
                     end;
                  elsif Env.Has_Expression_Binding (Name) then
                     Root.Set_Annotation
                       (Unbind
                          (Env.Expression_Binding (Name).Annotation));
                  else
                     Leander.Errors.Error
                       (Root.Get_Node.Source,
                        "undefined: " & Name);
                     Add_Binding;
                     Root.Set_Annotation (Vector.Last_Element);
                  end if;
               else
                  Set_Named_Binding (Name);
               end if;
            end;
         elsif Root.Is_Constructor then
            declare
               Name : constant String := Root.Constructor_Name;
            begin
               if Cons.Has_Binding (Name) then
                  Root.Set_Annotation
                    (Unbind (Cons.Binding (Name).Constructor_Type));
               elsif Env.Has_Expression_Binding (Name) then
                  Root.Set_Annotation
                    (Unbind (Env.Constructor_Binding (Name).Constructor_Type));
               else
                  Leander.Errors.Error
                    (Root.Get_Node.Source,
                     "undefined: " & Name);
                  Add_Binding;
                  Root.Set_Annotation (Vector.Last_Element);
               end if;
            end;
         elsif Root.Get_Node.Class = Lambda then
            Set_Named_Binding (-Root.Get_Node.Name);
         end if;
      end Scan_Variables;

      ------------
      -- Unbind --
      ------------

      function Unbind
        (Bound_Type : Leander.Types.Trees.Tree_Type)
         return Leander.Types.Trees.Tree_Type
      is
         Local_Map : Binding_Maps.Map;

         function Go (T : Leander.Types.Trees.Tree_Type)
                      return Leander.Types.Trees.Tree_Type;

         --------
         -- Go --
         --------

         function Go (T : Leander.Types.Trees.Tree_Type)
                      return Leander.Types.Trees.Tree_Type
         is
         begin
            if T.Is_Application then
               return Go (T.Left).Apply (Go (T.Right));
            elsif T.Is_Variable then
               if Local_Map.Contains (T.Variable_Name) then
                  return Vector (Local_Map.Element (T.Variable_Name));
               else
                  Add_Binding;
                  Local_Map.Insert (T.Variable_Name, Vector.Last_Index);
                  return Vector.Last_Element;
               end if;
            else
               return T;
            end if;
         end Go;

         T : constant Leander.Types.Trees.Tree_Type := Go (Bound_Type);
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
     (Left_Annotation  : Leander.Types.Trees.Tree_Type;
      Right_Annotation : Leander.Types.Trees.Tree_Type;
      Bindings         : in out Array_Of_Annotations)
      return Leander.Types.Trees.Tree_Type
   is
      use Leander.Types.Trees;
      Left    : constant Leander.Types.Trees.Tree_Type :=
                  Dereference (Left_Annotation, Bindings);
      Right   : constant Leander.Types.Trees.Tree_Type :=
                  Dereference (Right_Annotation, Bindings);
   begin
--        Ada.Text_IO.Put_Line
--          ("Unify: " & Left.Show & " with " & Right.Show);
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

end Leander.Core.Type_Inference;
