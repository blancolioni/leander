with SK.Machine.Assembler;

with Leander.Types.Bindings;
with Leander.Types.Trees;

with Leander.Errors;
with Leander.Logging;

package body Leander.Core.Compiler is

   function Is_Algebraic_Case
     (Tree : Trees.Tree_Type)
      return Boolean
   is (Tree.Is_Application
       and then Tree.Left.Is_Leaf
       and then Tree.Left.Get_Node.Class = Algebraic_Case);

   function Is_Primitive_Case
     (Tree : Trees.Tree_Type)
      return Boolean
   is (Tree.Is_Application
       and then Tree.Left.Is_Leaf
       and then Tree.Left.Get_Node.Class = Primitive_Case);

   function Is_Lambda
     (Tree : Trees.Tree_Type)
      return Boolean
   is (Tree.Is_Application
       and then Tree.Left.Is_Leaf
       and then Tree.Left.Get_Node.Class = Lambda);

   function Is_Newtype_Constructor
     (Env  : Leander.Environments.Environment;
      Tree : Trees.Tree_Type)
      return Boolean;

   procedure Compile_Tree
     (Env      : Leander.Environments.Environment;
      Tree     : Leander.Core.Trees.Tree_Type;
      Machine  : SK.Machine.SK_Machine;
      Instance : Boolean);

   procedure Compile_Constraint_Abstractions
     (Tree    : Leander.Core.Trees.Tree_Type;
      Machine : SK.Machine.SK_Machine);

   -------------
   -- Compile --
   -------------

   procedure Compile
     (Env     : Leander.Environments.Environment;
      Name    : String;
      Tree    : Leander.Core.Trees.Tree_Type;
      Machine : SK.Machine.SK_Machine)
   is
   begin
      if not Tree.Has_Annotation then
         Leander.Logging.Log ("skipping: " & Tree.Show);
      else
         Compile_Tree (Env, Tree, Machine, False);
         Compile_Constraint_Abstractions (Tree, Machine);

         Leander.Logging.Log
           (Name & " = " & SK.Machine.Show_Stack_Top (Machine));
         SK.Machine.Bind (Machine, Name);
      end if;
   end Compile;

   -------------------------------------
   -- Compile_Constraint_Abstractions --
   -------------------------------------

   procedure Compile_Constraint_Abstractions
     (Tree    : Leander.Core.Trees.Tree_Type;
      Machine : SK.Machine.SK_Machine)
   is

      procedure Add_Constraint_Abstraction
        (Constraint : Leander.Types.Type_Constraint'Class;
         Variable   : String);

      --------------------------------
      -- Add_Constraint_Abstraction --
      --------------------------------

      procedure Add_Constraint_Abstraction
        (Constraint : Leander.Types.Type_Constraint'Class;
         Variable   : String)
      is
         Name : constant String :=
                  Constraint.Show & "-" & Variable & "-vt";
      begin
         SK.Machine.Assembler.Lambda
           (Machine, Name);
      end Add_Constraint_Abstraction;

   begin
      Leander.Types.Trees.Scan_Constraints
        (Tree.Annotation,
         Include_Cons => False,
         Process      => Add_Constraint_Abstraction'Access);
   end Compile_Constraint_Abstractions;

   -----------------------------
   -- Compile_Instance_Method --
   -----------------------------

   procedure Compile_Instance_Method
     (Env           : Leander.Environments.Environment;
      Name          : String;
      Instance_Name : String;
      Tree          : Leander.Core.Trees.Tree_Type;
      Machine       : SK.Machine.SK_Machine)
   is
   begin
      Compile_Tree (Env, Tree, Machine, True);

      SK.Machine.Assembler.Lambda (Machine, Instance_Name);

      Compile_Constraint_Abstractions (Tree, Machine);

      Leander.Logging.Log
        (Name & " = " & SK.Machine.Show_Stack_Top (Machine));
      SK.Machine.Bind (Machine, Name);
   end Compile_Instance_Method;

   ------------------
   -- Compile_Tree --
   ------------------

   procedure Compile_Tree
     (Env      : Leander.Environments.Environment;
      Tree     : Leander.Core.Trees.Tree_Type;
      Machine  : SK.Machine.SK_Machine;
      Instance : Boolean)
   is

      procedure Compile (T : Leander.Core.Trees.Tree_Type);

      procedure Compile_Algebraic_Case
        (Top : Leander.Core.Trees.Tree_Type);

      procedure Compile_Primitive_Case
        (Top : Leander.Core.Trees.Tree_Type);

      procedure Compile_Constructor_Expression
        (Binding : Leander.Types.Bindings.Constructor_Binding'Class);

      function X_Con
        (Index : Leander.Types.Bindings.Constructor_Index_Range)
                  return String
      is ("k" & Integer'Image (-Positive (Index)));

      -------------
      -- Compile --
      -------------

      procedure Compile (T : Leander.Core.Trees.Tree_Type) is
      begin
         if T.Is_Empty then
            --  class method
            null;
         elsif T.Is_Application then
            if Is_Lambda (T.Left)
              and then T.Right.Is_Application
              and then T.Right.Right.Show
                = -T.Left.Left.Get_Node.Name
            then
               Leander.Logging.Log
                 ("replacing: " & T.Show);
               Leander.Logging.Log
                 ("     with: " & T.Right.Left.Show);
               Compile (T.Right.Left);
            elsif Is_Lambda (T) then
               declare
                  X : constant String :=
                        -T.Left.Get_Node.Name;
               begin
                  Compile (T.Right);
                  SK.Machine.Assembler.Lambda (Machine, X);
               end;
            elsif Is_Algebraic_Case (T) then
               Compile_Algebraic_Case (T.Right);
            elsif Is_Primitive_Case (T) then
               Compile_Primitive_Case (T.Right);
            elsif False and then Is_Newtype_Constructor (Env, T) then
               Compile (T.Right);
            else
               Compile (T.Left);
               Compile (T.Right);
               SK.Machine.Apply (Machine);
            end if;
         else
            declare
               Node : constant Core_Node := T.Get_Node;
            begin
               case Primitive_Node_Class (Node.Class) is
                  when Constructor =>
                     Compile_Constructor_Expression
                       (Env.Constructor_Binding (-Node.Name));
                  when Literal =>
                     SK.Machine.Assembler.Push
                       (Machine,
                        Natural'Value
                          (-Node.Name));
                  when Variable =>
                     SK.Machine.Assembler.Push
                       (Machine, -Node.Name);

                     declare
                        procedure Add_Constraint_Argument
                          (Constraint : Leander.Types.Type_Constraint'Class;
                           Variable   : String);

                        function Corresponding_Leaf
                          (Image    : String;
                           Original : Leander.Types.Trees.Tree_Type;
                           Bound    : Leander.Types.Trees.Tree_Type)
                           return Leander.Types.Trees.Tree_Type;

                        -----------------------------
                        -- Add_Constraint_Argument --
                        -----------------------------

                        procedure Add_Constraint_Argument
                          (Constraint : Leander.Types.Type_Constraint'Class;
                           Variable   : String)
                        is
                           Target : constant Types.Trees.Tree_Type :=
                                      Corresponding_Leaf
                                        (Variable,
                                         Node.Original_Type,
                                         T.Annotation);

                           function Name
                             (Constrained_Type : Types.Trees.Tree_Type)
                              return String
                           is (Constraint.Show & "-"
                               & Constrained_Type.Show
                               & "-vt");

                           procedure Constrain
                             (It : Leander.Types.Trees.Tree_Type);

                           ---------------
                           -- Constrain --
                           ---------------

                           procedure Constrain
                             (It : Leander.Types.Trees.Tree_Type)
                           is
                              T : constant Leander.Types.Trees.Tree_Type :=
                                    (if It.Is_Leaf then It else It.Right);
                           begin

                              if not It.Is_Leaf then
                                 Constrain (It.Left);
                              end if;

                              if not Instance or else It.Is_Leaf then
                                 SK.Machine.Assembler.Push
                                   (Machine, Name (T));
                              end if;

                              if not It.Is_Leaf and then not Instance then
                                 SK.Machine.Assembler.Apply (Machine);
                              end if;
                           end Constrain;

                        begin
                           Constrain (Target);
                           SK.Machine.Assembler.Apply (Machine);
                        end Add_Constraint_Argument;

                        ------------------------
                        -- Corresponding_Leaf --
                        ------------------------

                        function Corresponding_Leaf
                          (Image    : String;
                           Original : Leander.Types.Trees.Tree_Type;
                           Bound    : Leander.Types.Trees.Tree_Type)
                           return Leander.Types.Trees.Tree_Type
                        is
                        begin
                           if Original.Is_Leaf then
                              if Original.Show = Image then
                                 return Bound;
                              else
                                 return Leander.Types.Trees.Empty;
                              end if;
                           else
                              declare
                                 Left : constant Types.Trees.Tree_Type :=
                                          Corresponding_Leaf
                                            (Image, Original.Left, Bound.Left);
                              begin
                                 if Left.Is_Empty then
                                    return Corresponding_Leaf
                                      (Image, Original.Right, Bound.Right);
                                 else
                                    return Left;
                                 end if;
                              end;
                           end if;
                        end Corresponding_Leaf;

                     begin
                        Leander.Types.Trees.Scan_Constraints
                          (T.Get_Node.Original_Type, False,
                           Add_Constraint_Argument'Access);
                     end;
               end case;
            end;
         end if;
      end Compile;

      ----------------------------
      -- Compile_Algebraic_Case --
      ----------------------------

      procedure Compile_Algebraic_Case
        (Top : Leander.Core.Trees.Tree_Type)
      is
         It : Leander.Core.Trees.Tree_Type := Top.Right;
         Expr : constant Leander.Core.Trees.Tree_Type := Top.Left;
         Expr_Type     : constant Leander.Types.Trees.Tree_Type :=
                           Expr.Annotation;
         Expr_Tycon    : constant Leander.Types.Bindings.Type_Binding'Class :=
                           Env.Type_Constructor_Binding
                             (Expr_Type.Head.Constructor_Name);
         Type_Head     : constant Leander.Types.Type_Node :=
                           Expr_Type.Head;
         Type_Binding  : constant Leander.Types.Bindings.Type_Binding'Class :=
                           Env.Type_Constructor_Binding
                             (Type_Head.Constructor_Name);
         Pats, Exps    : array (1 .. Type_Binding.Constructor_Count)
           of Trees.Tree_Type := (others => Trees.Empty);
         Default_Expr       : Trees.Tree_Type := Trees.Empty;
         Default_Variable   : Trees.Tree_Type;
      begin
         Compile (Expr);
         while not It.Is_Empty loop
            declare
               Alt : constant Trees.Tree_Type := It.Left;
               Pat : constant Trees.Tree_Type := Alt.Left;
               Exp : constant Trees.Tree_Type := Alt.Right;
            begin
               It := It.Right;
               if not Default_Expr.Is_Empty then
                  Leander.Errors.Error
                    (Pat.Get_Node.Source,
                     "pattern never checked");
               end if;

               if Pat.Is_Leaf and then Pat.Get_Node.Is_Variable then
                  if Default_Expr.Is_Empty then
                     Default_Expr := Exp;
                     Default_Variable := Pat;
                  end if;
               else
                  declare
                     use Leander.Types.Bindings;
                     Index : constant Constructor_Index_Range :=
                               Env.Constructor_Binding
                                 (Pat.Head.Constructor_Name)
                                 .Constructor_Index;
                  begin
                     Pats (Index) := Pat;
                     Exps (Index) := Exp;
                  end;
               end if;
            end;
         end loop;

         for I in Pats'Range loop
            if Pats (I).Is_Empty then
               if Default_Expr.Is_Empty then
                  Leander.Errors.Error
                    (Top.Left.Get_Node.Source,
                     "missing pattern match for "
                     & Expr_Tycon.Constructor_Name (I));
               else
                  Pats (I) := Default_Variable;
                  Exps (I) := Default_Expr;
               end if;
            end if;
         end loop;

         for I in Pats'Range loop
            if Pats (I).Is_Variable then
               declare
                  use Leander.Types.Bindings;
                  Con : constant Constructor_Binding'Class :=
                          Env.Constructor_Binding
                            (Expr_Tycon.Constructor_Name (I));
                  function Arg_Name
                    (Index : Positive)
                     return String
                  is ("x" & Integer'Image (-Index));
                  Arity : constant Natural :=
                            Con.Constructor_Type.Arity;
               begin
                  Compile (Default_Expr);
                  SK.Machine.Assembler.Lambda
                    (Machine, Pats (I).Variable_Name);
                  SK.Machine.Assembler.Push
                    (Machine, X_Con (Con.Constructor_Index));
                  for I in 1 .. Arity loop
                     SK.Machine.Assembler.Push
                       (Machine, Arg_Name (I));
                     SK.Machine.Assembler.Apply (Machine);
                  end loop;
                  for I in reverse 1 .. Expr_Tycon.Constructor_Count loop
                     SK.Machine.Assembler.Lambda
                       (Machine, X_Con (I));
                  end loop;
                  SK.Machine.Assembler.Apply (Machine);
                  for I in reverse 1 .. Arity loop
                     SK.Machine.Assembler.Lambda
                       (Machine, Arg_Name (I));
                  end loop;

               end;
            else
               declare
                  use Leander.Types.Bindings;
                  Pat_Args  : constant Trees.Array_Of_Trees :=
                                Pats (I).Arguments;
                  First_Arg : Positive := Pat_Args'First;
                  E         : Trees.Tree_Type := Exps (I);
               begin
                  if False then
                     Leander.Logging.Log
                       ("compiling case expr: "
                        & Pats (I).Show & " -> " & E.Show);

                     for I in Pat_Args'Range loop
                        if E.Is_Application
                          and then E.Right.Is_Variable
                          and then E.Right.Variable_Name
                            = Pat_Args (I).Variable_Name
                        then
                           Leander.Logging.Log
                             ("dropping because \"
                              & Pat_Args (I).Variable_Name & ".E "
                              & E.Right.Variable_Name
                              & " ==> E");
                           E := E.Left;
                        else
                           First_Arg := I;
                           exit;
                        end if;
                     end loop;
                  end if;

                  Compile (E);

                  for I in First_Arg .. Pat_Args'Last loop
                     SK.Machine.Assembler.Lambda
                       (Machine, Pat_Args (I).Variable_Name);
                  end loop;

               end;
--                 Leander.Logging.Log
--                   ("final expression: "
--                    & SK.Machine.Show_Stack_Top (Machine));
            end if;
            SK.Machine.Assembler.Apply (Machine);
         end loop;
      end Compile_Algebraic_Case;

      -------------------------
      -- Compile_Constructor --
      -------------------------

      procedure Compile_Constructor_Expression
        (Binding : Leander.Types.Bindings.Constructor_Binding'Class)
      is
         use Leander.Types.Bindings;
         Con_Type     : constant Leander.Types.Trees.Tree_Type :=
                          Binding.Constructor_Type;
         Con_Arity    : constant Natural :=  Binding.Constructor_Arity;
         Con_Index    : constant Types.Bindings.Constructor_Count_Range :=
                          Binding.Constructor_Index;
         Type_Head    : constant Leander.Types.Type_Node :=
                          Con_Type.Last_Map.Head;
         Type_Binding : constant Types.Bindings.Type_Binding'Class :=
                          Env.Type_Constructor_Binding
                            (Type_Head.Constructor_Name);
      begin
         if Type_Binding.Is_Primitive then
            SK.Machine.Assembler.Push
              (Machine, Positive (Con_Index) - 1);
         else
            SK.Machine.Assembler.Push
              (Machine, X_Con (Con_Index));
            for I in 1 .. Con_Arity loop
               SK.Machine.Assembler.Push
                 (Machine, "c" & Integer'Image (-I));
               SK.Machine.Assembler.Apply (Machine);
            end loop;

            for I in reverse 1 .. Type_Binding.Constructor_Count loop
               SK.Machine.Assembler.Lambda (Machine, X_Con (I));
            end loop;

            for I in 1 .. Con_Arity loop
               SK.Machine.Assembler.Lambda
                 (Machine, "c" & Integer'Image (-I));
            end loop;
         end if;
      end Compile_Constructor_Expression;

      ----------------------------
      -- Compile_Primitive_Case --
      ----------------------------

      procedure Compile_Primitive_Case
        (Top : Leander.Core.Trees.Tree_Type)
      is
         X : constant String := "-p-";

         procedure Compile_Alts (Alt_It : Leander.Core.Trees.Tree_Type);

         ------------------
         -- Compile_Alts --
         ------------------

         procedure Compile_Alts (Alt_It : Leander.Core.Trees.Tree_Type) is
         begin
            if Alt_It.Is_Empty then
               SK.Machine.Assembler.Push (Machine, SK.Fail_Object);
            else
               declare
                  Alt : constant Trees.Tree_Type := Alt_It.Left;
                  Pat : constant Trees.Tree_Type := Alt.Left;
                  Exp : constant Trees.Tree_Type := Alt.Right;
               begin
                  pragma Assert (Pat.Is_Leaf);
                  if Pat.Is_Variable then
                     Compile (Exp);
                     SK.Machine.Assembler.Lambda (Machine, Pat.Show);
                     SK.Machine.Assembler.Push (Machine, X);
                     SK.Machine.Apply (Machine);
                  else
                     SK.Machine.Assembler.Push (Machine, SK.Select_Object (2));
                     SK.Machine.Assembler.Push (Machine, "neq?");
                     SK.Machine.Assembler.Push (Machine, X);
                     SK.Machine.Assembler.Apply (Machine);
                     SK.Machine.Assembler.Push
                       (Machine, Natural'Value (Pat.Show));
                     SK.Machine.Assembler.Apply (Machine);
                     SK.Machine.Assembler.Apply (Machine);
                     Compile (Exp);
                     SK.Machine.Assembler.Apply (Machine);
                     Compile_Alts (Alt_It.Right);
                     SK.Machine.Assembler.Apply (Machine);
                  end if;
               end;
            end if;
         end Compile_Alts;

      begin
         Compile_Alts (Top.Right);
         SK.Machine.Assembler.Lambda (Machine, X);
         Compile (Top.Left);
         SK.Machine.Apply (Machine);
      end Compile_Primitive_Case;

   begin
      Compile (Tree);
   end Compile_Tree;

   ----------------------------
   -- Is_Newtype_Constructor --
   ----------------------------

   function Is_Newtype_Constructor
     (Env  : Leander.Environments.Environment;
      Tree : Trees.Tree_Type)
      return Boolean
   is
   begin
      if Tree.Left.Is_Leaf
        and then Tree.Left.Is_Constructor
      then
         declare
            use Leander.Types.Bindings;
            Binding : constant Constructor_Binding'Class :=
                        Env.Constructor_Binding (-Tree.Left.Get_Node.Name);

            Con_Type     : constant Leander.Types.Trees.Tree_Type :=
                             Binding.Constructor_Type;
            Type_Head    : constant Leander.Types.Type_Node :=
                             Con_Type.Last_Map.Head;
            Type_Binding : constant Types.Bindings.Type_Binding'Class :=
                             Env.Type_Constructor_Binding
                               (Type_Head.Constructor_Name);
         begin
            return Type_Binding.Constructor_Count = 1
              and then Binding.Constructor_Arity = 1;
         end;
      else
         return False;
      end if;
   end Is_Newtype_Constructor;

end Leander.Core.Compiler;
