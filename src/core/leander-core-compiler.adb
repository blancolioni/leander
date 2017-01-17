with Ada.Text_IO;

with SK.Machine.Assembler;

with Leander.Types.Bindings;
with Leander.Types.Trees;

with Leander.Errors;

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

   -------------
   -- Compile --
   -------------

   procedure Compile
     (Env     : Leander.Environments.Environment;
      Name    : String;
      Tree    : Leander.Core.Trees.Tree_Type;
      Machine : SK.Machine.SK_Machine)
   is

      procedure Add_Constraint_Abstraction
        (Constraint : Leander.Types.Type_Constraint'Class;
         Variable   : String);

      procedure Compile (T : Leander.Core.Trees.Tree_Type);

      procedure Compile_Algebraic_Case
        (Top : Leander.Core.Trees.Tree_Type);

      procedure Compile_Constructor_Expression
        (Binding : Leander.Types.Bindings.Constructor_Binding'Class);

      function X_Con
        (Index : Leander.Types.Bindings.Constructor_Index_Range)
                  return String
      is ("k" & Integer'Image (-Positive (Index)));

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

      -------------
      -- Compile --
      -------------

      procedure Compile (T : Leander.Core.Trees.Tree_Type) is
      begin
         if T.Is_Empty then
            --  class method
            null;
         elsif T.Is_Application then
            if Is_Lambda (T) then
               Compile (T.Right);
               SK.Machine.Assembler.Lambda
                 (Machine, -T.Left.Get_Node.Name);
            elsif Is_Algebraic_Case (T) then
               Compile_Algebraic_Case (T.Right);
            elsif Is_Primitive_Case (T) then
               null;
            else
               Compile (T.Left);
               if T.Left.Is_Variable then
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
                                      T.Left.Get_Node.Original_Type,
                                      T.Left.Annotation);
                        Name   : constant String :=
                                   Constraint.Show & "-"
                                   & Target.Show
                                   & "-vt";
                     begin
                        SK.Machine.Assembler.Push
                          (Machine, Name);
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
                              Left : constant Leander.Types.Trees.Tree_Type :=
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
                       (T.Left.Get_Node.Original_Type, False,
                        Add_Constraint_Argument'Access);
                  end;
               end if;
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
                  It  : Trees.Tree_Type := Pats (I);
               begin
                  Compile (Exps (I));
                  while It.Is_Application loop
                     SK.Machine.Assembler.Lambda
                       (Machine, It.Right.Variable_Name);
                     It := It.Left;
                  end loop;
               end;
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
            for I in reverse 1 .. Type_Binding.Constructor_Count loop
               SK.Machine.Assembler.Lambda (Machine, X_Con (I));
            end loop;
         end if;
      end Compile_Constructor_Expression;

   begin
      if not Tree.Has_Annotation then
         Ada.Text_IO.Put_Line ("skipping: " & Tree.Show);
      else
         Compile (Tree);
         Leander.Types.Trees.Scan_Constraints
           (Tree.Annotation,
            Include_Cons => False,
            Process      => Add_Constraint_Abstraction'Access);

         Ada.Text_IO.Put_Line
           (Name & " = " & SK.Machine.Show_Stack_Top (Machine));
         SK.Machine.Bind (Machine, Name);
      end if;
   end Compile;

end Leander.Core.Compiler;
